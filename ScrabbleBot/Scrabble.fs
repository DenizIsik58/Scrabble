namespace YourClientName

open System.Runtime.InteropServices
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint


// The RegEx module is only used to parse human input. It is not used for the final product.
          
module RegEx =
    
    open System.Text.RegularExpressions

     
      
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    type coord = int * int
    type tile = char * int
    type piece = uint32 * tile
    type move = (coord * piece) list
    
    type word = (coord * (uint32 * (char * int))) list
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        piecesOnBoard : Map<coord, char>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h = {board = b; piecesOnBoard = Map.empty; dict = d;  playerNumber = pn; hand = h }

    let board st         = st.board
    let dict st          = st.dict
    
    let piecesOnBoard st = st.piecesOnBoard
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


module Scrabble =
    open System.Threading
    open Parser
    open State
    
    let getNewHand hand newPieces = newPieces |> List.fold (fun _ (tileId, amount) ->
        let timesOccured = Map.tryFind tileId hand |> Option.defaultValue 0u
        if (timesOccured >= 1u) then MultiSet.add tileId (timesOccured + amount) hand else MultiSet.add tileId amount hand) hand 

    let bestFoundMove (firstMove: word) (secondMove: word) : word =
         let longest = if (List.length firstMove > List.length secondMove) then firstMove else secondMove
         if List.length longest > 1 then longest else []

            
    let getCoords isHorizontal (x, y) : coord = if isHorizontal then (x + 1, y) else (x, y + 1)
    
    let rec specifiedStartingCoordinates coord isHorizontal board =
        let x, y = coord
        let changedCoords = if isHorizontal then (x - 1, y) else (x, y - 1)
        match Map.tryFind changedCoords board with
        | None -> coord
        | Some _ -> specifiedStartingCoordinates changedCoords isHorizontal board
        
  
    let isLegalMove (isHorizontal: bool) (coord: coord) (piecesOnBoard: Map<coord, char>) =
        let (xCoord, yCoord) = coord
        
        let moveUp = if isHorizontal then (xCoord, yCoord - 1) else (xCoord - 1, yCoord)
        let moveDown = if isHorizontal then (xCoord, yCoord + 1) else (xCoord + 1, yCoord)
        (Map.containsKey moveUp piecesOnBoard || Map.containsKey moveDown piecesOnBoard) |> not
        
    let rec findMove (isHorizontal: bool) (coordinates: coord) (st:state) (pieces: Map<uint32, Set<char * int>>) (piecesLaidOnTable: word) (bestPossibleWord: word) : word =
                  let xCoords,yCoords = coordinates
                  let changedCoords = getCoords isHorizontal (xCoords, yCoords)
                  match Map.tryFind coordinates st.piecesOnBoard with
                  | Some(ch) -> match Dictionary.step ch st.dict with
                                                  | Some (wordEnded, trie) ->
                                                                                let bestPossibleWord = if wordEnded && not (Map.containsKey changedCoords st.piecesOnBoard) then bestFoundMove bestPossibleWord piecesLaidOnTable else bestPossibleWord
                                                                                let stateUpdated = {
                                                                                    st with
                                                                                    dict = trie
                                                                                }
                                                                                findMove isHorizontal changedCoords stateUpdated pieces piecesLaidOnTable bestPossibleWord
                                                  | None -> bestPossibleWord
                  | None ->
                        MultiSet.fold (fun accumulator key _ ->
                                bestFoundMove (Set.fold (fun accumulator (c, p) ->
                                    match Dictionary.step c st.dict with
                                    | None -> accumulator
                                    | Some(wordFound, trie) -> 
                                        if isLegalMove isHorizontal coordinates st.piecesOnBoard then
                                            let stateUpdated =
                                                { st with 
                                                      dict = trie
                                                      hand = MultiSet.removeSingle key st.hand
                                                      piecesOnBoard = Map.add coordinates c st.piecesOnBoard
                                                }
                                            let piecesDownOnBoard = ((coordinates, (key, (c, p))) :: piecesLaidOnTable)
                                            let bestPossibleWord =
                                                match Map.tryFind changedCoords st.piecesOnBoard with
                                                | None -> if wordFound && not (Map.containsKey changedCoords st.piecesOnBoard) then bestFoundMove bestPossibleWord piecesDownOnBoard else accumulator
                                                | Some (_) -> accumulator
                                            findMove isHorizontal changedCoords stateUpdated pieces piecesDownOnBoard bestPossibleWord
                                        else
                                            accumulator
                                    ) bestPossibleWord (Map.find key pieces)) accumulator
                                ) bestPossibleWord st.hand                                                                           
                                         
    let botMove (piecesOnBoard:Map<coord, char>) (st: state) (pieces: Map<uint32, Set<char * int>>) : word =        
        
        if Map.isEmpty piecesOnBoard then
            bestFoundMove (findMove true (0, 0) st pieces [] []) (findMove false (0,0) st pieces [] [])    
        else
            let rec aux coordinates : word =
                match coordinates with
                | [] -> []
                | (x, y)::xs -> findMove true (specifiedStartingCoordinates (x, y) true st.piecesOnBoard)  st pieces [] []
                                |> bestFoundMove (findMove false (specifiedStartingCoordinates (x, y) false st.piecesOnBoard)  st pieces [] [])
                                |> bestFoundMove (aux xs)
            aux (Seq.toList (Map.keys st.piecesOnBoard))
           
    let playGame cstream pieces (st : State.state) =

        
        let stateUpdate:State.state = {st with
                               piecesOnBoard = st.piecesOnBoard
                               playerNumber = st.playerNumber
        }
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            // remove the force print when you move on from manual input (or when you have learnt the format)
            let move = botMove st.piecesOnBoard st pieces
            // TODO 4. Make a new file - a bot which automates moves. Give it pieces, st.piecesOnBoard, st.hand and return a move.
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            if List.length move = 0 then send cstream (SMPass) else send cstream (SMPlay move)

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let moves = ms |> List.fold (fun acc (coord, (pId, (ch, pv))) -> pId :: acc) [] // moves
                let removed = moves |> List.fold (fun acc tile -> MultiSet.removeSingle tile acc ) st.hand
                let newHand = newPieces |> List.fold (fun acc (pId, amount) -> MultiSet.add pId amount acc ) removed
                
                let piecesAndCoords = ms |> List.fold (fun acc (coord, (pId, (ch, pv))) -> (coord, (pId, (ch, pv))):: acc) [] // piecesAndCoords
                let stateUpdated:state = {
                               st with
                               hand = newHand
                               piecesOnBoard = piecesAndCoords |> List.fold (fun acc (coord, (pId, (ch, pv))) -> Map.add coord (ch) acc)  st.piecesOnBoard
                               playerNumber = st.playerNumber
                }
                printfn "My H"
                
                // TODO: 1. extract id (uint32) from ms, and remove it from the hand
                // TODO: 2. Add new pieces (id of the tile, amount of times id has been drawn) to the current hand by adding the ids to the hand
                // TODO: 3. Add all coordinates and pieces from ms to st.piecesOnBoard 
                
                 // This state needs to be updated
                aux stateUpdated
            | RCM (CMPlayed (pid, ms, points)) ->
                let piecesAndCoords = ms |> List.fold (fun acc (x, (id, (c, pv))) -> (x, (id, (c, pv))):: acc) [] // piecesAndCoords
                let stateUpdated:state = {
                               st with
                               piecesOnBoard = piecesAndCoords |> List.fold (fun acc (x, (id, (c, pv))) -> Map.add x (c) acc)  st.piecesOnBoard
                               playerNumber = st.playerNumber
                }
                // TODO: 3. Add all coordinates and pieces from ms to st.piecesOnBoard
                (* Successful play by other player. Update your state *)
                let st' = stateUpdated // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
                
            | RCM (CMPassed pid) ->

                aux st
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux stateUpdate

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, Set<char * int>>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        