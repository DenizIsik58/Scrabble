namespace YourClientName

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
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        piecesOnBoard : Map<coord, piece>
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
    let givenPieces hand newPieces = newPieces |> List.fold (fun _ (tileId, amount) -> MultiSet.add tileId amount hand) hand 

    let bestFoundMove (firstMove: 'a list) (secondMove: 'a list) =
        if (List.length firstMove > List.length secondMove) && List.length secondMove > 3 then firstMove else secondMove
    
    let isLegalMove (isVertical: bool) (coord: coord) (piecesOnBoard: Map<coord, piece>) =
        let (x, y) = coord
        
        if isVertical then
            let left = match Map.tryFind (x - 1, y) piecesOnBoard with
                        | Some _ -> true
                        | None -> false
            let right = match Map.tryFind (x + 1, y) piecesOnBoard with
                        | Some _ -> true
                        | None -> false
            
            if right || left then false else true
        else
            let down = match Map.tryFind (x, y - 1) piecesOnBoard with
                        | Some _ -> true
                        | None -> false
            let up = match Map.tryFind (x, y + 1) piecesOnBoard with
                        | Some _ -> true
                        | None -> false
            
            if up || down then false else true
            
        
        
        
    let rec findMoveFromGivenCoord (isVertical: bool) (coord: coord) (st:state) (pieces: Map<uint32, Set<char * int>>) (alreadyUsedPieces: (coord * piece) list) (bestMove: (coord * piece) list) =
                  let (x,y) = coord
                  let coord' = if isVertical then (x, (y + 1))  else  ((x + 1), y)
                  
                  match Map.tryFind coord st.piecesOnBoard with
                  | Some(pId, (ch, pv)) -> match Dictionary.step ch st.dict with
                                                  | Some (isEndOfWord, trie') ->
                                                                                 if isEndOfWord then alreadyUsedPieces
                                                                                  else
                                                                                    let st' = {
                                                                                        st with
                                                                                        dict = trie'
                                                                                    }
                                                                                    let bestMove' = if isEndOfWord then bestFoundMove bestMove alreadyUsedPieces else bestMove
                                                                                    findMoveFromGivenCoord isVertical coord' st' pieces alreadyUsedPieces bestMove'
                                                  | None -> bestMove
                  | None -> Map.fold (fun acc key value ->
                            let (ch, pv) = Map.find key pieces |> Set.maxElement 
                            match Dictionary.step ch st.dict with
                                  | Some (isEndOfWord, trie') -> if isLegalMove isVertical coord st.piecesOnBoard then
                                                                        let st' =
                                                                          {st with 
                                                                          dict = trie'
                                                                          hand = MultiSet.removeSingle key st.hand
                                                                          piecesOnBoard = Map.add coord (key, (ch, pv)) st.piecesOnBoard
                                                                        }
                                                                        let alreadyUsedPieces' = (coord, (key, (ch, pv))) :: alreadyUsedPieces
                                                                        let bestMove' =
                                                                            match Map.tryFind coord' st.piecesOnBoard with
                                                                            | None -> if isEndOfWord then bestFoundMove bestMove alreadyUsedPieces' else bestMove
                                                                            | Some (_) -> acc
                                                                        findMoveFromGivenCoord isVertical coord' st' pieces alreadyUsedPieces' bestMove'
                                                                        else acc
                                  | None -> acc ) alreadyUsedPieces st.hand
                                                      
                                                                                      
                                                                                      
                                         
    let move (piecesOnBoard:Map<coord, piece>) (st: state) (hand: MultiSet.MultiSet<uint32>) (pieces: Map<uint32, Set<char * int>>) =        
        
        if Map.isEmpty piecesOnBoard then
            bestFoundMove (findMoveFromGivenCoord true (0, 0) st pieces [] []) (findMoveFromGivenCoord false (0,0) st pieces [] [])

            
        else
            Map.fold (fun acc key _ ->
                                        let bestMove = bestFoundMove (findMoveFromGivenCoord true key st pieces [] []) (findMoveFromGivenCoord false key st pieces [] [])
                                        bestFoundMove acc bestMove) [] st.piecesOnBoard
                                        
             
    let playGame cstream pieces (st : State.state) =

        let st':State.state = {st with
                               hand = st.hand
                               piecesOnBoard = st.piecesOnBoard
                               playerNumber = st.playerNumber
        }
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            // remove the force print when you move on from manual input (or when you have learnt the format)
            let move = move st.piecesOnBoard st st.hand pieces
            // TODO 4. Make a new file - a bot which automates moves. Give it pieces, st.piecesOnBoard, st.hand and return a move.
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move )

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let moves = ms |> List.fold (fun acc (coord, (pId, (ch, pv))) -> pId :: acc) [] // moves
                let removed = moves |> List.fold (fun acc tile -> MultiSet.removeSingle tile acc ) st.hand 
                let piecesAndCoords = ms |> List.fold (fun acc (coord, (pId, (ch, pv))) -> (coord, (pId, (ch, pv))):: acc) [] // piecesAndCoords
                
                let st:State.state = {
                               st with
                               hand = givenPieces removed newPieces
                               piecesOnBoard = piecesAndCoords |> List.fold (fun acc (coord, (pId, (ch, pv))) -> Map.add coord (pId, (ch, pv)) acc)  st.piecesOnBoard
                               playerNumber = st.playerNumber
                }
                // TODO: 1. extract id (uint32) from ms, and remove it from the hand
                // TODO: 2. Add new pieces (id of the tile, amount of times id has been drawn) to the current hand by adding the ids to the hand
                // TODO: 3. Add all coordinates and pieces from ms to st.piecesOnBoard 
                
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                let piecesAndCoords = ms |> List.fold (fun acc (x, (id, (c, pv))) -> (x, (id, (c, pv))):: acc) [] // piecesAndCoords
                let st:State.state = {
                               st with
                               piecesOnBoard = piecesAndCoords |> List.fold (fun acc (x, (id, (c, pv))) -> Map.add x (id, (c, pv)) acc)  st.piecesOnBoard
                               playerNumber = st.playerNumber
                }
                // TODO: 3. Add all coordinates and pieces from ms to st.piecesOnBoard
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st'

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
        