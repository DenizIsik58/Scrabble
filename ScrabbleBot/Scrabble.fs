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
    let givenPieces hand newPieces = newPieces |> List.fold (fun _ (tileId, amount) -> MultiSet.add tileId amount hand) hand 

    let playGame cstream pieces (st : State.state) =

        let st':State.state = {st with
                               hand = st.hand
                               piecesOnBoard = st.piecesOnBoard
                               playerNumber = st.playerNumber
        }
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input
            // TODO 4. Make a new file - a bot which automates moves. Give it pieces, st.piecesOnBoard, st.hand and return a move.
        
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                let moves = ms |> List.fold (fun acc (x, (id, (c, pv))) -> id :: acc) [] // moves
                let removed = moves |> List.fold (fun acc element -> MultiSet.removeSingle element acc ) st.hand 
                let piecesAndCoords = ms |> List.fold (fun acc (x, (id, (c, pv))) -> (x, (id, (c, pv))):: acc) [] // piecesAndCoords
                
                let st:State.state = {
                               st with
                               hand = givenPieces removed newPieces
                               piecesOnBoard = piecesAndCoords |> List.fold (fun acc (x, (id, (c, pv))) -> Map.add x (id, (c, pv)) acc)  st.piecesOnBoard
                               playerNumber = st.playerNumber
                }
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
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
            (tiles : Map<uint32, tile>)
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
        