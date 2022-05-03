namespace YourClientName

open System
open System.ComponentModel
open StateMonad

module internal ElitenBot =
    open State
    
    
            // TODO 4. Make a new file - a bot which automates moves. Give it pieces, st.piecesOnBoard, st.hand and return a move.
    
      
    let intToChar (toAsci : uint32) = Convert.ToChar (toAsci + 64u)
    let move (piecesOnBoard:Map<coord, piece>) (hand: MultiSet.MultiSet<uint32>) (pieces:uint32 * uint32) list =
        let wordsInHand = hand |> Map.fold (fun acc key value -> (intToChar key)::acc) [] |> List.toArray |> String
        // Take the string and bruteforce till you find a word in the dictionary if you are the first one to play
        // Otherwise run through the tiles on the board and see if you can place any of chars in your hand by using the step function
        
        
        for entry in piecesOnBoard do
            let x, y = entry.Key
            let pId, (ch, pv) = entry.Value

        