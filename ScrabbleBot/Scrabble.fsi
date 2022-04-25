namespace YourClientName

module internal State =
    type coord = int * int
    type tile = char * int
    type piece = uint * tile
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
    
module Scrabble =

    open ScrabbleUtil
    open System.IO

    
    val startGame :
        boardProg ->                 (* Scrabble board *)
        (bool -> Dictionary.Dict) -> (* Dictionary (call with true if using a Gaddag, and false if using a Trie) *)
        uint32 ->                    (* Number of players *)
        uint32 ->                    (* Your player number *)
        uint32 ->                    (* starting player number *)
        (uint32 * uint32) list ->    (* starting hand (tile id, number of tiles) *)
        Map<uint32, tile> ->         (* Tile lookup table *)
        uint32 option ->             (* Timeout in miliseconds *)
        Stream ->                    (* Communication channel to the server *)
        (unit -> unit)               (* Delay to allow everyone to start at the same time after setup *)
