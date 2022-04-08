module ScrabbleBot.Dict

    type Dictionary
    
    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool