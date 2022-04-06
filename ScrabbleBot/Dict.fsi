module ScrabbleBot.Dict

    type Dict = Set<string>
    
    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val lookup : string -> Dict -> bool