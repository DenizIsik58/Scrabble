// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = Map<'a, uint32>
    
    val empty: MultiSet<'a> // Create an empty list
    val isEmpty: MultiSet<'a> -> bool// Returns a bool to check if list is empty
    
    val size: MultiSet<'a> -> uint32 // returns the size of the map
    val contains: 'a -> MultiSet<'a> -> bool
    val numItems: 'a -> MultiSet<'a> -> uint32
    val add: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val addSingle: 'a -> MultiSet<'a> -> MultiSet<'a>
    val remove: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val removeSingle: 'a -> MultiSet<'a> -> MultiSet<'a>
    val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
    val foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b