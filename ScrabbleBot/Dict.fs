module ScrabbleBot.Dict

    open System.Security.Authentication

     // This trie implementation has been taken from the lecture_4 slides and been reworked
     // 1. Let every Node be a pair of (bool, map<char, Dictionary>)
     // 2. If the key is a word we insert (true, map<char, Dictionary>)
     // 3. The trie can still contain other words and therefore it has to look further even if it sees true or false.
     
    type Dictionary =
    | Leaf
    | Node of bool ref * Dictionary ref list
    
    
    // Find the index of the char in the tree
    let at (key : string) = int key.[0] - int 'A'
    
    // Creates an empty tree
    let empty () = Node(ref false, List.init 26 (fun x -> ref Leaf))
     
     
    let insert key dict : Dictionary =
     let rec aux key =
      function
      | Leaf -> failwith "Failed to insert key in the specified index"
      | Node (b, l) when key = "" -> b.Value <- true
      | Node (b, l) when l.[at key].Value = Leaf -> l.[at key].Value <- empty (); aux key.[1..] l.[at key].Value
      | Node (b, l) -> aux (key.[1..]) l.[at key].Value
     aux key dict
     dict
   // MAKE STEP
       
    let rec lookup key =
    function
     | Leaf -> false
     | Node (b, l) when key = "" -> b.Value
     | Node (b, l) -> lookup (key.[1..]) l.[at key].Value