// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
    type MultiSet<'a when 'a : comparison> = Map<'a, uint32>
    
    let empty : MultiSet<'a> = Map.empty
    let isEmpty (map:MultiSet<'a>) = Map.isEmpty map
    let size (map:MultiSet<'a>) = map |> Map.fold (fun acc _ value -> acc + value) 0u ;
    let contains a (s:MultiSet<'a>) = s.ContainsKey a
    let numItems a (s:MultiSet<'a>) = s.TryFind a |> Option.defaultValue 0u
        
    let add (a:'a) (n:uint32) (s:MultiSet<'a>) : MultiSet<'a> = s.Add (a, n)
    let addSingle (a:'a) (s:MultiSet<'a>) =
        let timesOccured = s.TryFind a |> Option.defaultValue 0u
        if timesOccured > 1u then s.Add (a, timesOccured + 1u) else s.Add(a, 1u)  
    let remove (a:'a) (n:uint32) (s:MultiSet<'a>) : MultiSet<'a> =
        let timesOccured = s.TryFind a |> Option.defaultValue 0u
        if timesOccured <= n then
            s.Remove a
        else
            s.Add(a, timesOccured - n)
        
    let removeSingle (a:'a) (s:MultiSet<'a>) : MultiSet<'a> =
        match a, s with
        | a, s when (numItems a s) <= 1u -> Map.remove a s
        | _ -> Map.add a ((numItems a s) - 1u) s
        
    let fold f acc (s:MultiSet<'b>) = Map.fold f acc s
    
    let foldBack f (s:MultiSet<'b>) acc = Map.foldBack f s acc
