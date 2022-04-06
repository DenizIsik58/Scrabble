module ScrabbleBot.Dict



    type Dict = Set<string>
    
    let empty (u:unit) :Dict = Set.empty
        
    let insert s (d:Dict) : Dict = d.Add s 
  
    let lookup s (d:Dict) = d.Contains s