let memo = Hashtbl.create 1
let rec combination n m =
    if n=m then 1 else 
    if m>n then combination m n else 
    match (n,m) with    
        (n,1) -> n
    |   (n,0) -> 1
    |   _ ->
        if Hashtbl.mem memo (n,m) 
            then Hashtbl.find memo (n,m)
            else 
                begin 
                    Hashtbl.add memo (n,m) 
                        ((combination (n-1) m) + (combination (n-1) (m-1)));
                    Hashtbl.find memo (n,m)
                end
;;





