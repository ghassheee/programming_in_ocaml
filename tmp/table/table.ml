
type ('k, 'v) t = Empty | Entry of 'k * 'v * ('k, 'v) t

let empty = Empty

let add k v table = Entry (k, v, table)  

let (<<<) table (k,v) = add k v table  

let rec retrieve key = function
      Empty -> None
    | Entry (k,v,rest) -> 
            if k = key then Some v else retrieve key rest;;

let rec delete key = function
      Empty -> Empty
    | Entry (k,v,rest) -> 
            if k = key 
                then delete key rest 
                else Entry (k, v, delete key rest);;
    
let rec dump = function
      Empty -> []
    | Entry (k,v,rest) -> (k,v) :: (dump (delete k rest));;






