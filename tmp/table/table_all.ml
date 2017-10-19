module Table = 
    struct
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
    end;;



module type TABLE1 =
    sig
        type ('k, 'v) t = Empty | Entry of 'k * 'v * ('k, 'v) t
        val empty : ('k, 'v) t
        val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
        val (<<<) : ('k, 'v) t -> ('k * 'v) -> ('k, 'v) t
        val retrieve : 'k -> ('k, 'v) t -> 'v option
        val dump : ('k, 'v) t -> ('k * 'v) list
    end;;

module Table1 : TABLE1 = Table
module Table1' = (Table : TABLE1)



module type TABLE2 = 
    sig
        type ('k, 'v) t
        val empty : ('a, 'b) t
        val add : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
        (* similar *)
    end;;

module AbsTable : TABLE2 = Table;;




module TableAL = (* AL = association list *)
    struct
        type ('k, 'v) t = ('k * 'v) list
        let empty = []
        let add k v table = (k,v) :: table
        let retrieve k table = 
            try Some (List.assoc k table) with Not_found -> None
        let delete k table =
            List.filter (fun (k', v') -> k <> k') table 
        let rec dump = function
              [] -> []
            | (k,v) :: rest -> (k,v) :: (dump (delete k rest))
    end;;

module AbsTableAL : TABLE2 = TableAL;;

