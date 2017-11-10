open Base.Polymorphic
open Base.MyList

module type TABLE =
    sig
        type ('k, 'v) t 
        val empty       : ('k,'v)t
        val add         : 'k -> 'v -> ('k,'v)t -> ('k,'v)t
        val (>>>)       : ('k*'v)  -> ('k,'v)t -> ('k,'v)t
        val (<<<)       : ('k,'v)t -> ('k*'v) -> ('k,'v)t
        val foldtbl     : ('k -> 'v -> 'a -> 'a) -> 'a -> ('k,'v)t -> 'a
        val retrieve    : 'k -> ('k,'v)t -> 'v option
        val delete      : 'k -> ('k,'v)t -> ('k,'v)t
        val dump        : ('k,'v)t -> ('k*'v) list
    end;;

module Table : TABLE = 
    struct
        type ('k, 'v) t     = Empty | Entry of 'k*'v*('k,'v)t
        let empty           = Empty
        let add k v table   = Entry (k, v, table)  
        let (>>>)(k,v)table = add k v table
        let (<<<)table(k,v) = add k v table  
        let rec foldtbl h c = function
                  Empty             -> c
                | Entry(k,v,rest)   -> h k v (foldtbl h c rest)
        let rec retrieve key= foldtbl(fun k v xs->if k=key then Some v else xs)None
        let rec delete key t= foldtbl(fun k v->if k=key then id else add k v)Empty t
        let rec dump t      = foldtbl (fun k v xs->(k,v)::xs) [] t     
    end;;

module type ABSTABLE = 
    sig
        type ('k, 'v) t
        val empty       : ('k,'v)t
        val add         : 'k -> 'v -> ('k,'v)t -> ('k,'v)t
        val (>>>)       : ('k*'v) -> ('k,'v)t -> ('k,'v)t
        val (<<<)       : ('k,'v)t -> ('k*'v) -> ('k,'v)t
        val retrieve    : 'k -> ('k,'v)t -> 'v option
        val delete      : 'k -> ('k,'v)t -> ('k,'v)t
        val dump        : ('k,'v)t -> ('k*'v) list
    end
module TableAL : ABSTABLE = (* AL = association list *)
    struct
        type ('k, 'v) t         = ('k*'v) list
        let empty               = []
        let add k v table       = (k,v)::table
        let (>>>) (k,v) table   = add k v table 
        let (<<<) table (k,v)   = add k v table 
        let retrieve k table    = try Some (List.assoc k table)with Not_found->None
        let delete k table      = List.filter (fun (k', v') -> k <> k') table 
        let rec dump         l  = foldr(fun (k,v)->(cons(k,v))$(delete k))[] l
    end;;

module AbsTableAL   = (TableAL: ABSTABLE);;
