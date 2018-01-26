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


module Table: TABLE

module type ABSTABLE = 
    sig
        type ('k, 'v) t         (* type is not defined *) 
        val empty       : ('k,'v)t
        val add         : 'k -> 'v -> ('k,'v)t -> ('k,'v)t
        val (>>>)       : ('k*'v) -> ('k,'v)t -> ('k,'v)t
        val (<<<)       : ('k,'v)t -> ('k*'v) -> ('k,'v)t
        val retrieve    : 'k -> ('k,'v)t -> 'v option
        val delete      : 'k -> ('k,'v)t -> ('k,'v)t 
        val dump        : ('k,'v)t -> ('k*'v) list
    end;;

