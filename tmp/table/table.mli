type ('k, 'v) t = Empty | Entry of 'k * 'v * ('k, 'v) t
val empty : ('k, 'v) t
val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val (<<<) : ('k, 'v) t -> ('k * 'v) -> ('k, 'v) t
val retrieve : 'k -> ('k, 'v) t -> 'v option
val dump : ('k, 'v) t -> ('k * 'v) list
