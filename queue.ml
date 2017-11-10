module type IQUEUE = (* immutable queue *) 
    sig 
        type 'a t
        exception Empty
        val empty   : 'a t
        val add     : 'a t -> 'a -> 'a t
        val take    : 'a t -> 'a * 'a t
        val peek    : 'a t -> 'a 
    end

(* immutable queue *)  (* always it makes new queue *)
module IQueue : IQUEUE =
    struct 
        type 'a t = IQ of ('a list * 'a list)
        exception Empty
        let empty           = IQ([],[])
        let add (IQ(l,m))x  = if l=[] then IQ([x],m) else IQ(l,x::m)
        let take            = function
                  IQ([],[])     -> raise Empty
                | IQ([x],l)     -> (x,IQ(List.rev l,[]))
                | IQ(x::xs,l)   -> (x,IQ(xs,l))
                | _             -> failwith "Input queue is broken." 
        let peek            = function
                  IQ([],[])     -> raise Empty
                | IQ(x::_,_)    -> x
                | _             -> failwith "Input queue is broken."
    end
