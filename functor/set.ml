(* syntax of functor (no abbreviation) *)

module F 
    : functor(Category : sig end) -> sig end 
    = functor(Category : sig end) -> struct end
module C = struct end
module D = F(C)


(* Set module *)
(* e.g. *)
(* Make IntSet *)
#directory "../base"
#use "base.ml"



module IntSet = Set.Make (
    struct
        type t              = int
        let compare i j     = i - j
    end) open IntSet

(* e.g. *)
        let a               = add 1(add 2(add 3 empty))
        let b               = add 2(add 3(add 4(add 5 empty )))
        let ab              = inter a b
        let a_b             = union a b
        let list_a_b        = elements a_b




module type TOrd = 
    sig 
        type t
        val compare : t -> t -> int
    end;;


module type Set     =   
    sig
        type elt
        type t                          (* define  abstruct type t *)
        val empty : t                   (* returns abstract type t *)
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t         (* returns abstract type t *) 
        val inter : t -> t -> t         (* returns abstract type t *)
        val union : t -> t -> t         (* returns abstract type t *)
        val diff  : t -> t -> t         (* returns abstract type t *)
        val elements : t -> elt list
    end

(* define a functor
 * whose element types are hidden 
 * if signature does not exist, 
 * the element types are exposed *)
module MakeSet (S: TOrd) 
    : Set with type elt = S.t           (* 'with' signature *)   
    = struct
        type elt            = S.t
        type t              = elt list
        let empty:t         = []
        let is_empty    s   = if s = [] then true else false
        let rec mem elm     = function
              []                -> false
            | x::xs             -> let r = S.compare elm x in
                                   (r=0) || ((r<0)&&(mem x xs));;
        let rec add elm     = function
              []                -> [elm]
            | (x::xs as l)      -> match S.compare elm x with 
                                      0             -> l
                                    | n when n<0    -> elm :: l
                                    | _             -> x::(add elm xs);;
        let rec inter a b   = match (a,b) with 
              (a,[])            -> []
            | ([],b)            -> []
            | ((x::xs),(y::ys)) -> match S.compare x y with 
                                      0             -> x::(inter xs ys)
                                    | n when n>0    -> inter a ys
                                    | _             -> inter xs b;;
        let rec union a b   = match (a,b) with 
              (a,[])            -> a
            | ([],b)            -> b
            | ((x::xs),(y::ys)) -> match S.compare x y with 
                                      0             -> x::union xs ys
                                    | n when n>0    -> y::union a ys
                                    | _             -> x::union xs b;;
        let rec diff  a b   = match (a,b) with 
              (a,[])            -> a
            | ([],b)            -> []
            | ((x::xs),(y::ys)) -> match S.compare x y with 
                                      0             -> diff xs ys
                                    | n when n>0    -> diff a ys
                                    | _             -> x::diff xs b;;
        let rec elements s  = s 
    end;;

module IntSet       =   MakeSet(struct  type t = int
                                        let compare i j = i - j end);;


module Int          =   struct 
                            type t = int
                            let compare i j = i - j
                        end

(* multiple args to functor *)
module type Pair    =   sig 
                            module Elt : TOrd 
                            module Set : Set with type elt = Elt.t
                        end
module MakePair(T:TOrd) = 
    struct
        module Elt = T
        module Set = MakeSet(Elt)
    end
module IntPair      = MakePair(Int) 


module MakeTest(P:Pair) = 
    struct
        let is_tord_set s  = 
            let rec loop            = function
                  [] | [_]              ->  true
                | a::b::rest            ->  if P.Elt.compare a b>0 
                                                then false
                                                else loop (b::rest) in
            loop (P.Set.elements s)
    end


(* e.g. *)
module Test = MakeTest(IntPair);;
let _ = Test.is_tord_set (IntPair.Set.add 1 IntPair.Set.empty)
