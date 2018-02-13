
(* polymorphic variant              *)
(* allows a lot of ill-definitions  *)
(* e.g.                             *)
let next_season_wrong   = function
      `Spring                   -> `Summer
    | `Summer                   -> `Autumn
    | `Autmn (* spell miss *)   -> `Winter
    (* pattern match hole *)

let hoge b              = if b then `A 1 else `B
let a                   = (`A 2,`A true)

let add_A x             = `A    :: x
let add_B x             = `B    :: x
let add_C n x           = `C n  :: x
let l                   = add_C 1 (add_B (add_A (add_C 4 [])))

let c                   = ref [`Can]
let _                   = c :=  `May :: !c 
let _                   = c := (`Will:[`Can|`May|`Will]):: !c     
                    (* Closing the type, we cannot add new type anymore *)


(* [> ] type and [< ] type *)
let kani                = function (* : [< `R|`L ] -> string *)
      `R                    -> "walking to the right"
    | `L                    -> "walking to the left"
let kani'               = function (* : [> `R|`L ] -> string *)
      `R                    -> "walking to the right"
    | `L                    -> "walking to the left"
    | x                     -> "hoge"
let human               = function (* : [< `B|`F|`L|`R ] -> string *)
      `F                    -> "walking forward"
    | `B                    -> "walking backward"
    | (`R|`L) as x          -> kani x
(* let human            = function
      `F                    -> "walking forward"
    | `B                    -> "walking backward"
    |  x                    -> kani x  (* => type error *) *)
let mirror              = function (* : [> `R|`L ] -> [> `R|`L ] *)
      `R                    -> `L
    | `L                    -> `R
    | x                     -> x

let a                   = [kani; human] (* ([<`R|`L]       -> string) list  *)
let b                   = [kani; kani'] (* ([`R|`L|`F|`B]  -> string) list  *)


(* type definitions *)
type seasons            = [ `Spring | `Summer | `Autumn | `Winter ]
type seasons_of_japan   = [ seasons | `Tsuyu ]
let a : [> seasons ]    = `Tsuyu
let  next_season        = function
      `Spring               -> `Summer
    | `Summer               -> `Autumn
    | `Autumn               -> `Winter
    | `Winter               -> `Spring


(* '#'-pattern *)
type lr                 = [`L|`R]
let  human              = function
      `F                    -> "walking Forward"
    | `B                    -> "walking Backward"
    | #lr as x              -> kani x

type t  = [ 
      `A of int
    | `B of string * int 
    | `C of int * bool * t 
    (* ... *)
]
let hoge (#t as x) = x      (* hoge : [< t ] -> [> t ] *)


(* e.g. List *)
let x       = `Nil        (* [> `Nil ]                                     *)
let y       = `Cons(1,x)  (* [> `Cons of int * [> `Nil ]                   *)
let z       = `Cons(2,y)  (* [> `Cons of int * [> `Cons of int * [> `Nil ] *)
let u       = `Cons(true,z)
let a       = `Cons(3,`App(y,z))
let b       = `App(z,a)
let l       = [x;y;z]
(* let l    = [x;y;z;u]   => type error *)
let rec length          = function
      `Nil                  -> 0
    | `Cons(x,xs)           -> 1 + length xs
let nil                 = `Nil
let cons x xs           = `Cons(x,xs)
let rec rev_append l m  = match l with
      `Nil                  -> m
    | `Cons(x,xs)           -> rev_append xs (cons x m)
let rev l               = rev_append l nil
let append l m          = rev_append (rev l) m
let rec foldr h c       = function
      `Nil                  -> c
    | `Cons(x,xs)           -> h x (foldr h c xs)
let map f l             = foldr (fun x ->cons(f x)) nil l
let rec foldnr s z n    = if n<=0 then nil else cons z (foldnr s(s z)(n-1))
let downto1 n           = foldnr pred  n n 

let rec list_of_alist   = let f = list_of_alist in function
      `Nil                  -> nil
    | `Cons(x,xs)           -> cons(x)(f xs)
    | `App(x,y)             -> append(f x)(f y)

let rec alength         = function
      `Nil                  -> 0
    | `Cons(x,xs)           -> 1 + alength xs
    | `App(x,y)             -> alength x + alength y
let rec alength_wrong   = function
     (`Nil|`Cons(_,_))as l  -> length l (* recursive call => type error *)
    | `App(a,b)             -> alength_wrong a + alength_wrong b
    
type ('a,'b) list       = [ `Nil | `Cons of 'a * 'b ]
let make_length  f      = function
      `Nil                  -> 0
    | `Cons(x,xs)           -> 1 + f xs
let rec length   l      = make_length length l
let make_alength f      = function
      #list as l            -> make_length f l
    | `App(x,y)             -> f x + f y
let rec alength  l      = make_alength alength l



(* '&' - intersection of types *)
let f               = function
      `A x              -> x+1
    | `B                -> 2
let g               = function
      `A x              -> int_of_float x + 2
    | `B                -> 3
let f_or_g b        = if b then f else g (* : bool->[< `A of int & float|`B ] *)
let _               = f_or_g false `B
(* let _            = f_or_g true (`A 1) (* type 'int&float' has no element *)*)


