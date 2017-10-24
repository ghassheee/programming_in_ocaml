(* verbose.ml
 *
 * This file supplements "base.ml".
 * Sometimes practical way is not beautiful or complicated.
 * This file includes all ideas in constructing "base.ml".
 *
 * e.g.
 * - not basic things (e.g. bmi )
 * - beautiful expression but cause stackoverflow (e.g. fold expression) 
 * - primitive and not practical (e.g. crypto thing)
 *)

#use "base.ml"

        (* char *)
        let capitalize c = 
            let int = int_of_char c in
            if int<123 && int>96 
                then char_of_int (int - 32) 
                else char_of_int int

        (* num *)
        let fib'  = fst $ foldn (fun(x,y)->(x+y,x)) (0,1) 



        (* random list self-made *)
        let nextrand seed =
            let a = 16807.0 and m =2147483647.0 in 
            let t = a *. seed in
            t -. m *. floor(t/.m) ;;
        let rec randlist_of_igarashi n seed tail = if n=0 then (seed,tail)
            else randlist_of_igarashi(n-1)(nextrand seed)(seed::tail) ;;
        

        (* bmi *)
        let bmi name h w  =
            let bm        = w /. (square h) *. 10000.0 in
            let bmi_value = (float_of_int(int_of_float(100.0*.bm+.0.5)))/.100.0 in
            let shape     =
                if bmi_value < 18.0 then "thin" else
                    if bmi_value < 25.0 then "moderate" else
                        if bmi_value < 30.0 then "fat" else "fattest" in
            print_string ( name ^ "'s BMI is " ^ 
                            string_of_float bmi_value ^ ", " ^ shape ^ ".");;

        (* record e.g. *)
        type student            = {name: string; id: int}
        let string_of_student s = s.name ^ "'s ID is " ^ string_of_int s.id

        (* variant e.g. 2D functions*)
        (* 2D is very important !!              *)
        (* because the sight has 2D interface   *)

module Plane =
    struct
        type figure              = 
                  Point
                | Circle                of float
                | Rectangle             of float * float
                | Square                of float
        let area_of_figure          = function
                  Point                 -> 0.0
                | Circle    r           -> pi *. (r ^^ 2)
                | Rectangle (x,y)       -> abs_float (x *. y)
                | Square    x           -> x ^^ 2
        let farpoint_of_figure      = function
                  Point                 -> 0.0
                | Circle    r           -> abs_float r
                | Rectangle (x,y)       -> sqrt ((x^^2)+.(y^^2))
                | Square    x           -> (sqrt 2.0) *. (abs_float x) 
        let     similar     x y     = match (x,y) with
                  (Point, Point)     
                | (Circle _, Circle _)  
                | (Square _, Square _)              -> true
                | (Square _, Rectangle(x,y))     
                | (Rectangle(x,y), Square _)        -> x=y
                | (Rectangle(x,y), Rectangle(a,b))  -> x*.b-.y*.a=0.
                | _                                 -> false
        type 'a with_location       = {x: float; y: float; body: 'a}
        let rec overlap a b         = 
            let rec loop c d            = match (c,d) with
              (Point,Point)                     -> a.x=b.x && a.y=b.y
            | (Circle r,Circle s)               -> 
                    (s+.r)^^2 > ((a.x-.b.x)^^2)+.((a.y-.b.y)^^2)
            | (Rectangle(x,y),Rectangle(u,v))   ->
                    abs_float((x-.u)/.2.) > abs_float(a.x-.b.x) &&
                    abs_float((y-.v)/.2.) > abs_float(a.y-.b.y) 
            | (Rectangle(x,y),Circle r)         ->
                    let (cx,cy) = (a.x+.x/.2., a.y+.y/.2.) in (
                    ((b.x-.cx)^^2) +. ((b.y-.cy)^^2)  < r^^2         &&
                    abs_float(b.x-.a.x) > abs_float(cx-.a.x)    &&
                    abs_float(b.y-.a.y) > abs_float(cy-.a.y) )  ||
                    loop (Rectangle(x,y))(Rectangle(r,r))
            | (Circle r,Rectangle(x,y))         -> overlap b a
            | (Square x,o)                      -> loop(Rectangle(x,x))o 
            | (o,Square x)                      -> overlap b a
            | _ -> false
            in loop a.body b.body
    end
open Plane










module Color =
    struct
        type color              = Black | Blue | Red | Magenta 
                                | Green | Cyan | Yellow | White ;;
        let reverse_color       = function
                  Black             -> White
                | Blue              -> Yellow
                | Red               -> Cyan
                | Magenta           -> Green
                | Green             -> Magenta
                | Cyan              -> Red
                | Yellow            -> Blue
                | White             -> Black
    end
open Color




module BadNat   =
    struct
        type    nat         = Zero | Succ of nat;;
        let     zero        = Zero
        let     one         = Succ zero
        let     two         = Succ one
        let     three       = Succ two
        let rec add n       = function 
              Zero              -> n 
            | Succ m            -> Succ (add m n);;
    end
        open BadNat;;

module EvenOdd = 
    struct
        type    even                = Zero | Succ_E of odd
            and odd                 = Succ_O of even
        let rec o_plus_e(Succ_O o)e = Succ_O(e_plus_e o e)
            and e_plus_e  e f       = match e with
                  Zero              -> f
                | Succ_E o          -> Succ_E (o_plus_e o f)
    end
        open EvenOdd




module Tree =
    struct
        let rec bt_rt'           = 
            let h x = let Br(a,l,_) = bt_rt' x in br a l in function
              RLf                   -> br None lf lf 
            | RBr(a,l)              -> br(Some a)(foldr h lf l)lf
    end




        (* expansion make slow *)
        let first_divisor_slow  n   = foldn (filter(divisible n)id succ) 2 n 
        let isprime             n   = foldr((&&)$((!=)0)$((mod)n))true(2--(n-1))
        
        (* m--n will cause stack overflow when 1--1000000 *)
        let add_prime               = filter is_prime cons (k id)    
        let prime_tbl m n           = foldr add_prime [] (m--n)

(*
type 'a seq = Cons of 'a*(unit->'a seq) ;;
let rec from n = Cons (n, fun () -> from (n+1));;
let Cons(x,f) = from 1;;
let Cons(y,g) = f();;
let Cons(z,h) = g();;
let rec mapseq f (Cons(x,t)) = 
    Cons(f x, fun() -> mapseq f (t()));;
let reciprocals = mapseq (fun x -> 1.0 /. float_of_int x) (from 2);;
let rec take n s = match(n,s)with
          (0,_)         -> []
        | (n,Cons(x,f)) -> x::(take(n-1)(f()));;
*)




(* when (is not pattern) *) 
let f = function 
      []                    -> 0
    | a::x when a>0         -> 1
    | a::x (*when a<=0*)    -> 2;; (* if comment out compiler throws warning *) 
