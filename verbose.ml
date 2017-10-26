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


        (* association list *)
        let rec assoc key = function
                (k,v)::_ when k=key -> v
              |  _    :: rest       -> assoc key rest 
              (* _                  -> raise Match_failure "" *)
        let city_phone = [("Kyoto","075");("Osaka","06");("Tokyo","03")] ;;


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

module Expr = 
    struct 
        type arith                  =
                  Const of int
                | Add of arith*arith
                | Mul of arith*arith
        let rec eval                = function
                  Const n               -> n
                | Add(n,m)              -> (eval n)+(eval m)
                | Mul(n,m)              -> (eval n)*(eval m)
        let rec string_of_arith     = let soa = string_of_arith in function
                  Const n               -> soi n          
                | Add(n,m)              -> "(" ^ soa n ^ "+" ^ soa m ^ ")"
                | Mul(n,m)              -> soa n ^ "*" ^ soa m 
        let rec expand              = function
                  Add(a,b)              -> Add(expand a,expand b)
                | Mul(n,Add(a,b))       -> Add(expand(Mul(n,a)),expand(Mul(n,b)))
                | Mul(Add(a,b),m)       -> Add(expand(Mul(a,m)),expand(Mul(b,m)))
                | x                     -> x
        (* e.g.  (3+4)*(2+5) *)
        let expr                    = 
            Mul( 
                Add ((Const 3),(Const 4)),
                Add ((Const 2),(Const 5))
            )
    end open Expr 


module CurryHaward = 
    struct 
        (* a*(b+c)              -> (a*b)+(a*c)             *)
        (* (a*b)+(a*c)          -> a*(b+c)                 *)
        (* (a+b)*(c+d)          -> (a*c+a*d)+(b*c+b*d)     *)
        (* (a*c+a*d)+(b*c+b*d)  -> (a+b)*(c+d)             *)
        (* (c^a)*(c^b)          -> c^(a+b)                 *) 
        (* c^(a+b)              -> (c^a)*(c^b)             *)
        let f1                  = function 
                  (a,Left  b)       -> Left (a,b)
                | (a,Right c)       -> Right(a,c)
        let f2                  = function 
                  Left(a,b)         -> (a,Left b)
                | Right(a,c)        -> (a,Right c)
        let f3                  = function
                  (Left a ,Left  c) -> Left (Left (a,c))
                | (Left a ,Right d) -> Right(Left (a,d))
                | (Right b,Left  c) -> Left (Right(b,c))
                | (Right b,Right d) -> Right(Right(b,d))
        let f4                  = function
                  Left (Left (a,c)) -> (Left  a,Left  c)
                | Right(Left (a,d)) -> (Left  a,Right d)
                | Left (Right(b,c)) -> (Right b,Left  c)
                | Right(Right(b,d)) -> (Right b,Right d)
        let f5      (f,g)       = function  
                  Left a            -> f a
                | Right b           -> g b
        let f6       f          =          
            let left f a = f (Left a) in let right f b = f (Right b) in 
            (left f, right f)
    end  


module BadPrime =
    struct
        (*  #use "prime".ml

        (* expansion make slow *)
        let first_divisor_slow  n   = foldn (filter(divisible n)id succ) 2 n 
        let isprime             n   = foldr((&&)$((!=)0)$((mod)n))true(2--(n-1))
        (* m--n will cause stack overflow when 1--1000000 *)
        let add_prime               = filter is_prime cons (k id)    
        let prime_tbl m n           = foldr add_prime [] (m--n)

        *)
    end

(*
        type 'a seq                 = Cons of 'a*(unit->'a seq) ;;
        let rec from n              = Cons (n, fun()-> from (n+1));;
        let Cons(x,f)               = from 1;;
        let Cons(y,g)               = f();;
        let Cons(z,h)               = g();;
        let rec mapseq f(Cons(x,t)) = Cons(f x, fun()-> mapseq f (t()));;
        let reciprocals = mapseq (fun x-> 1.0 /. float_of_int x) (from 2);;
        let rec take n s = match(n,s)with
                  (0,_)         -> []
                | (n,Cons(x,f)) -> x::(take(n-1)(f()));;
*)




(* 
        (* when usage (is not pattern) *)
        let f = function 
              []                    -> 0
            | a::x when a>0         -> 1
            | a::x (*when a<=0*)    -> 2  (* commentout throws warning *) 
*)





module CoinChange   = 
    struct
        let rec change coins amount = match (coins, amount) with
              (_,0)                     -> []
            | ((c::rest)as coins,total) -> 
                    if c>total then change rest total else ( 
                        try c::change coins(total-c) 
                        with Failure "change"->change rest total )
            | _                         -> raise (Failure "change");;

        let us_coins = [25;10;5;1]
        let gb_coins = [50;20;10;5;2;1]
    end open CoinChange

module Exception = 
    struct
        (*
        type exn        = Invalid_argument of string
                        | Not_found
                        | Division_by_zero
                        | End_of_file
                        | Failure
                        | ...   *)

        (* my exception *)
        exception Foo;;
        exception Bar of int;;
        exception Hoge of string;;
        exception Zero_found ;;

        let prod_list   l       = let rec pl = function
                  []                -> 1
                | a::_ when a=0     -> raise Zero_found
                | a::rest           -> a*(pl rest) in
            try pl l with Zero_found -> 0;;

        (*  
        invalid_arg
        failwith 
        try expr with exn_pattern -> expr 
        *) 
        let map_sqrt l          = 
            let sqrt'' x = if x<0. then invalid_arg "sqrt (minus)" else sqrt' x in
            try Some (listr sqrt' l ) 
            with Invalid_argument "sqrt (minus)" -> None;;

        (* e.g. exception list *)
        let exnlist = [Hoge "hoge"; Bar 0; Foo]
        let f = function
              Foo           -> 0
            | Bar n         -> n
            | Hoge "hoge"   -> 1
            | x             -> raise x;;
    end 

module ControlStructure =
    struct
        let unit1 = print_string "Hello, " 
        let unit2 = print_string "world!"
        let unit3 = print_endline "" ;;
        
        let()=unit1 in let()=unit2 in unit3    ;;
        let f x y z=5 in f unit3 unit2 unit1   ;;  (* reversed order *)
        let ((),(),()) = ( unit3,unit2,unit1 ) ;;  (* reversed order *)
        unit1; unit2; unit3                    ;;
 
        let print_hello ()  = print_string "Hello, "; 0;; (* () -> int *)

        (* if *)
        let g b     = if b then unit1; unit2; unit3      (* suger syntax *)
        (* let h b     = if b then(unit3, unit2, unit1)*)  (* type error *)
        (* suger syntax :   "if b then unit"   => "if b then unit else ()"  *)

        (* while *)
        let parrot ()       = let str = ref "" in
            while (str:=read_line(); !str<>".") do
                print_string !str;
                print_endline !str
            done;;

        (* whle *)
        let rec whle cond body = 
            if (fun()->cond)() then ((fun()->body)();whle cond body)
        
        (* e.g. *)
        let fact n          = let i=ref 1 and res=ref 1 in
            whle (!i<=n) (
                res := !res * !i;
                i   := !i + 1
            ); 
            !res;;

        let rec iter f  = foldr (fun x xs->begin f x;xs end) ()
        
        let stations = 
            ["Tokyo";"Shinagawa";"Shin-Yokohama";"Nagoya";"Kyoto";"Shin-Osaka" ]
        let print_station s = print_string "Station: "; print_endline s ;;
        iter print_station stations ;;

    end



