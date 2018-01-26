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
        let g1                  = function 
                  Left(a,b)         -> (a,Left b)
                | Right(a,c)        -> (a,Right c)
        let f2                  = function
                  (Left a ,Left  c) -> Left (Left (a,c))
                | (Left a ,Right d) -> Right(Left (a,d))
                | (Right b,Left  c) -> Left (Right(b,c))
                | (Right b,Right d) -> Right(Right(b,d))
        let g2                  = function
                  Left (Left (a,c)) -> (Left  a,Left  c)
                | Right(Left (a,d)) -> (Left  a,Right d)
                | Left (Right(b,c)) -> (Right b,Left  c)
                | Right(Right(b,d)) -> (Right b,Right d)
        let f3      (f,g)       = function  
                  Left a            -> f a
                | Right b           -> g b
        let g3       f          =          
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


module UnitSeq = 
    struct

        (* following definition can walk 
         * along a sequence step by step with unit stopper () *)
        type 'a useq                = Cons of 'a*(unit->'a useq) ;;
        let rec useq_from z         = Cons (z, fun()->useq_from(succ z));;
        let Cons(x,f)               = useq_from 1;;  (* seq_from 1 *)
        let Cons(y,g)               = f();;         (* seq_from 2 *)
        let Cons(z,h)               = g();;         (* seq_from 3 *)
        let rec mapuseq f (Cons(x,t))= Cons(f x, fun()-> mapuseq f (t()));;
        let reciprocal_useq         = mapuseq (fun x->1.0/.foi x)(useq_from 2);;
        let rec list_of_useq n seq  = match(n,seq)with
                  (0,_)                 -> []
                | (n,Cons(x,f))         -> x::list_of_useq(n-1)(f()) 
        (* e.g. type *)
        (* # list_of_useq 4 reciprocal_seq ;; *)
    end


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
        let rec iter_array f = () (* define as list    e.g. folda fun () *)
        let rec iteri_array f i [|a,b,c|] = () (* f 1 a; f 2 b; f 2 c *) 
        
        let stations = 
            ["Tokyo";"Shinagawa";"Shin-Yokohama";"Nagoya";"Kyoto";"Shin-Osaka" ]
        let print_station s = print_string "Station: "; print_endline s ;;
        iter print_station stations ;;

    end

module IO = 
    struct 
        let open_file_at_last filename
            = open_out_gen [Open_wronly; Open_append; Open_text] 0o755 filename

        (*
        (* IO functions *)
        open_out filename
        open_in filename
        close_out filename
        close_in filename

        output_string
        output_char
        output_byte
        fluesh (* casuse write action immediately *)

        (* to stdout *)
        print_string  == output_string stdout
        print_char
        print_int
        print_float 
        print_endline 
        print_newline

        (* to stderr *)
        prerr_string
        prerr_char
        ...

        (* from stdin *)
        read_line
        read_int
        read_float
        *)
        let opg             = open_out_gen
        let open_create s   = opg [Open_wronly;Open_creat;Open_text]0o666 s
        let open_append s   = opg [Open_wronly;Open_append;Open_text]0o666 s
        let prl             = print_endline
        let ask_remove  s   = let()= prl ("overwrite "^s^" ? (yes/no)") in 
                              if "yes"=read_line()then Sys.remove s else exit 1;;
    end

module Module = 
    struct
        let array_make      = Array.make
        let array_init      = Array.init (* # init 10(fun i->char_of_int(i+48)) *)
        let array_length    = Array.length
        let array_append    = Array.append
        let array_concat    = Array.concat
        let array_of_list   = Array.of_list
        let list_of_array   = Array.to_list
        let array_map       = Array.map
        let array_itter     = Array.iter

        (* escape sequence *)
        (* %d int                       *)
        (* %x hexadecimal               *)
        (* %X HEXADECIMAL               *)
        (* %s string                    *)
        (* %c char                      *)
        (* %C interpret EscapeSequence  *)
        (* %f float                     *)
        (* %B BOOL                      *)
        (* %% %                         *)
        let printf          = Printf.printf
        let fprintf         = Printf.fprintf
        let sprintf         = Printf.sprintf (* return string *)
        let scanf           = Scanf.scanf
        let fscanf          = Scanf.fscanf
        let sscanf          = Scanf.sscanf
        let f name age      = name^" can"^(if age<20 then"not "else" ")^"vote."
        let g s             = sscanf s "%s is %d years old." f
        (* # g "Yourname is 20 years old."      *)
        
        (* printf takes type format6 *)
        (* # open CamlinternalFormatBasics  ;;                           *)
        (* # format_of_string "a %s b %d c" ;;                          
         - : (string -> int -> '_a, '_b, '_c, '_d, '_d, '_a) format6
         = Format (Char_literal ('a ',
                        String (No_padding,
                            Char_literal (' b ',
                                Int (Int_d, No_padding, No_precision,
                                    Char_literal (' c', End_of_format))))),
            "a %s b %d c")      *)
        
        (* non-regular libs *)
        (* # #load "nums.cma";;     *) (* or $ ocaml "nums.cma" *)
        (* # open Num               *)
        (* # Int 1 +/ Int 2         *)
        (* # Int 1 // Int 3         *) (* interpreted as type constructor Ratio *)

        let queue_create        = Queue.create
        let queue_add           = Queue.add
        let queue_take          = Queue.take 

    end open Module


(* e.g. dot notation is ambiguous *)
module M = struct 
        type r = {a:int; b:int} 
    end
        (* let x = {M.a=1; M.b=2} in    *)
        (* x.M.a + x.M.b                *) (* 3 *)
        (* x.(M.a) + x.(M.b)            *) (* err *) 


