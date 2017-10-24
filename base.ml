open Int64

exception Arg 

let soi = string_of_int



module Types = 
    struct
        type 'a maybe           = Nothing | Just of 'a
        type ('a,'b) either     = Left of 'a | Right of 'b
    end open Types



module Polymorphic =
    struct
        let fst (a,b)           = a
        let snd (a,b)           = b
        let id  a               = a
        let apply f a           = f a
        let twice f a           = f (f a)
        let fourtimes x         = twice twice x
        let rec foldn s z n     = if n<=0  then z else foldn s (s z)(n-1)
        let rec limit s z       = if s z=z then z else limit s (s z)
        let curry f x y         = f (x,y)
        let uncurry f(x,y)      = f x y

        (* combinators are polymorphic *)
        let ($) f g x       = f (g x)  (* dot operator in Haskell *)
        let i x             = x
        let k x y           = x
        let s x y z         = x z (y z)
        let t x y           = y x
        let pair f g x      = (f x)(g x)
        let filter p f g x  = if p x then f x else g x
    end open Polymorphic












module MyInt64  =
    struct
        (* fibonacci *)
        let     table       = Hashtbl.create 1;;
        let rec fibL        = function
            0L -> 0L
          | 1L -> 1L 
          | n  -> if Hashtbl.mem table n
                    then Hashtbl.find table n
                    else begin 
                        Hashtbl.add table n(add(fibL(sub n 1L))(fibL(sub n 2L)));
                        Hashtbl.find table n end ;;

    end open MyInt64




































module Int = 
    struct
        let     even    n   = if n mod 2=0 then true else false;;
        let     odd     n   = not ( even n ) ;;
        let     abs     n   = if n>0  then n else -n ;;
        let     succ    n   = n+1
        let     pred    n   = n-1
        let rec pow   n m   = foldn (( * )n) 1 m
        let     ( ** )      = pow 
        let rec sum   i n f = if i=n then f i else f i + sum(i+1)n f;;
        let     square  n   = n*n;;
        let     cube    n   = n*n*n;;
        let sum_of_square n = sum 1 n square
        let sum_of_cube   n = sum 1 n cube
        let fib         n   = fibL (of_int n)
        let rec fact    n   = if n<0 then raise Arg else 
                                if n=0 then 1 else n*fact(n-1);;
        
        (* combination *)
        let     tbl         = Hashtbl.create 1;;
        let rec comb n m    =
            if n<0 then 0 else 
            if n=m then 1 else
            if m>n then comb m n else match (n,m) with 
                    (n,1)   -> n
                  | (n,0)   -> 1
                  | _       -> 
                    if Hashtbl.mem tbl (n,m)
                        then Hashtbl.find tbl (n,m)
                        else begin
                            Hashtbl.add tbl(n,m)((comb(n-1)m)+(comb(n-1)(m-1)));
                            Hashtbl.find tbl (n,m) end;;

        (* euclid method *)
        let rec gcd n m     =
            if n<m then gcd m n else
                if m<1 then 0 else 
                    if n=m then n else
                        if m=1 then 1 else
                            if (n mod m)=0 then m else 
                                gcd n (n mod m)
    end open Int








module Float =
    struct
        let     pi              = 3.141592653589793;;
        let     e               = 2.718281828459045;;
        let     abs_float   x   = if x > 0.0 then x else -.x;;
        let area_of_circle  r   = if r > 0.0 then r*.r*.pi*.2.0 else 0.0;;
        let     square      x   = x*.x
        let     cube        x   = x*.x*.x
        let rec pow_float x n   = if n<=0 then 1.0 else x*.pow_float x (n-1);;
        let (^^)                = pow_float

        (* newton raphson method *)
        let derivation    f x   = let dx=1e-10 in (f(x+.dx) -. f x)/.dx ;;
        let fixedpoint   f init = 
            let epsiron             = 1e-10 in
            let rec loop x          = let fx = f x in
                                      if abs_float(x-.fx)<epsiron 
                                        then x
                                        else loop fx    in loop init
        let newton1step f x     = x -. (f x)/.(derivation f x) 
        let newton_method f ini = fixedpoint (newton1step f) ini 
        let sqrt a              = newton_method (fun x->x*.x-.a) 1.0

        (* integral *)
        let integral f a b      =     
            let dx                  = 1e-7 in
            let n                   = (b-.a)/.dx in
            let rec loop k sum      = let sum' = sum +.(f(a+.k*.dx))*.dx in
            if k>n-.1.0 then sum' else loop (k+.1.0) sum' in
            ((f a)*.dx+.2.0*.(loop 1.0 0.0)+.(f b)*.dx) /. 2.0

        (* average *)
        let add_average x y     = (x+.y)/.2.0;;
        let mul_average x y     = sqrt (x*.y);;
        let average   (x,y)     = (add_average x y ,mul_average x y);;
        let fix_average f i     =
            let     epsiron         = 1e-8 in
            let rec loop(x,y)       = if abs_float(x-.y)<epsiron
                                        then (x,y)
                                        else loop (f(x,y))      in loop i

        let med_average x y = fix_average average (x,y) 
        (* use `#trace function` if evaluation does not end *) 
        (* arctan1 = 1 - 1/3 + 1/5 - 1/7 + ... + 1/(4k+1) - 1(4k+3) *)
        let arctan1 n = 
            let rec pos n = neg(n-1) +. 1.0/.(float_of_int(4*n+1)) 
            and     neg n = if n<0 then 0.0 else
                            pos n    -. 1.0/.(float_of_int(4*n+3))  in pos n 
    end open Float


module String =
    struct
        let cat s t         = s ^ t
        let sand t s        = s ^ t ^ s
        let emphasize s     = sand s "_"
        let repeat s n      = foldn ((^)s) "" n
    end open String




module MyList =
    struct
        let cons x xs           = x::xs
        let rec foldl f i       = function    []    -> i
                                            | x::xs -> f (foldl f i xs) x 
        let rec foldr f i       = function    []    -> i
                                            | x::xs -> f x (foldr f i xs) 
        let listr  f            = foldr (fun x -> cons(f x)) []
        let forall p            = foldr (fun x -> (&&)(p x)) true 
        let exists p            = foldr (fun x -> (||)(p x)) false 
        let length l            = foldr (Polymorphic.k succ) 0 l  
        let append l m          = foldr cons m l        
        let snoc x xs           = append xs [x]
        let rev_append l m      = foldr snoc m l 
        let rev l               = rev_append l []
        let concat              = foldr append []
        let rec elem n l        = match (n,l) with
                (n,l) when n<0||l=[]    -> raise Arg
              | (0,x::_)                -> x
              | (n,_::xs)               -> elem (n-1) xs
        let (%) l n             = elem n l
        let rec zip l r         = match (l,r) with
                ([],_) | (_,[])         -> []
              | (x::xs,y::ys)           -> (x,y)::zip xs ys ;;
        let rec unzip       = function
                []                      -> ([],[])
              | (x,y)::rest             -> let(xs,ys)=unzip rest in(x::xs,y::ys)
        let     filterr p       = foldr (filter p cons (k id)) [] 
        let     (|-)            = filterr
        let iterate     x xs    = let ()=x in xs

        (* sorting *)
        let rec randlist max    = function
                n when n<1          -> []
              | n                   -> (Random.float max)::(randlist max (n-1));;
        let rec insert x        = function
                []                  -> [x]
              | y::ys when x<y      -> x::y::ys
              | y::ys               -> y::(insert x ys) ;;
        let insertion_sort      = foldr insert [] ;;
        let rec quick_sort      = function
                ([] | [_]) as s     -> s
              | pivot::rest         -> (
                  let rec partition l r = function
                        []              -> (quick_sort l)@(pivot::quick_sort r)    
                      | x::xs           -> if pivot<x 
                                            then partition l(x::r)xs
                                            else partition(x::l)r xs in
                  partition [] [] rest) ;;

        (* Nat Algebra to List Algebra *)
        let rec foldnr s z n    = if n<=0 then [] else z::foldnr s(s z)(n-1)
        let     downto1 n       = foldnr pred n n  ;;
        open Int64
        let rec natlist m n     = if m>n then [] else m::natlist (m+1) n
        let rec natlistL m n    = if m>n then [] else m::natlistL(add m 1L)n
        let (--)                = natlist
        let (---)               = natlistL           

        let rec product l m     = match (l,m) with
                  ([],_) | (_,[])   -> []
                | (x::xs,y::ys)     -> let f y = cons(x,y) in 
                                       (foldr f [] m) @ product xs m
        let rec self_product    = function
                  []                -> []
                | (x::xs) as l      -> let f y = cons(x,y) in
                                       (foldr f [] l) @ self_product xs
        let rec product_with_filter p l m = match(l,m) with
                  ([],_)|(_,[])     -> []
                | (x::xs,y::ys)     -> let f y = filter p cons (k id) (x,y) in
                                       (foldr f[]m) @ product_with_filter p xs m
        let squares r           = 
            let s = truncate(sqrt(float_of_int r)) + 1 in
            product_with_filter (fun(x,y)->x*x+y*y=r)(1--s)(1--s) 
        (* Haskell>>> squares r = let s = sqrt r in
         *              [ (x,y) | x<-[1..s], y<-[1..s], x^2+y^2=r ] *) 
    end open MyList










module Tree =
    struct
        (* binary tree *) 
        type 'a btree           = Lf | Br of 'a * 'a btree * 'a btree 
        let     lf              = Lf
        let     br a l r        = Br(a,l,r)
        let rec foldbt h c      = function
                  Lf                -> c
                | Br(a,l,r)         -> h a (foldbt h c l)(foldbt h c r)  
        let size                = foldbt (fun x l r -> 1 + l + r)   0
        let depth               = foldbt (fun x l r -> 1 + max l r) 0
        let elem      t x       = foldbt (fun a l r ->(x=a)||l||r) false t
        let reflect             = foldbt (fun a l r -> br a r l)    lf 
        let preorder            = foldbt (fun x l r -> x::l @ r)    []
        let inorder             = foldbt (fun x l r -> l @ (x::r))  []
        let postorder           = foldbt (fun x l r -> l @ r @ [x]) []
        let preorder2 t         = 
            let rec preord t list   = match t with
                      Lf                -> list
                    | Br(a,l,r)         -> a::(preord l(preord r list)) in
            preord t []
        
        (* binary search tree *)
        let rec mem    bst x    = match bst with 
                  Lf                -> false
                | Br(a,l,r)         -> if x=a then true else 
                                       if x<a then mem l x else mem r x
        let rec add    bst x    = match bst with 
                  Lf                -> br x lf lf 
                | Br(a,l,r) as s when x=a   -> s
                | Br(a,l,r)     -> if x<a then br a(add l x)r else br a l(add r x)
        (* note !! if we use mem/add to normal int tree then it casuse bug !! *)


        let bt = Br(1,Br(2,Lf,Br(3,Lf,Lf)),Br(4,Br(5,Lf,Lf),Lf))

        (* rosetree *)
        (* is this good for foldt g h c d ? *)
        type 'a rtree           = RLf | RBr of 'a * 'a rtree list
        let     rlf             = RLf
        let     rbr a l         = RBr(a,l)
        let rec foldrt g h d c  = function
                  RLf               -> d
                | RBr(a,[])         -> g a d
                | RBr(a,l)          -> g a (foldr(h $(foldrt g h d c)) c l);;
        let     bt_rt           = 
            let g a l   = br (Some a) l lf                                  in
            let h       = function Lf -> br None lf | Br(a,l,_) -> br a l   in 
            foldrt g h Lf Lf 
        let rec purifybt        = function 
              Lf                    -> lf
            | Br(None,l,Lf)         -> purifybt l
            | Br(None,Lf,r)         -> purifybt r
            | Br(None,_,_)          -> raise Arg
            | Br(Some a,l,r)        -> br a(purifybt l)(purifybt r)
        let     btree_of_rtree  = purifybt $ bt_rt 
        let     rtree_of_btree  = foldbt (fun a l r -> rbr a [l;r]) rlf  

        let rt  = 
            RBr(1,[
                RBr(2,[RLf;RLf]);
                RBr(3,[
                    RBr(4,[]);
                    RBr(5,[RLf]);
                    RBr(6,[RLf;RLf]);
                    RLf;
                    RBr(7,[
                        RBr(8,[
                            RBr(9,[]);
                            RBr(10,[])]);
                        RBr(11,[])])]);
                RLf])



        (* xmltree *)
        (* 'a is tag type *)
        type ('a,'b) xml = XLf of 'b Types.maybe | XBr of 'a * ('a,'b)xml list
        let rec string_of_xml   = function
                  XLf Nothing       -> ""
                | XLf(Just s)       -> s
                | XBr(tag, l)       -> "<" ^tag^">\n" ^
                                       foldr (fun x ->(^)(string_of_xml x)) "" l ^
                                       "</"^tag^">\n";;
        let addressbook = 
            XBr ("addressbook", [
                XBr ("person",  [
                    XBr ("name", [XLf (Just "Atsushi Igarashi")]);
                    XBr ("tel",  [XLf (Just "075-123-4567")])]);
                XBr ("person", [XLf Nothing]);
                XBr ("person", [XLf Nothing])]);;

    end open Tree








module InfSeq = 
    struct
        type 'a seq                 = Cons of 'a * ('a->'a seq)
        let value_of_seq(Cons(i,f)) = i 
        let successor   (Cons(i,f)) = f i
        let nth_seq           n z   = foldn successor z n  
        let nth_value_of_seq  n     = value_of_seq $ nth_seq n 
        let rec fun_seq     s n z   = Cons(s n z,fun_seq s (n+1))
        let     succ_seq            = fun_seq (k succ) 0
        let     step                = fun_seq (+)
        (* fib is in another method, 
         * to define fib with fun_seq, There needs complex analysis *)
        let rec fib_seq       n m   = Cons(n, fib_seq(n+m))

    end open InfSeq



module Prime = 
    struct 
        open Int64
        let divisible     n d   = n mod d = 0
        let divisibleL    n d   = rem n d = 0L
        let rec div_from  d n   = if divisible n d  then d else div_from(1+d)n
        let rec divL_from d n   = if divisibleL n d then d else divL_from(add d 1L)n
        let first_divisor   n   = div_from 2 n
        let first_divisorL  n   = divL_from 2L n 
        let isprime         n   = n = first_divisor n 
        let isprimeL        n   = n = first_divisorL n
        let is_prime_table      = Hashtbl.create 1
        let is_primeL           = function
              0L -> false
            | 1L -> false
            | n  -> if Hashtbl.mem is_prime_table n 
                        then Hashtbl.find is_prime_table n
                        else begin
                            Hashtbl.add is_prime_table n (isprimeL n);
                            Hashtbl.find is_prime_table n end
        let is_prime n      = is_primeL (of_int n)

        let     print_bald list = foldr (fun x -> (^)(soi x^";")) "" list
        let     print_list l    = "[" ^ print_bald l ^ "]" 
        let rec prime_tbl m n   = if m>n then [] else 
                                    let next = prime_tbl (m+1) n in 
                                    if is_prime m then m::next else next
        let mk_prime_tbl m n    = 
            let tbl             = "prime_tbl_" ^ soi m ^ "_"   ^ soi n in 
            let fn              = "prime/"     ^ tbl   ^ ".ml" in
            let fd              = open_out fn in 
            let loop m n        = output_bytes fd (print_bald(prime_tbl m n)) in
            let size            = (n-m-1) / 10 in
            let m'              = (m-1) / size and n' = n / size in 
            let list            = rev (m' -- n') in
            let iter      x     = iterate (loop(1+x*size)((1+x)*size)) in 
            let ()              = output_bytes fd ("let " ^ tbl ^ " = [")       in
            let ()              = foldr iter () list                            in 
            let ()              = output_bytes fd "];;"                         in
            close_out fd;;  

        let db_size             = 100000
        let mk_prime_db m n     = 
            let iter x = iterate(mk_prime_tbl(1+x*db_size)((1+x)*db_size)) in 
            foldr iter () (rev((m-1)--(n-1)));; 

    end open Prime 

#use "prime.ml"


