open Int64

exception Arg 

let soi = string_of_int
let sof = string_of_float
let iof = int_of_float
let foi = float_of_int


module Types = 
    struct
        type 'a option          = None | Some of 'a ;;
        type 'a maybe           = Nothing | Just of 'a
        type ('a,'b) either     = Left of 'a | Right of 'b
    end open Types

module MySys = 
    struct 
        let cmd                 = Sys.command
    end open MySys


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
        let fi b t f            = if b then t else f

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
        let     hashtbl     = Hashtbl.create 1;;
        let rec fibL        = function
            0L -> 0L
          | 1L -> 1L 
          | n  -> if Hashtbl.mem hashtbl n
                    then Hashtbl.find hashtbl n
                    else begin 
                        Hashtbl.add hashtbl n(add(fibL(sub n 1L))(fibL(sub n 2L)));
                        Hashtbl.find hashtbl n end ;;

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
        let rec sqrt' a         = 
            let e x = x*.x-.a in
            let nm  = newton_method in
            if a>1000000000000.     then 100. *. (sqrt' (a/.10000.)) else 
            if a>10000000000.       then nm e 1000000.      else
            if a>100000000.         then nm e 100000.       else
            if a>1000000.           then nm e 10000.        else 
            if a>10000.             then nm e 1000.         else
            if a>100.               then nm e 100.          else 
            if a<0.                 then nan                else 
                nm e 1. 

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


module MyString =
    struct
        let cat s t         = s ^ t
        let sand t s        = s ^ t ^ s
        let emphasize s     = sand s "_"
        let repeat s n      = foldn ((^)s) "" n
    end open MyString




module MyList =
    struct
        let cons x xs           = x::xs
        let nil                 = []
        let tail (x::xs)        = xs
        let head (x::xs)        = x
        let rec foldl f i       = function    []    -> i
                                            | x::xs -> f (foldl f i xs) x 
        let rec foldr f i       = function    []    -> i
                                            | x::xs -> f x (foldr f i xs) 
        let listr       f    l  = foldr (fun x -> cons(f x)) [] l
        let forall      p    l  = foldr (fun x -> (&&)(p x)) true l
        let exists      p    l  = foldr (fun x -> (||)(p x)) false l
        let length           l  = foldr (k succ) 0 l  
        let append      k    l  = foldr cons l k        
        let snoc x xs           = append xs [x]
        let rev_append  k    l  = foldr snoc l k 
        let rev              l  = rev_append l []
        let concat           l  = foldr append [] l
        let rec elem    n    l  = match (n,l) with
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
        let     filterr p    l  = foldr (filter p cons (k id)) [] l
        let     (|-)  x         = filterr x
        let iter_fun    x xs    = let ()=x in xs

        (* sorting *)
        let rec randlist max    = function
                n when n<1          -> []
              | n                   -> (Random.float max)::(randlist max (n-1));;
        let rec insert x        = function
                []                  -> [x]
              | y::ys when x<y      -> x::y::ys
              | y::ys               -> y::(insert x ys) ;;
        let insertion_sort    l = foldr insert [] l 
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
        let size             t  = foldbt (fun x l r -> 1 + l + r)   0 t
        let depth            t  = foldbt (fun x l r -> 1 + max l r) 0 t
        let elem      t x       = foldbt (fun a l r ->(x=a)||l||r) false t
        let reflect          t  = foldbt (fun a l r -> br a r l)    lf t
        let preorder         t  = foldbt (fun x l r -> x::l @ r)    [] t
        let inorder          t  = foldbt (fun x l r -> l @ (x::r))  [] t
        let postorder        t  = foldbt (fun x l r -> l @ r @ [x]) [] t
        let preorder2        t  = 
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
        let     bt_rt         t = 
            let g a l   = br (Some a) l lf                                  in
            let h       = function Lf -> br None lf | Br(a,l,_) -> br a l   in 
            foldrt g h Lf Lf t
        let rec purifybt        = function 
              Lf                    -> lf
            | Br(None,l,Lf)         -> purifybt l
            | Br(None,Lf,r)         -> purifybt r
            | Br(None,_,_)          -> raise Arg
            | Br(Some a,l,r)        -> br a(purifybt l)(purifybt r)
        let     btree_of_rtree t= purifybt ( bt_rt t )
        let     rtree_of_btree t= foldbt (fun a l r -> rbr a [l;r]) rlf t

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
        let of_seq     (Cons(i,_))  = i 
        let successor  (Cons(i,f))  = f i
        let nth_seq           n z   = foldn successor z n  
        let nth_of_seq        n     = of_seq $ nth_seq n 
        let rec fun_seq     s n z   = Cons(s n z,fun_seq s (n+1))
        let     succ_seq            = fun_seq (k succ) 0
        let     step                = fun_seq (+)
        (* fib is in another method, 
         * to define fib with fun_seq, There needs complex analysis *)
        let rec fib_seq       n m   = Cons(n, fib_seq(n+m))

    end open InfSeq




module MyQueue =
    struct
        type 'a reflist     = RNil | RCons of 'a * 'a reflist ref
        type 'a queue       = {mutable head: 'a reflist; mutable last: 'a reflist}
        
        let create ()       = {head = RNil; last = RNil}
        let append a        = let c = RCons(a,ref RNil) in function
              {head=RNil;last=RNil}as q -> q.head<-c; q.last<-c
            | {last=RCons(_,xs)} as q   -> xs:=c; q.last<-c  
            | _                         -> failwith "input queue broken"

        let push    a       = function
              {head=RNil;last=RNil}as q -> let c=RCons(a,ref RNil) in
                                           q.head<-c;q.last<-c 
            | {head=b}as q              -> let c=RCons(a,ref b) in q.head<-c
            | _                         -> failwith "input queue broken"

        let peek            = function
              {head=RNil;last=RNil}     -> failwith "empty queue"
            | {head=RCons(a,_)}         -> a
            | _                         -> failwith "queue's head broken"        

        let pop             = function
              {head=RNil;last=RNil}     -> failwith "empty queue"
            | {head=RCons(x,xs)}as q when !xs=RNil 
                                        -> q.head<-RNil; q.last<-RNil; x
            | {head=RCons(x,xs)}as q    -> q.head<- !xs; x 
            | _                         -> failwith "queue's head broken"        
        (* let q               = create () *)
    end open MyQueue


