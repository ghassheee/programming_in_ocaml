exception Arg 

module Int = 
    struct
        let     even n      = if n mod 2=0 then true else false;;
        let     odd n       = not ( even n ) ;;
        let     abs n       = if n>0  then n else -n ;;
        let rec pow n m     = if m<=1 then n else n*(pow n(m-1)) ;;
        let ( ** )          = pow 
        let rec fact n      = if n<0 then raise Arg else 
                                if n=0 then 1 else n*fact(n-1);;
        let rec sum i n f   = if i=n then f i else f i + sum(i+1)n f;;
        let     square n    = n*n;;
        let     cube   n    = n*n*n;;
        let sum_of_square n = sum 1 n square;;
        let sum_of_cube   n = sum 1 n cube;;

        (* fibonacci *)
        open Int64
        let     table       = Hashtbl.create 1;;
        let rec fibo        = function
            0L -> 0L
          | 1L -> 1L 
          | n  -> if Hashtbl.mem table n
                    then Hashtbl.find table n
                    else begin 
                        Hashtbl.add table n(add(fibo(sub n 1L))(fibo(sub n 2L)));
                        Hashtbl.find table n end ;;
        let fib n = fibo (of_int n)
        
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


    end;;


module Float =
    struct
        let     pi              = 3.141592653589793;;
        let     e               = 2.718281828459045;;
        let     abs_float x     = if x > 0.0 then x else -.x;;
        let area_of_circle r    = if r > 0.0 then r*.r*.pi*.2.0 else 0.0;;
        let     square  x       = x*.x
        let     cube    x       = x*.x*.x
        let rec pow_float x n   = if n<=0 then 1.0 else x*.pow_float x (n-1);;
        let (^^)                = pow_float

        (* newton raphson method *)
        let derivation f x      = let dx=1e-10 in (f(x+.dx) -. f x)/.dx ;;
        let fixedpoint f init   = 
            let epsiron             = 1e-10 in
            let rec loop x          = let fx = f x in
                                      if abs_float(x-.fx)<epsiron 
                                        then x
                                        else loop fx in
            loop init;;
        let newton1step f x = x -. (f x)/.(derivation f x) ;;
        let newton_method f init = fixedpoint (newton1step f) init ;;
        
        let sqrt a          = newton_method (fun x->x*.x-.a) 1.0;;

        (* integral *)
        let integral f a b = 
            let dx= 1e-7 in
            let n = (b-.a)/.dx in
            let rec loop k sum = let sum' = sum +.(f(a+.k*.dx))*.dx in
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
                                        else loop (f(x,y)) in
            loop i;;
        let med_average x y = fix_average average (x,y) 
        
        (* use `#trace function` if evaluation does not end *) 

        (* arctan1 = 1 - 1/3 + 1/5 - 1/7 + ... + 1/(4k+1) - 1(4k+3) *)
        let arctan1 n = 
            let rec pos n = neg(n-1) +. 1.0/.(float_of_int(4*n+1)) 
            and     neg n = if n<0 then 0.0 else
                            pos n    -. 1.0/.(float_of_int(4*n+3)) in 
            pos n;;
    end;;


module String =
    struct
        let cat s t         = s ^ t
        let sand t s        = s ^ t ^ s
        let emphasize s     = sand s "_"
        let rec repeat s n  = if n<0 then raise Arg else match n with 
            0   -> ""
          | k   -> s ^ (repeat s (k-1));;

    end;;


module Polymorphic =
    struct
        let fst (a,b)           = a
        let snd (a,b)           = b
        let id  a               = a
        let apply f a           = f a
        let twice f a           = f (f a)
        let fourtimes x         = twice twice x
        let rec foldn s z n     = if n<=0 then z else foldn s (s z)(n-1)

        let curry f x y         = f (x,y)
        let uncurry f(x,y)      = f x y

        (* combinators are polymorphic *)
        let ($) f g x   = f (g x)  (* dot operator in Haskell *)
        let i x         = x
        let k x y       = x
        let s x y z     = x z (y z)
    end

module MyList =
    struct
        let cons x xs           = x::xs
        let rec foldl f i       = function
                []                  -> i
              | x::xs               -> f (foldl f i xs) x ;;
        let rec foldr f i       = function
                []                  -> i
              | x::xs               -> f x (foldr f i xs) ;;
        let listr  f            = foldr (fun x -> cons(f x)) []
        let forall p            = foldr (fun x -> (&&)(p x)) true ;;
        let exists p            = foldr (fun x -> (||)(p x)) false ;;
        let length l            = foldr (Polymorphic.k succ) 0 l ;; 
        let append l m          = foldr cons m l        
        let snoc x xs           = append xs [x]
        let rev_append l m      = foldr snoc m l 
        let rev l               = rev_append l [];;
        let concat              = foldr append [];;
        let rec elem n l        = match (n,l) with
                (n,l)when n<0||l=[] -> raise Arg
              | (0,x::_)            -> x
              | (n,_::xs)           -> elem (n-1) xs
        let (%) l n             = elem n l
        let rec zip l r         = match (l,r) with
                ([],_) | (_,[])     -> []
              | (x::xs,y::ys)       -> (x,y)::zip xs ys ;;
        let rec unzip       = function
                []                  -> ([],[])
              | (x,y)::rest         -> let (xs,ys) = unzip rest in
                                       (x::xs,y::ys) ;;
        let     filter p        = foldr (fun x xs->if p x then x::xs else xs) [] 
        let (|-)                = filter

        (* association list *)
        let rec assoc key = function
                (k,v)::_ when k=key -> v
              |  _    :: rest       -> assoc key rest 
              (* _                  -> raise Match_failure "" *)
        let city_phone = [("Kyoto","075");("Osaka","06");("Tokyo","03")] ;;

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
        let rec natlist m n     = if m>n  then [] else m::natlist (m+1) n
        let (--)                = natlist

        let rec product l m     = match (l,m) with
                  ([],_) | (_,[])   -> []
                | (x::xs,y::ys)     -> let f = fun y ys->(x,y)::ys in 
                                       (foldr f [] m) @ product xs m
        let rec selfpairlist        = function
                  []                -> []
                | (x::xs) as l      -> let f = fun y ys -> (x,y)::ys in
                                       (foldr f [] l) @ selfpairlist xs
        (* in Haskell 
         * > squares r = let s = sqrt r in
         *                  [(x,y)|x<-[1..s],y<-[1..s],x^2+y^2=r] 
         * They do not make big product space, so;  *)

        let rec product_with_filter p l m = match(l,m) with
              ([],_)|(_,[]) -> []
            | (x::xs,y::ys) -> let f y ys = if p(x,y) then (x,y)::ys else ys in
                               (foldr f [] m) @ product_with_filter p xs m
        let squares r           = 
            let s = truncate(sqrt(float_of_int r)) + 1 in
            product_with_filter (fun(x,y)->x*x+y*y=r)(1--s)(1--s) 

    end;;
open MyList

module Types = 
    struct
        type 'a maybe           = Nothing | Just of 'a
        type ('a,'b) either     = Left of 'a | Right of 'b
    end
open Types

module Tree =
    struct
        type 'a btree           = Lf | Br of 'a * 'a btree * 'a btree 
        let rec foldbt h c      = function
                  Lf                -> c
                | Br(a,l,r)         -> h a (foldbt h c l)(foldbt h c r)  
        let size                = foldbt (fun x l r -> 1 + l + r)   0
        let depth               = foldbt (fun x l r -> 1 + max l r) 0
        let elem      t x       = foldbt (fun a l r ->(x=a)||l||r) false t
        let preorder            = foldbt (fun x l r -> x::l @ r)    []
        let inorder             = foldbt (fun x l r -> l @ (x::r))  []
        let postorder           = foldbt (fun x l r -> l @ r @ [x]) []
        let preorder2 t         = 
            let rec preord t list   = match t with
                      Lf                -> list
                    | Br(a,l,r)         -> a::(preord l(preord r list)) in
            preord t []
        
        (* binary search tree (bst) *)
        let rec mem    bst x    = match bst with 
                  Lf                -> false
                | Br(a,l,r)         -> if x=a then true else 
                                       if x<a then mem l x else mem r x
        let rec add    bst x    = match bst with 
                  Lf                -> Br(x,Lf,Lf)
                | Br(a,l,r) as s when x=a   -> s
                | Br(a,l,r)     -> if x<a then Br(a,add l x,r)else Br(a,l,add r x)
        (* note !! if we use mem/add to normal int tree then it casuse bug !! *)


        let bt = Br(1,Br(2,Lf,Br(3,Lf,Lf)),Br(4,Br(5,Lf,Lf),Lf))

        (* rosetree *)
        (* is this good for foldt g h c d ? *)
        type 'a rtree       = RLf | RBr of 'a * 'a rtree list
        let rec foldrt g h d c = function
                  RLf               -> d
                | RBr(a,[])         -> g a d
                | RBr(a,l)          -> g a(foldr(fun x->h(foldrt g h d c x))c l);;
        let rtree_of_btree      = 


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


    end


open Types
open Int
open Float
open String
open Polymorphic
open MyList
open Tree
