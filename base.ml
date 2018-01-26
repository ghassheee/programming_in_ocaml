module Int = 
    struct
        let     even n      = if n mod 2=0 then true else false;;
        let     odd n       = not ( even n ) ;;
        let     abs n       = if n>0  then n else -n ;;
        let rec pow n m     = if m<=1 then n else n*(pow n(m-1)) ;;
        let rec fact n      = if n<0 
                    then raise (Invalid_argument "")
                    else if n=0 then 1 else n*fact(n-1);;
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
        let     abs     x       = if x > 0.0 then x else -.x;;
        let area_of_circle r    = if r > 0.0 then r*.r*.pi*.2.0 else 0.0;;
        let     square  x       = x*.x
        let rec pow     x n     = if n<=0 then 1.0 else x*.pow x (n-1);;
        let     cube    x       = pow x 3

        (* newton raphson method *)
        let derivation f x      = let dx=1e-10 in (f(x+.dx) -. f x)/.dx ;;
        let fixedpoint f init   = 
            let epsiron             = 1e-10 in
            let rec loop x          = let fx = f x in
                                      if abs(x-.fx)<epsiron 
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
            let rec loop(x,y)       = if abs(x-.y)<epsiron
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
        let rec repeat s n  = if n<0 
            then raise (Invalid_argument "") else match n with 
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
        let rec foldn s z n     = if n<=0 then z else foldn s z(n-1)

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
        let cons x xs       = x::xs
        let rec list f      = function
                []              -> []
              | x::xs           -> f x :: list f xs ;;
        let rec foldl f i   = function
                []              -> i
              | x::xs           -> f (foldl f i xs) x ;;
        let rec foldr f i   = function
                []              -> i
              | x::xs           -> f x (foldr f i xs) ;;
        let forall p        = foldr (fun x -> (&&)(p x)) true ;;
        let exists p        = foldr (fun x -> (||)(p x)) false ;;
        let length          = foldr (Polymorphic.k succ) 0 ;; 
        let append l m      = foldr cons m l        
        let snoc x xs       = append xs [x]
        let rev_append l m  = foldr snoc m l 
        let rev l           = rev_append l [];;
        let concat          = foldr append [];;
        let nth n l         = (* get nth element from list *) 
            let len = length l in 
            if (abs n)>= len then raise(Invalid_argument "") else
            let n'  = n mod len in
            let n'' = if n'<0 then n'+len else n' in
            let rec pure_nth n l = match (n,l) with
                (0,x::_ ) (* when cond *)   -> x
              | (n,x::xs)                   -> pure_nth(n-1)xs in
            pure_nth n'' l;;
        let (%) l n         = nth n l
        let rec zip l r     = match (l,r) with
                ([],_)          -> []
              | (_,[])          -> []
              | (x::xs,y::ys)   -> (x,y)::zip xs ys ;;
        let rec unzip       = function
                []              -> ([],[])
              | (x,y)::rest     -> let (xs,ys) = unzip rest in
                                   (x::xs,y::ys) ;;
        let     filter p    = foldr (fun x xs->if p x then x::xs else xs) [] 

        (* association list *)
        let rec assoc key = function
            (k,v)::_ when k=key -> v
          | _::rest             -> assoc key rest 
    (*    | _                   -> raise Match_failure "" *)

        let city_phone = [
            ("Kyoto","075");
            ("Osaka","06");
            ("Tokyo","03")] ;;

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
        let rec nat2list s z n  = if n<=0 then [] else z::nat2list s(s z)(n-1) ;;
        let     downto1 n       = nat2list pred n n  ;;



    end;;



module Tree =
    struct

    end
