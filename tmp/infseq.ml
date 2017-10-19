type intseq = Cons of int*(int->intseq);;
let rec step n x = Cons(x+n, step (n));;
let rec nthseq n seq = 
    match (n,seq) with
          (0,Cons(x,_)) -> x
        | (_,Cons(x,f)) -> nthseq (n-1) (f x);; 

(*   intseq := μ X. cons:{Nat,Nat->X}   *)

(*
let rec step n x = Cons(x+n, step (n+1));;
let rec nthseq n (Cons(x,f)) = if n = 1 then x else nthseq (n-1) (f x);;
let is_prime x = x 
*)


type 'a seq = Cons of 'a*(unit->'a seq) ;;
let rec from n = Cons (n, fun () -> from (n+1));;
let Cons(x,f) = from 1;;
let Cons(y,g) = f();;
let Cons(z,h) = g();;
let rec mapseq f (Cons(x,tail)) = 
    Cons(f x, fun() -> mapseq f (tail()));;
let reciprocals = mapseq (fun x -> 1.0 /. float_of_int x) (from 2);;
let rec take n s = match(n,s)with
          (0,_)         -> []
        | (n,Cons(x,f)) -> x::(take(n-1)(f()));;

