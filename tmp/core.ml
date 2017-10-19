type 'a option = None | Some of 'a 

let id x = x
let ($) f g  = fun x -> f ( g x )   (* Haskell's dot operator *)
let twice f x = f ( f x )
let rec repeat f n x = if n>0 then repeat f(n-1)(f x) else x;;

let curry f     = fun x y   -> f (x,y)
let uncurry f   = fun (x,y) -> f x y



(* char *)
let capitalize c = 
    let int = int_of_char c in
    if int<123 && int>96 
        then char_of_int (int - 32) 
        else char_of_int int

(* num *)
let fib n = let (c_n,_) = repeat (fun(x,y)->(x+y,x)) n (1,1) in c_n
   



