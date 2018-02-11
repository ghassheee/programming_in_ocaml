(* optional arg should not be the last arg *) 
let rec seq from ?step:(s=1) n =
    if n<=0 then [] else from :: seq(from+s) ~step:s (n-1);;

let _   = seq 1 10;;        (* when 10 is given, optional is erased *)
let _   = seq 1 10 ~step:100;;
let _   = seq ~step:5 1 10;;

(*this optional arg cannot be erased *)
let rec seq from n ?step:(s=1) = 
    if n<=0 then [] else from :: seq(from+s) ~step:s (n-1);;

let _   = seq 1 10;;        
let _   = seq 1 10 ~step:100;;
let _   = seq ~step:5 1 10;;

(* this optional arg cannot be erased *)
let f ?(x=1) ~y = x + y
let _           = f ~y:3

(* optional argument is implemented via 
 *  type 'a option = None | Some of 'a 
 *)
let rec seq from ?step n = match step with 
      None                  -> if n<=0 then [] else from::seq(from+1)(n-1)
    | Some s                -> if n<=0 then [] else from::seq(from+s)~step:s(n-1)
let rec seq from ?step n = 
    let s = match step with 
      None                  -> 1
    | Some s                -> s in
    if n<=0 then [] else from :: seq (from+s) ~step:s (n-1)
let _                   = seq 1 10
let _                   = seq 1 10 ~step:10





(* let test f      = f 10 ~step:2 100;; *)
(* let _           = test seq *)  
let test (f:int -> ?step:int -> int -> 'a list) = f 10 ~step:2 100;;
let _           = test seq  
let hoge a ~step c = step
let _           = test hoge 
