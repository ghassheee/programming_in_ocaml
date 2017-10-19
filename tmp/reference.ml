

let str = "life" ;;
str.[2] <- 'k';;

let pair = let p="life" in (p,p) ;;
Bytes.set (fst pair) 2 'k';;

(*       Equality       *)
(* =  : same value      *)
(* == : same instance   *)

(* mutable record *)
type teacher = {name : string ; mutable office :string};;
let t = {name = "Igarashi"; office= "140"};;
t.office <- "142";;

(* reference *)
let p = ref 5 
and q = ref 2;;
(!p, !q);;
p := !p + !q ;;
(!p, !q);;

let reflist = [p;q;p];;
p := 100;;
reflist;;

let p = ref 5 and q = ref 2;;
let refp = ref p and  refq = ref q;;
!refq := !(!refp);;
(!p, !q);;

(* array *)
let arr = [| 1;2 |];;
let [| b;c |] = arr;;

(* polymorphism & editable data *)
let x = ref [];;

let (get,set) = 
    let r = ref [] in 
    ((fun () -> !r ) , (fun x -> r:=x));;

let g = fun x -> (( let f = fun y -> (x,y) in f 4 ), x + 1);;

