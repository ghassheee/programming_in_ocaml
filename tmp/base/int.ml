#use "core.ml";;

(* int *)
let abs x = if x > 0 then x else -x;;
let rec pow x n = if n<=1 then x else x * (pow x (n-1))

let rec fact n  = if n<1 then 1 else n*fact(n-1) 
let     fact n  = let rec tailfact (n,res) =  (* tail recursve *)
                    if n = 1 then res else tailfact(n-1, n*res) in 
                  tailfact(n,1);;
let rec fact n  = if n < 0 (* with exception *) 
                    then raise (Invalid_argument "fact: negative argument")
                    else if n = 0 then 1 else n * fact (n-1) ;;

(* higher order function *)
let rec sum_of f n = if n=0 then 0 else f n + sum_of f(n-1);;
let square x = x*x ;; 
let cube x = x*x*x ;; 
let sum_of_square   = sum_of square;;
let sum_of_cube     = sum_of cube;;


