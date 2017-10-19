#use "list.ml";;
#use "tree.ml";;

let rec find x              = function
     []                 -> 0
    | a::l when a=x     -> 1
    | _::l              -> 1 + find x l ;;


let f x = if raise Division_by_zero then raise Division_by_zero else x;;

let rec find x = function
      [] -> raise Not_found 
    | a::l when a=x -> 1
    | _::l -> 1 + find x l ;;

(* try 
try find 7 [0;8;7;3] with Not_found -> 0;;

try find 9 [0;8;7;3/0] with 
      Not_found -> 0
    | Division_by_zero -> -1;; *)
let find x li =
    let rec find' s  = function 
          [] -> raise Not_found
        | a::l when a=s -> 1
        | _::l -> 1 + find s l
    in  
    try find' x li with Not_found -> 0;;



let map_sqrt l =
    let sqrt' x = if x<0.0 then raise (Invalid_argument "sqrt'")
    else sqrt x
    in try Some (map sqrt' l ) with Invalid_argument "sqrt'" -> None;;





exception Foo;;
exception Bar of int;;
exception Hoge of string;;

let exnlist = [Hoge "hello"; Bar 0; Foo]
let f = function
      Foo -> 0
    | x -> raise x;;



(*
 * exn
 *
 * * constructor
 *
 * Invalid_Argument
 * Failure
 *
 * Not_found
 *
 * End_of_file 
 *
 * * function
 * 
 * invalid_arg
 * failwith
 *)

