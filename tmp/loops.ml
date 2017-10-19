
(* control structure *)
let () = print_string "Hello, " in print_string "World!\n";;

let f x y = 2 in 
f (print_string "Hello, ")(print_string "World\n");;
( print_string "Hello, " , print_string "World\n");;

(*
 * expr1; expr2; ... ; exprn 
 *)

let print_hello () = print_string "Hello, "; 0;;


(* if *)

(* if clause < ; 
 * if b then p == if b then p else ()  *)
let f1 b = if b then  print_string "hello\n"; print_string "world\n"
let f2 b = if b then (print_string "hello\n"; print_string "world\n")

(* while cond do; stmt;stmt; done *)
let fact n = 
    let i = ref 1 and res = ref 1 in
    while (!i <= n) do 
        res := !res * !i;
        i   := !i+1
    done;
    !res;;

(* while (stmt;cond) do; stmt;stmt; done *)
let parrot () =
    let s = ref "" in 
    while ( s := read_line (); !s <> "." ) do 
        print_string !s;
        print_endline !s
    done ;;

(* make my-while *) 
let rec whle cond body =
    if cond () 
        then (
            body ();
            whle cond body );;

let fact n = 
    let i = ref 1 and res = ref 1 in 
    whle(fun() -> !i<=n )
        (fun() -> res := !res* !i; i:= !i+1);
        !res;;

(* iter f [a;b;c;d] *)
let rec iter f = function
      []         -> ()
    | a::x       -> begin f a; iter f x end;;

let stations = 
    [
        "Tokyo";
        "Shinagawa";
        "Shin-Yokohama";
        "Nagoya";
        "Kyoto";
        "Shin-Osaka"
    ];;

let print_station = (fun s -> print_string "Station: "; print_endline s)

