(* initialize prime db 
 *
 * $ mkdir prime
 * $ ocaml
 * ocaml # #use "base.ml";;
 * ocaml # mk_prime_db 1 10;;
 *
 *)

#directory "prime"
#use "prime_tbl_1_100000.ml"
#use "prime_tbl_100001_200000.ml"
#use "prime_tbl_200001_300000.ml"
#use "prime_tbl_300001_400000.ml"
#use "prime_tbl_400001_500000.ml"
#use "prime_tbl_500001_600000.ml"
#use "prime_tbl_600001_700000.ml"
#use "prime_tbl_700001_800000.ml"
#use "prime_tbl_800001_900000.ml"
#use "prime_tbl_900001_1000000.ml"
#directory ".."


let p = concat [
    prime_tbl_1_100000;
    prime_tbl_100001_200000;
    prime_tbl_200001_300000;
    prime_tbl_300001_400000;
    prime_tbl_400001_500000;
    prime_tbl_500001_600000;
    prime_tbl_600001_700000;
    prime_tbl_700001_800000;
    prime_tbl_800001_900000;
    prime_tbl_900001_1000000]


let     print_bald_list l   = foldr(fun x->(^)(soi x^";")) "" l 
let     print_list l        = "[" ^ print_bald_list l ^ "]" 
let rec prime_tbl m n       = if m>n then [] else 
    if is_prime m then m::prime_tbl(m+1)n else prime_tbl (m+1) n ;;

let     mk_prime_tbl m n    = 
    let tbl                 = "prime_tbl_" ^ soi m ^ "_" ^ soi n                in 
    let fn                  = "prime/" ^ tbl ^ ".ml"                            in
    let fd                  = open_out fn                                       in 
    let loop m n fd         = output_bytes fd(print_bald_list(prime_tbl m n))   in
    let size                = (n-m-1)   /   10                                  in
    let m'                  = (m-1)     /   size                                in 
    let n'                  = n         /   size                                in 
    let iter                = rev (m' -- n') in
    let () = output_bytes fd ("let " ^ tbl ^ " = [") in
    let () = foldr(fun x xs->let()=loop(x*size+1)((x+1)*size)fd in xs)()iter in 
    let () = output_bytes fd "];;" in
    close_out fd;;  

let pdb_size = 100000
let mk_prime_db m n = foldl (fun xs x->
    let start = x*pdb_size+1 and target = (x+1)*pdb_size in 
    let () = mk_prime_tbl start target in xs) () (rev ((m-1)--(n-1)));; 

