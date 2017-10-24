

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


let prime = concat [
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
