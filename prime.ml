#use "base.ml"

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

module Prime = 
    struct 
        (* Int64 *)
        let divisibleL    n d   = rem n d = 0L
        let rec first_divisor_fromL d n   = 
            if divisibleL n d then d else first_divisor_fromL(Int64.add d 1L)n
        let isprimeL        n   = n = first_divisor_fromL 2L n
        let is_prime_table      = Hashtbl.create 1
        let is_primeL           = function
              0L -> false
            | 1L -> false
            | n  -> if Hashtbl.mem is_prime_table n 
                        then Hashtbl.find is_prime_table n
                        else(Hashtbl.add  is_prime_table n (isprimeL n);
                             Hashtbl.find is_prime_table n);;
        let primetable         = Hashtbl.create 1
        let add_prime           = function
            (n, p)            -> if Hashtbl.mem primetable n
                                then () else Hashtbl.add primetable n p
        let primetable_of_prime = foldl 



        (* Int *)
        let     divisible n d   = n mod d = 0
        let rec div_from  d n   = if divisible n d  then d else div_from(1+d)n
        let     threshold       = iof $ floor $ sqrt $ foi $ succ
        let rec find_prime p(x::xs) = if p=x then x::xs else find_prime p xs
        let     next_prime      = function
                  []                -> id [] (* implement here *)
                | l                 -> tail l
        let first_divisor_from pl n =  
            let rec loop pl n = 
                let p = head pl in 
                if p>threshold n then n else 
                if divisible n p then p else 
                loop (next_prime pl) n in loop pl n
        let first_divisor   n   = first_divisor_from prime n
        let isprime         n   = n = first_divisor n   
        let is_prime n          = is_primeL (of_int n)

        let     print_bald l    = foldr (fun x -> (^)(soi x^";")) "" l
        let     print_list l    = "[" ^ print_bald l ^ "]" 
        let rec prime_tbl m n   = if m>n then [] else 
                                    let next = prime_tbl (m+1) n in 
                                    if is_prime m then m::next else next ;;

        let savefile_of_list name l = 
            let fn              = name ^ ".ml" in
            let fd              = open_out fn in
            let ()              = output_bytes fd ("let "^name^" = ") in 
            let ()              = output_bytes fd ((print_list l)^";;") in 
            close_out fd;;



        let mk_prime_tbl m n    = 
            let tbl             = "prime_tbl_" ^ soi m ^ "_"   ^ soi n in 
            let fn              = "prime/"     ^ tbl   ^ ".ml" in
            let fd              = open_out fn in 
            let loop i j        = output_bytes fd (print_bald(prime_tbl i j)) in
            let size            = (n+1-m) / 10 in
            let m'              = (m-1) / size in 
            let n'              = n     / size - 1 in 
            let l               = rev (m' -- n') in
            let iter      x     = iter_fun (loop(1+x*size)((1+x)*size)) in 
            let ()              = output_bytes fd ("let " ^ tbl ^ " = [")       in
            let ()              = foldr iter () l                               in 
            let ()              = output_bytes fd "];;"                         in
            close_out fd;;  

        let db_size             = 100000
        let mk_prime_db m n     = 
            let iter x = iter_fun(mk_prime_tbl(1+x*db_size)((1+x)*db_size)) in 
            foldr iter () (rev((m-1)--(n-1)));; 

    end open Prime 

