(* #load "nums.cma" *)
module MyNum = 
    struct
        open Num
        (* 2 ways of presentation of num *)
        (* approx_num_fix *)
        (* approx_num_exp *)
        let approx              = approx_num_fix 100 (* 100 digit string *) 
        let lt                  = lt_num (* <  *)
        let gt                  = gt_num (* >  *)
        let le                  = le_num (* <= *)
        let ge                  = ge_num (* >= *)

        (* newton raphson method *)
        let d                   = Int 1//Int max_int
        let derivation f x      = (f(x+/d)-/f x) // d ;;
        let rec limit  s z      = if le(abs_num(s z-/z))d then z else limit s(s z)
        let newton_step f x     = x -/ (f x)//(derivation f x)
        let newton_method f i   = limit (newton_step f) i
        let rec sqrt' a         = let e x = x*/x-/a in
            let nm = newton_method in
            if ge a(Int 100000000) then Int 100 */ (sqrt' (a//Int 10000)) else
            if ge a(Int 1000000)   then nm e(Int 10000)    else
            if ge a(Int 10000)     then nm e(Int 1000)     else
            if ge a(Int 100)       then nm e(Int 100)      else
            if le a(Int 0)         then failwith "nan"     else 
            nm e (Int 1)

        (* integral *)
        let integral f a b      = let n = (b-/a)//d in
            let rec loop k sum      = let sum' = sum +/(f(a+/k*/d))*/d in
            if ge k(n-/Int 1) then sum' else loop(k+/Int 1) sum' in
            ((f a)*/d+/Int 2*/(loop(Int 1)(Int 0))+/(f b)*/d) // Int 2

    end

