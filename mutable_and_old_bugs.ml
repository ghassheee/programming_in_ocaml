module Mutables     =
    struct  
        (* Here, 3 mutable (rewritable) data types *)
        (*
         * 1. string
         * 2. mutable record
         * 3. reference         
         * 4. array 
         *
         *)

        (* 1. string *)
        let str = "like";; 
        (* str.[2] <- 'k' *)     (* forbidden in 2017 *)
        Bytes.set str 2 'k'  ;;  (* Byte.set string int char *)

        let p           = "life"
        let pair1       = ("life",p)
        let pair2       = (p,p) ;;
        (pair1= pair2, fst pair1= fst pair2, snd pair1= snd pair2);; (* TTT *)
        (pair1==pair2, fst pair1==fst pair2, snd pair1==snd pair2);; (* FFT *)
        (*    equality     *)
        (*      =       :    same value      *)
        (*      ==      :    same instance   *)

        (* 2. mutable record *)
        type teacher    = {name:string; mutable office:string}
        let     t       = {name="Igarashi"; office="140"}
        (* t.office <- "142"  *) (* forbidden in 2017 *)

        (* reference *)
        let p = ref 5 and q = ref 2;;
        (!p, !q);;
        p := !p + !q ;;
        (!p, !q);;

        let reflist = [p;q;p];;
        p := 100;;
        reflist;;
        
        (* double reference *)
        let p = ref 5 and q = ref 2;;
        let refp = ref p and  refq = ref q;;
        !refq := !(!refp);;
        (!p, !q);;

        (* array *)
        let arr = [| 1;2 |];;
        let [| b;c |] = arr;;
        arr.(1) <- 9;;
        arr.(1) ;;

        (* polymorphism & editable data *)
        let x = ref [] ;;
        (*(1:: !x, true:: !x) ;;*)  (* forbidden in 2017 *)

        let (get,set) = let r = (ref []) in ((fun()-> !r),(fun x->r:=x)) ;;
        1 :: get ()         ;;
        (* "abc" :: get ()  ;; *)   (* forbidden in 2017 *)
        (* set "abc"        ;; *)   (* forbidden in 2017 *)
        1 :: get ()         ;;

        let g x = ((fun y->(x,y)) 4, x+1) ;;
    end open Mutables
