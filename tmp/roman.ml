
#use "string.ml";;

(* Roman Number *)
let rec roman l n = match (l,n) with
      (_, 0)                    -> ""
    | ((num,roma)::rest, _ )    -> 
            let (q,r) = (n/num, n mod num) in
            (repeat roma q) ^ (roman rest r);;
let list2roman = [                          (1000,"M");
        (900,"CM"); (500,"D"); (400,"CD");  (100,"C");
        (90,"XC");  (50,"L");  (40,"XL");   (10,"X");
        (9,"IX");   (5,"V");   (4,"IV");    (1,"I");  
      ];;
let int2roman = roman list2roman 








