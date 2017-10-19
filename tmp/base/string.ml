


let strcat x y  = x ^ y ;;  (* string catenation *)
let sand x      = (fun y -> x^y^x );;
let emphasize   = sand "_";;


let rec repeat x        = function
      0 -> ""
    | n -> x ^ (repeat x (n-1))
