let rec max_list        = function
    | []                    -> 0
    | x::xs                 -> let m = max_list xs in 
                               if x>m then x else m;;
let rec make_list n f   = 
    if n=0 then [] else f() :: make_list (n-1) f;;

let rec map3 f l m n    = match (l,m,n) with 
    | ([],[],[])            -> []
    | (x::xs,y::ys,z::zs)   -> f x y z :: map3 f xs ys zs
    | _                     -> raise (Invalid_argument "map3")

let rec iter3 f l m n   = match (l,m,n) with 
    | ([],[],[])            -> ()
    | (x::xs,y::ys,z::zs)   -> f x y z; iter3 f xs ys zs
    | _                     -> raise (Invalid_argument "iter3")
