
type 'a option = None | Some of 'a ;;

let rec find n = function
      [] -> None
    | a::rest when a=n -> Some a
    | _::rest -> find n rest;;


exception Zero_found ;;

let rec prod_list l = 
    let rec pl = function
      [] -> 1
    | a::_ when a=0 -> raise Zero_found
    | a::rest -> a * (prod_list rest)
    in 
    try pl l with Zero_found -> 0;;





let rec change coins amount = 
    match (coins, amount) with
      (_,0) -> []
    | ((c :: rest) as coins, total) ->
            if c > total 
            then change rest total
            else c :: change coins (total - c);;


let rec change coins amount = 
    match (coins, amount) with
      (_,0) -> []
    | ((c :: rest) as coins, total) ->
            if c > total 
            then change rest total
            else 
                (try 
                    c :: change coins (total - c) 
                with 
                    Failure "change" -> change rest total)
    | _ -> raise (Failure "change");;



let us_coins = [25;10;5;1]
let gb_coins = [50;20;10;5;2;1]


