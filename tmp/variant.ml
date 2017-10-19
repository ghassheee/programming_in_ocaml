




let area_of_figure2 = function
      ("point", _, _) -> 0
    | ("circle", r, _) -> r*r*3
    | ("rectangle", x, y) -> x*y
    | ("square", x, _) -> x*x;;
(* pattern matching is not exhaustive *)



type figure =
      Point 
    | Circle of int
    | Rectangle of int * int 
    | Square of int 


let area_of_figure = function
      Point -> 0
    | Circle r -> r*r*3
    | Rectangle (x,y) -> x*y
    | Square x -> x*x;;



type location = Coodinate of int * int ;;


let similar x y = match (x, y) with
    (Point,Point)|(Circle _,         Circle _        )
    |(Square _,         Square _        ) -> true           
    |(Rectangle (a,b),  Square _        )    
    |(Square _,         Rectangle (a,b) ) -> a = b
    |(Rectangle (a,b),  Rectangle (c,d) )           -> (b*c-a*d) = 0
    | _                                             -> false;;

type 'a with_location = Located of location * 'a;;


let overlap fl1 fl2 = 
    match (fl1, fl2) with
     (Located(p1,Point),Located(p2,Point)) -> p1 = p2;;

type color = 
      Black | Blue | Red | Magenta
    | Green | Cyan | Yellow | White ;;

let reverse_color = function
      Black -> White | Blue -> Yellow 
    | Red -> Cyan    | Magenta -> Green
    | Green -> Magenta | Cyan -> Red 
    | Yellow -> Blue | White -> Black;;

(*
 * type bool = true | false
 *
 * if e1 then 
*)



type nat = Zero | Succ of nat;;

let zero = Zero and two = Succ (Succ Zero);;

let rec add m n =
    match m with 
      Zero -> n 
    | Succ m' -> Succ (add m' n);;

type intlist = INil | ICons of int * intlist;;

type even = Zero | SuccE of odd and odd = SuccO of even;;

let rec o_plus_e (SuccO o) e = SuccO (e_plus_e o e)
    and e_plus_e e1 e2 =
        match e1 with 
          Zero -> e2 
        | SuccE o -> SuccE (o_plus_e o e2);;




type 'a mylist = Nil | Cons of 'a * 'a mylist;;

type 'a with_location = {loc_x: float; loc_y: float; body: 'a};;

type ('a,'b) list_with_tail = Nil of 'b | Cons of 'a * ('a,'b) list_with_tail;;

type 'a option = None | Some of 'a;;

let fact n =
    let rec fact' n = 
        if n = 0 then 1 else n * fact'(n-1) 
    in
    if n < 0 then None else Some (fact' n);;


type ('a, 'b) sum = Left of 'a | Right of 'b;;

