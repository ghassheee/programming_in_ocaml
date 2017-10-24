




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




