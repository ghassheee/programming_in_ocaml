#use "base.ml"

        (* random list self-made *)
        let nextrand seed =
            let a = 16807.0 and m =2147483647.0 in 
            let t = a *. seed in
            t -. m *. floor(t/.m) ;;
        let rec randlist_of_igarashi n seed tail = if n=0 then (seed,tail)
            else randlist_of_igarashi(n-1)(nextrand seed)(seed::tail) ;;
        

        (* bmi *)
        let bmi name h w  =
            let bm        = w /. (square h) *. 10000.0 in
            let bmi_value = (float_of_int(int_of_float(100.0*.bm+.0.5)))/.100.0 in
            let shape     =
                if bmi_value < 18.0 then "thin" else
                    if bmi_value < 25.0 then "moderate" else
                        if bmi_value < 30.0 then "fat" else "fattest" in
            print_string ( name ^ "'s BMI is " ^ 
                            string_of_float bmi_value ^ ", " ^ shape ^ ".");;

        (* record e.g. *)
        type student            = {name: string; id: int}
        let string_of_student s = s.name ^ "'s ID is " ^ string_of_int s.id

        (* variant e.g. 2D functions*)
        (* 2D is very important !!              *)
        (* because the sight has 2D interface   *)

module Plane =
    struct
        type figure              = 
                  Point
                | Circle                of float
                | Rectangle             of float * float
                | Square                of float
        let area_of_figure          = function
                  Point                 -> 0.0
                | Circle    r           -> pi *. (r ^^ 2)
                | Rectangle (x,y)       -> abs_float (x *. y)
                | Square    x           -> x ^^ 2
        let farpoint_of_figure      = function
                  Point                 -> 0.0
                | Circle    r           -> abs_float r
                | Rectangle (x,y)       -> sqrt ((x^^2)+.(y^^2))
                | Square    x           -> (sqrt 2.0) *. (abs_float x) 
        let     similar     x y     = match (x,y) with
                  (Point, Point)     
                | (Circle _, Circle _)  
                | (Square _, Square _)              -> true
                | (Square _, Rectangle(x,y))     
                | (Rectangle(x,y), Square _)        -> x=y
                | (Rectangle(x,y), Rectangle(a,b))  -> x*.b-.y*.a=0.
                | _                                 -> false
        type 'a with_location       = {x: float; y: float; body: 'a}
        let rec overlap a b         = 
            let rec loop c d            = match (c,d) with
              (Point,Point)                     -> a.x=b.x && a.y=b.y
            | (Circle r,Circle s)               -> 
                    (s+.r)^^2 > ((a.x-.b.x)^^2)+.((a.y-.b.y)^^2)
            | (Rectangle(x,y),Rectangle(u,v))   ->
                    abs_float((x-.u)/.2.) > abs_float(a.x-.b.x) &&
                    abs_float((y-.v)/.2.) > abs_float(a.y-.b.y) 
            | (Rectangle(x,y),Circle r)         ->
                    let (cx,cy) = (a.x+.x/.2., a.y+.y/.2.) in (
                    ((b.x-.cx)^^2) +. ((b.y-.cy)^^2)  < r^^2         &&
                    abs_float(b.x-.a.x) > abs_float(cx-.a.x)    &&
                    abs_float(b.y-.a.y) > abs_float(cy-.a.y) )  ||
                    loop (Rectangle(x,y))(Rectangle(r,r))
            | (Circle r,Rectangle(x,y))         -> overlap b a
            | (Square x,o)                      -> loop(Rectangle(x,x))o 
            | (o,Square x)                      -> overlap b a
            | _ -> false
            in loop a.body b.body
    end
open Plane



        type color              = Black | Blue | Red | Magenta 
                                | Green | Cyan | Yellow | White ;;
        let reverse_color       = function
                  Black             -> White
                | Blue              -> Yellow
                | Red               -> Cyan
                | Magenta           -> Green
                | Green             -> Magenta
                | Cyan              -> Red
                | Yellow            -> Blue
                | White             -> Black



        type    even                = Zero | Succ_E of odd
            and odd                 = Succ_O of even
        let rec o_plus_e(Succ_O o)e = Succ_O(e_plus_e o e)
            and e_plus_e  e f       = match e with
                  Zero              -> f
                | Succ_E o          -> o_plus_e o f

