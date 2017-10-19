
(* float *)
let pi = 3.141592653589793;;
let e  = 2.718281828459045;;
let abs_f x = if x > 0.0 then x else -.x;;
let area_of_circle r =
    if r > 0.0 then r else 0.0;;

(* DIFFERENTIAL *)
let deriv f     = let dx = 1e-10 in 
                  fun x -> (f(x+.dx)-.f(x))/.(dx);;
let fixedpoint f init =
        let epsiron = 1e-10 in 
        let rec loop x = let fx = f x in
                         if abs_f(x-.fx)<epsiron then x else loop fx
        in loop init ;;
let newton_transform f = fun x -> x -. f(x) /. (deriv f x) ;;
let newton_method f guess = fixedpoint (newton_transform f) guess;;

(* INTEGRAL *)
let bad_integral f a b =    (* stack overflow *)
    let dx  = 0.1e-10 in 
    let n   = (b-.a)/.(dx) in
    let rec loop k = let k' = k+.1.0 in
                     if k'>n then 0.0 else (f(a+.k*.dx))+.(loop k') in 
    ((f a)+.2.0*.(loop 1.0)+.(f b))/.(2.0*.dx)

let integral f a b =        (* tail recursive *)
    let dx  = 0.1e-7 in
    let n   = (b -. a) /. dx in
    let rec loop k sum = let k' = k +. 1.0 in
                         let sum' = sum +. (f (a +. k*.dx)) in 
                         if k'>n then sum' else loop k' sum' in 
    ((f a) +. 2.0*.(loop 1.0 0.0) +. (f b)) /. 2.0 *. dx

