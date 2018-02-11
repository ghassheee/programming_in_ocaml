(* more detail see Reference Manual Chap. on website 3 *)



class calc =
    object
        val mutable num     = 0
        val mutable func    = fun x -> x
        method input n      = num <- n 
        method plus         = let x = num in
                              let f = func in
                              func <- (fun y -> (f x) + y)
        method eq           = func num
        method ac           = num <- 0;
                              func <- (fun x -> x )
    end

(* e.g. *)

let c = new calc
let _ = c#input 2; c#plus; c#input 4; c#eq;     (* This returns 6 *)




type base           = Oct | Dec | Hex
let string_of       = function
      Oct               -> Printf.sprintf "%o"
    | Dec               -> Printf.sprintf "%d"
    | Hex               -> Printf.sprintf "%x"


class calc_base b   = 
    object
        val mutable num     = 0
        val mutable func    = fun x -> x
        method input n      = num <- n 
        method plus         = let x = num in
                              let f = func in
                              func <- (fun y -> (f x) + y)
        method ac           = num <- 0;
                              func <- (fun x -> x )
        method eq = string_of b (func num)
    end

let c = new calc_base Oct
let _ = c#input 8; c#plus; c#input 4; c#eq     (* This returns 14 *)
let c = new calc_base Dec
let _ = c#input 8; c#plus; c#input 4; c#eq     (* This returns 12 *)
let c = new calc_base Hex
let _ = c#input 8; c#plus; c#input 4; c#eq     (* This returns c  *)


(* syntax *)
class classname arg = 
    let hoge = () in 
    object
        val arg = arg
        method hoge = hoge
    end

(* self *)
class calc_double =
    object (self)
        inherit calc
        method double       = self#plus; self#eq;
    end;;

let d = new calc_double;; 
let _ = d#input 13; d#double;;



class calc_with_buttons = 
    object(self)
        val mutable num     = 0
        val mutable func    = fun x->x
        method private shift n      = num <- num*10 + n
        method zero         = self#shift 0
        method one          = self#shift 1
        method two          = self#shift 2
        method three        = self#shift 3
        method four         = self#shift 4
        method five         = self#shift 5
        method six          = self#shift 6
        method seven        = self#shift 7
        method eight        = self#shift 8  
        method nine         = self#shift 9
        method plus         = let x = num in 
                              let f = func in
                                num <- 0;
                                func <- (fun y -> (f x)+ y)
        method eq           = let r = func num in 
                                num <- 0;
                                func <- (fun x->x);
                                r
    end;;

let c = new calc_with_buttons
let _ = c#one; c#two; c#plus; c#seven; c#plus; c#two; c#three; c#eq;;
let _ = c#one; c#plus; c#eq;;



(* initializer *)
class demo_calc n m =
    object (self)
        inherit calc
        initializer
            self#input n; self#plus; self#input m;
            Printf.printf "%d + %d = %d\n" n m (self#eq)
    end;;

let d = new demo_calc 10 234;;


(* inheritance *)
class calc_minus =
    object
        inherit calc
        method minus        = let x = num in 
                              let f = func in 
                              func <- (fun y -> (f x) - y)
    end;;
let c = new calc_minus
let _ = c#input 13; c#minus; c#input 98; c#eq;;


(* super *)
class calc_kids =
    object
        inherit calc_minus as super 
        method eq           = max (super#eq) 0
    end;;

let k = new calc_kids
let _ = k#input 4; k#minus; k#input 100; k#eq;;


(* multiple inheritance *)
class multiple_inherit = 
    object
        inherit calc as super1
        inherit calc_minus as super2
        inherit calc_kids as super3
    end;;



(* type of object *)
(* we do not always need class in order to define an object *)
let calc_obj = 
    object
        val mutable num     = 0
        val mutable func    = fun x -> x
        method input n      = num <- n
        method plus         = let x = num in
                              let f = func in
                              func <- (fun y -> (f y)+x)
        method eq           = func num
        method ac           = num <- 0 ; func <- (fun x -> x)
    end;;

let objlist     = [calc_obj; new calc]  (* these are the same type *)



(* subtype *)
(* :> denotes coersion *)
(* 
 * OCaml's coersion ~~ Java's UpCast 
 * None             ~~ Java's DownCast 
 *)
let objlist     = [new calc; (new calc_double :> calc); (new calc_minus :> calc)]


(* polymorphic object type *)
(*
 * what is the type of test ?  
 * test : < eq:int; input:int->a'; plus:'b; .. >
 * where '..' denotes etc.
 * formally,
 * '..' denotes 'row polymorphism'
 *)
let test c              = c#input 10; c#plus; c#input 20; c#eq = 30 ;;
let _                   = test (new calc) && test (new calc_kids) ;;


(* calc  : type of calc
 * #calc : type of calc with row polymorphism *)
let input_ten (c:#calc) = c#input 10
let _                   = input_ten (new calc_kids);;


(* f : bool -> <input:int->'a; plus:'a> *)
let input_ten c         = c#input 10 
and push_plus c         = c#plus
let f b                 = if b then input_ten else push_plus;;

(* g : bool -> <input:string->'a; input:int->'b> 
 * Thus the type of input is conflict
 *)
let input_foo c = c#input "foo"
(* let g b = if b then input_ten else input_foo *)


(* But we cannot use row annotation as type definition *)
(* type t = <m:int ..> *)
(* Thus <m:int ..> is called 'Type Variable' *)






(* virtual method *)
(* virtual class  *)
class virtual abstract_calc_demo n m op =
    object(s)
        val mutable num     = 0
        val mutable func    = fun x->x
        method input n      = num <- n
        method virtual op : unit
        method eq           = func num
        initializer
            s#input n; s#op; s#input m;
            Printf.printf "%d %s %d = %d\n" n op m s#eq
    end;;

class calc_demo_plus n m = 
    object
        inherit abstract_calc_demo n m "+"
        method op = let x = num in
                    let f = func in 
                    func <- (fun y -> (f x) + y)
    end;;




(* inherit private method 'shift' *)
class calc_with_buttons_00 = 
    object
        inherit calc_with_buttons as super 
        method zerozero     = super#shift 0; super#shift 0
    end;;



(* hide private method completely with type annotation *)
class calc_counter 
    : object 
        inherit calc
        method get  : int
    end
    = object (s)
        inherit calc as super 
        val mutable count   = 0
        method get          = count
        method private incr = count <- count+1
        method eq           = s#incr; super#eq (* count how much eq is called *)
    end;;

(* syntax of type annotation

object
    inherit name
    val [mutable] name : type 
    method [private] [virtual] name : type 
end

*)


class virtual foo 
    : object
        val a : int     (* hides a is mutable *)
        method virtual m : int -> int 
    end
    = object
        val mutable a = 0
        method m  x = 100 + x
    end;;


(* class type *)
class type calc_counter_t =
    object 
        inherit calc
        method get : int
    end;;


class calc_counter : calc_counter_t =
    object (s)
        inherit calc as super 
        val mutable c       = 0
        method get          = c
        method private incr = c <- c+1
        method eq           = s#incr; super#eq
    end;;


(* polymorphic class *)
let generic_calc init f = 
    object
        val mutable num     = init
        val mutable func    = fun x -> x 
        method input n      = num <- n
        method op           = let x = num in
                              let f' = func in 
                              func <- (fun y -> f (f' x) y)
        method eq           = func num
    end;;

let c = generic_calc 0.0 ( *. )
let _ = c#input 2.0; c#op; c#input 3.14; c#op; c#input 2.0; c#eq;; 

let s = generic_calc "" (^)
let _ = s#input "Hello, "; s#op; s#input "world!"; s#eq;;


class ['a] generic_calc init f =
    object
        val mutable num     = (init : 'a)
        val mutable func    = fun x->x
        method input n      = num <- n
        method op           = let x = num in
                              let f' = func in 
                              func <- (fun y -> f (f' x) y)
        method eq           = func num
    end;;

class virtual ['a] abstract_generic_calc init =
    object
        val mutable num     = (init : 'a)
        val mutable func    = fun x->x
        method input n      = num <- n
        method virtual op : unit
        method eq           = func num
    end;;

class fcalc =
    object
        inherit [float] generic_calc 0.0 ( *. )
    end;;

let f = new fcalc
let _ = f#input 2.0; f#op; f#input 3.14; f#eq;;



(* polymorphic method *)
class ['a] olist init 
    = object (s:<foldl:'b.('b->'a->'b)->'b->'b; .. >)
        val mutable l       = (init : 'a list)
        method cons a       = l <- a::l
        method length       = List.length l
        method append m     = l <- List.append l m 
        method foldl f e    = List.fold_left f e l 
        method foldr : 'b.('a->'b->'b)->'b->'b  (* define the type *)
                            = fun f e -> List.fold_right f l e
    end;;

let ol                  = new olist [1];;
let _                   = ol#cons 3; ol#length;;
let _                   = ol#foldr(+)10;;
let _                   = ol#foldl(fun x y -> x^" "^string_of_int y)"List has";;


(* multiple type variable *)
class ['a, 'b] hoge a b =
    object
        val a = (a : 'a)
        val b = (b : 'b)
    end;;










