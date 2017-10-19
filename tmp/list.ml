
let rec length          = function
      []        -> 0
    | _::x      -> 1 + length x;;
let rec append x y      = match x with 
      []        -> y
    | xa::xx    -> xa::(append xx y);;
let rec revAppend x y   = match x with
      []        -> y
    | xa::xx    -> revAppend xx (xa::y);;
let rev l               = revAppend l [];;

let rec map f           = function
      []        -> []
    | a::x      -> f a :: map f x;;
let rec forall p        = function
      []        -> true 
    | a::x      -> if p a then forall p x else false ;;
let rec exists p        = function
      []        -> false 
    | a::x      -> if p a then true else exists p x ;;
let rec foldr f i       = function
      []        -> i
    | a::x      -> f a (foldr f i x)
let rec foldl f i       = function 
      []        -> i
    | a::x      -> f (foldl f i x) a
let rec foldl' f i      = function
      []        -> i 
    | a::x      -> foldl' f (f i a) x;;

let rec nth n l             = match (n,l) with
      (1,a::_)          -> a
    | (_,_::x) when n>0 -> nth (n-1) x
let rec nth n l             = match (n,l) with 
      (n,_) when n<=0   -> None 
    | (1,a::_)          -> Some a
    | (_,_::x)          -> nth (n-1) x

(* when (is not pattern) *) 
let f = function 
      []                    -> 0
    | a::x when a>0         -> 1
    | a::x (*when a<=0*)    -> 2;; (* if comment out compiler throws warning *) 


(*e.g. concat*) let sample3 = [[1;2;3];[4;5];[6];[7;8;9;10]];;
let rec concat = function
      [] -> []
    | []::rest -> concat rest
    | (a::nrest) :: rest -> a::( concat (nrest::rest));;

let rec zip l r         = match (l,r) with
      ([],_)        -> []
    | (_,[])        -> []
    | (a::x,b::y)   -> (a,b)::(zip x y);;
let rec unzip           = function 
      []            -> ([],[])
    | (a,b)::rest   -> let (x,y) = unzip rest in ((a::x),(b::y));;

(********************)
(* association list *)
(********************)
let rec assoc key = function
    (k,v)::rest -> if key=k then v else assoc key rest;;

(* e.g. *) let city_phone = [
            ("Kyoto","075");
            ("Osaka","06");
            ("Tokyo","03") ];;

(********************)
(*     sorting      *)
(********************)

let nextrand seed = 
    let a = 16807.0 and m = 2147483647.0 in
    let t = a *. seed in
    t -. m *. floor(t /. m)
let rec randlist n seed tail = 
    if n = 0 
        then (seed, tail)
        else randlist (n-1) (nextrand seed) (seed::tail);;

let rec insert i = function
      []        -> [i]
    | a::x      -> if i<a then i::a::x else a::(insert i x);;
let rec insertion_sort = function
      []        -> []
    | a::x      -> insert a (insertion_sort x);; 

let rec quick_sort = function
      ([]|[_]) as s -> s
    | pivot::rest   -> (let rec partition left right = function
                              []   -> (quick_sort left)@(pivot::(quick_sort right))
                            | a::r -> if a<pivot
                                        then partition (a::left) right r 
                                        else partition left (a::right) r
                        in partition [] [] rest ) ;;


(* tools *)
let rec down2one = function
      0 -> []
    | n -> n :: (down2one (n-1));;

(* nested list *)
let rec nested_length = function
      [] -> 0
    | []::rest -> nested_length rest 
    | (a::nrest) :: rest  -> 1 + (nested_length (nrest::rest));;


(* --- verbose things --- *)
(* nested match needs () *)
let rec nth_foo n l = match n with 
      1 -> ( match l with a::_ -> a )
    | _ -> ( match l with _::x -> nth_foo (n-1) x);;
(* qsort 1
let rec partition pivot = function 
      []        -> ([], [])
    | a::rest   -> let (left,right) = partition pivot rest in 
                   if a<pivot then (a::left, right) else (left, a::right);;
let rec quick_sort = function
      [] -> []
    | a::rest -> let (left,right) = partition a rest in
                 (quick_sort left) @ ( a :: quick_sort right);; *)
(* qsort 2
let rec partition left right pivot = function
      []        -> (left,right)
    | a::rest   -> if a<pivot 
                        then partition (a::left) right pivot rest
                        else partition left (a::right) pivot rest;;
let rec quick_sort = function
      []            -> []
    | pivot::rest   -> let (left,right) = partition [] [] pivot rest in
                        left @ right;; *)

(* e.g. *)
let weeklist    = ["Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"]
let boollist    = true :: false :: []
let funlist     = [(fun x->x+1); (fun x->x*2)]
