

let foldl           = ListLabels.fold_left;;
let a               = foldl ~f:(+) ~init:0 [1;2;3;4];;
let b               = foldl ~init:0 ~f:(+) [1;2;3;4];;


type ('a,'b) c_h        = {c:'b->'a->'a; h:'a}
let rec foldl' {c=c;h=h} = function
      []                -> h
    | x::xs             -> foldl' {c=c;h=c x h} xs
let a           = foldl' {c=(+);h=0} [1;2;3;4];;
let b           = foldl' {h=0;c=(+)} [1;2;3;4];;



let g           = foldl ~f:(+) ;;
let g'          = foldl ~init:0 ;;
let a           = g 0 [1;2;3;4];;
let b           = g' (+) [1;2;3;4];;


open StdLabels   (* Labeled Array, List, and String *)
open ArrayLabels
open ListLabels
open StringLabels

open UnixLabels
open MoreLabels  (* Labeled Hashtbl, Map, and Set *)



let rec foldl ~c:c ~h:h = function
      []                -> h
    | x::xs             -> c x (foldl ~c:c ~h:h xs);; 
let rec foldl ~c ~h = function
      []                -> h
    | x::xs             -> c x (foldl ~c ~h xs);; 

let hoge ~a:x ~a:y  = x-y  (* ~~ let hoge x y = x-y *)
let foo ~x:y:int    = y+1
let bar ~x:x:int    = x+1
let bar ~(x:int)    = x+1


let k ~x ~y         = x
let k'              = k 1 2     (* not return 1 *)


let apply k x y     = k ~x ~y
let k               = apply k 
let k'              = k 1 2     (* return 1 *)



