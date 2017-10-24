open Sys 
open Array

let _ = 
    if length argv = 3 
    then rename argv.(1) argv.(2)
