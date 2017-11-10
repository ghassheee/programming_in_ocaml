open Sys

let _ = 
    if Array.length argv = 2
    then 
        print_endline (getenv argv.(1))
