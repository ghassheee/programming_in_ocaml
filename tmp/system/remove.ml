open Scanf
open Sys
open Array

let _ = 
    for i = 1 to length argv - 1 
    do 
        let f = argv.(i) in
        if file_exists f 
        then
            let _ = print_endline ( "really remove " ^ f ^ "?" ) in 
            let s = "" in 
            let _ = bscanf Scanning.stdin "%s" (fun x -> s = x) in
            remove f
    done



