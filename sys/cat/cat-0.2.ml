open Arg 

let ver         = "0.2"
let line_num    = ref false 
let files       = ref []

let print_ver () = Printf.printf "cat by OCaml version: %s\n" ver
let specs = [
    ("-n", Set  line_num,   "Display line number");
    ("-v", Unit print_ver,  "Display version"    )]

let rec print_file n fd = 
    let num = if !line_num then Printf.sprintf "%6d  " n else "" in
    let line = input_line fd in
    try 
        output_string stdout ( num ^ line ^ "\n" );
        print_file (n+1) fd
        with End_of_file -> ()

let _ = 
    Arg.parse specs (fun x -> files := x :: !files)
        "Usage: cat [-n] -[help] [-v] filename ...";
    List.iter 
        (fun f -> print_file 1 (open_in f)) (List.rev !files);;


