open Arg 
open Printf

let ver             = "0.2"
let line_num        = ref false 
let files           = ref []

let print_ver ()    = printf "cat by OCaml version: %s\n" ver
let specs           = [ ("-n", Set  line_num,  "Display line No.");
                        ("-v", Unit print_ver, "Display version" )]

(* print_file lineNo. filediscriptor *) 
let rec print_file n fd = 
    let num = if !line_num then sprintf "%6d  " n else "" in
    let line = input_line fd in
    try output line stdout 
    try 
        output_string stdout ( num ^ line ^ "\n" );
        print_file (n+1) fd
        with End_of_file -> ()

let _ = 
    Arg.parse specs (fun x -> files := x :: !files)
        "Usage: cat [-n] -[help] [-v] filename ...";
    List.iter 
        (fun f -> print_file 1 (open_in f)) (List.rev !files);;


