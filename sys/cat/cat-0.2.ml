let ver         = "0.2"
let line_num    = ref false 
let list        = ref []

let print_ver () = Printf.printf "cat by OCaml version: %s\n" ver
let spec = [
    ("-n", Arg.Set  line_num,   "Display line number");
    ("-v", Arg.Unit print_ver,  "Display version"    )]

let rec print_file n fd = 
    let num = if !line_num then Printf.sprintf "%6d  " n else "" in
    let line = input_line fd in
    try 
        output_string stdout ( num ^ line ^ "\n" );
        print_file (n+1) fd
        with End_of_file -> ()

let _ = 
    Arg.parse spec (fun x -> list := x :: !list)
        "Usage: cat [-n] -[help] [-v] filename ...";
    List.iter 
        (fun f -> print_file 1 (open_in f)) (List.rev !list);;


