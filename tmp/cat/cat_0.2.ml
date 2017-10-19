let ver = "0.2"
let line_n_on = ref false 
let list = ref []

let print_ver () = Printf.printf "cat by OCaml version: %s\n" ver
let spec = [
    ("-n", Arg.Set  line_n_on, "Display line number");
    ("-v", Arg.Unit print_ver,   "Display version")]

let rec println n fd = 
    let num = if !line_n_on then Printf.sprintf "%6d  " n else "" in
    let line = input_line fd in
    try 
        output_string stdout ( num ^ line ^ "\n" );
        println (n+1) fd
        with End_of_file -> ()

let _ = 
    Arg.parse spec (fun x -> list := x :: !list)
        "Usage: cat [-n] -[help] [-v] filename ...";
    List.iter 
        (fun f -> println 1 (open_in f)) 
            (List.rev !list);;


