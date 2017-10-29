let ver         = "0.2"
let line_num    = ref false 
let list        = ref []

let print_ver () = Printf.printf "cat by OCaml version: %s\n" ver
let spec = [
    ("-n", Arg.Set  line_num,   "Display line number");
    ("-v", Arg.Unit print_ver,  "Display version"    )]

let rec print_file n in_fd out_fd = 
    let num = if !line_num then Printf.sprintf "%6d  " n else "" in
    let line = input_line in_fd in
    try 
        output_string out_fd ( num ^ line ^ "\n" );
        print_file (n+1) in_fd out_fd
        with End_of_file -> ()

let _ = 
    Arg.parse spec (fun x -> list := x :: !list)
        "Usage: cat [-n] -[help] [-v] filename ...";

    let (out_file::in_files) = !list in
    List.iter (fun x-> print_endline x (*print_file 1 (open_in x)(open_out out_file) *) ) (List.rev in_files) ;;




