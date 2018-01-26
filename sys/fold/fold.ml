open Arg
open Printf

let ver     = "0.1"
let width   = ref 80
let files   = ref []

let print_ver ()    = printf "fold by OCaml: version %s\n" ver
let specs = [
    ("--width", Set_int width,  "Set the width of line");
    ("-w",      Set_int width,  "Set the width of line");
    ("-v",      Unit print_ver, "Display version")]

let rec fold_of_width n fd =
    let c = input_char fd in 
    try match c with 
          '\n'               ->
            let()= printf "\n" in fold_of_width 1 fd
        | _ when n >= !width -> 
            let()= printf "%c\n" c in fold_of_width 1 fd 
        | _                  -> 
            let()= printf "%c" c   in fold_of_width(n+1)fd 
    with End_of_file -> printf ""

let _ =
    Arg.parse specs (fun x -> files := x :: !files)
        "Usage: fold [--width width] filename ...";
    List.iter
        (fun f -> fold_of_width 1 (open_in f))(List.rev !files);;
