open Arg
open Printf


let  ver       = "0.1"
let  num_line  = ref false
let  files     = ref []

let print_ver ()    = printf "wc by OCaml, version %s\n" ver
let specs = [
    ("-l", Set num_line,    "Count the number of lines");
    ("-v", Unit print_ver,  "Display version")]

let rec number_of_lines n fd =
    let line = input_line fd in 
    try     number_of_lines (n+1) fd 
    with    End_of_file -> printf "%8i" (n+1)

(*
let rec number_of_letters n l fd =
    let line = input_line fd in
    let m = String.length line in
    try     number_of_letters (n+m) (l+1) fd 
    with    End_of_file -> printf "%8i%8i" (n+l+1) (l+1)
*)


let rec number_of_words first_space n w l fd =
    let c = input_char fd in
    try match c with 
          ' '   -> if first_space 
                    then number_of_words false (n+1)(w+1) l fd 
                    else number_of_words false (n+1)w l fd
        | '\n'  -> number_of_words false (n+1)(w+1)(l+1)fd
        | _     -> number_of_words true (n+1)w l fd
    with    End_of_file  -> printf "%8i%8i%8i" (l+1)(w+1)(n+1)

let number_of_fd fd =
    if !num_line then number_of_lines 0 fd else number_of_words false 0 0 0 fd

let _ = 
    Arg.parse specs (fun x-> files:=x :: !files)
        "Usage: wc [-l] filename ...";
    List.iter
        (fun f -> 
            number_of_fd (open_in f); 
            printf " %s\n" f 
        ) (List.rev !files);;

