let ver = "0.1"
let display_linenum = ref false 
let filenames = ref []


let spec = 
    [
    ("-n", Arg.Set display_linenum, "Display line number");
    ("-v", Arg.Unit (fun () -> Printf.printf "cat by OCaml version: %s\n" ver),
        "Display version number")
    ]


let getline file = 
    try input_line file with
    End_of_file -> "End_of_file";;

let _ = 
    Arg.parse spec 
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] -[help] [-v] filenames ...";
    if !display_linenum then print_endline "-n was turned on";
    List.iter 
        (fun s -> 
            let file = open_in s in
            let line = ref "" in 
            while (line := getline file ; !line <> "End_of_file" ) do
                output_string stdout ( !line ^ "\n") 
            done;
        )
            (List.rev !filenames);;


