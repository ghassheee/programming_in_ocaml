let version             = "0.2"
let line_number_is_on   = ref false 
let args                = ref []
let print_version ()    = Printf.printf "cat by OCaml version: %s\n" version
let spec                = [
    ("-n", Arg.Set  line_number_is_on,  "Display line number");
    ("-v", Arg.Unit print_version,      "Display version"    )]

let create_file s   = open_out_gen [Open_wronly; Open_creat;  Open_text] 0o666 s
let append_file s   = open_out_gen [Open_wronly; Open_append; Open_text] 0o666 s
let ask_remove  s   = let () = print_endline("overwrite "^s^" ? (yes/no)") in 
                      if "yes"=read_line() then Sys.remove s else exit 1;;
            
let rec print_file n i o = 
    let num     = if !line_number_is_on then Printf.sprintf "%6d  " n else "" in
    let line    = input_line i in
    try output_string o (num^line^"\n");
        print_file (n+1) i o
    with End_of_file    -> ()

let open_file s     = let () = if Sys.file_exists s 
                                    then(ask_remove s;close_out (create_file s))
                                    else () in 
                      open_out s;;

let _ = 
    Arg.parse spec (fun x -> args := x :: !args)
        "Usage: cat [-n] -[help] [-v] filename ...";
    let output::inputs          = !args             in
    let out_fd                  = open_file output  in 
    List.iter(fun s->print_file 1(open_in s)out_fd)(List.rev inputs);;




