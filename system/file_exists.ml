let _ = for i = 1 to Array.length Sys.argv - 1 do
    let f = Sys.argv.(i) in
    if Sys.file_exists f 
        then print_endline ( f ^ " exists." )
        else print_endline ( f ^ " does not exists." )
    done;;

