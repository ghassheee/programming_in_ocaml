module IO   =
    struct
        let create_file s   = 
            open_out_gen [Open_wronly; Open_creat;  Open_text] 0o666 s
        let append_file s   = 
            open_out_gen [Open_wronly; Open_append; Open_text] 0o666 s
        let ask_remove  s   = 
            let () = print_endline("overwrite "^s^" ? (yes/no)") in 
            if "yes"=read_line() then Sys.remove s else exit 1;;
    end
