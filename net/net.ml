module Debug =
    struct
        let print_debug () = print_endline "debug"
    end open Debug

module Net =
    struct
        open Unix
        open Printf
        let c = ref 0 
        let check_args ()           =
                if Array.length Sys.argv<2 then eprintf "specify port.\n" else ()
        let addr_of_host host       = host.h_addr_list.(0)
        let sockaddr_of addr port   = ADDR_INET (addr, port)
        let socket_internet         = socket    (PF_INET)(SOCK_STREAM)0
        let gethost ()              = gethostbyname (gethostname())
        let getaddr ()              = addr_of_host  (gethost())
        let getport ()              = int_of_string (Sys.argv.(1))
        let rec restart f x         = 
                try f x with Unix_error(EINTR,_,_) -> restart f x
        let server f sockaddr       = 
                bind socket_internet sockaddr;
                listen socket_internet 3;
                while true do   let (sock,caller) =
                    restart accept socket_internet in match fork() with
                          0   -> if fork()<>0 then exit 0;    (* fork start *)
                                 f sock ;                     (* main service *) 
                                 exit 0;                      (* fork end *)
                        | pid -> close sock; ignore(restart(waitpid[])pid)done;;
        let wrap_server f           = check_args ();
                try server f (sockaddr_of(getaddr())(getport()))
                with Failure("int_of_string")->eprintf "bad port number\n"
        let go f                    = handle_unix_error wrap_server f

        (*    fork () = if ( there exists its fork )                *)
        (*                  then ( return the existing pid )        *)
        (*                  else ( make a new fork and return 0 )   *)

        let uncurry f' = fun sock -> (* f' i o  -> f sock *)    
            let i = in_channel_of_descr sock in 
            let o = out_channel_of_descr sock in 
            f' i o; 
            close_in i; close_out o
 
        (* e.g. UpperCase Service *)
        let upper_case_service i o  = try while true do
                let s = String.uppercase (input_line i) in
                output_string o (s^"\n"); flush o;
                flush Pervasives.stdout;
                done with _     ->  printf"End of text\n"; 
                                    flush Pervasives.stdout;exit 0

        (* e.g. http service *)
        let http_header = 
            "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"
        let html = "<html><head>\
            <title>Goodbye, world!</title>\
            <style>body { background-color: #0FF }\
            h1 { font-size:3em; color: black; }</style></head>\
            <body><h1>Goodbye, world!</h1></body></html>\r\n"
        let http_service sock = try
            let res = http_header ^ html in
            write sock res 0 (String.length res);
        with _ -> printf "unknown Error\n"; flush Pervasives.stdout; exit 0
    end open Net



(* let main = go (uncurry upper_case_service) *)
let main = go http_service
