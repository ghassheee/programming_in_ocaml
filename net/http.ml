let try_final f x fin y     = (fin y);(try f x with e -> fin y; raise e)
let rec restart f x         = try f x with Unix_error(EINTR,_,_) -> restart f x

let double_fork_treatment server service (cl_fd, _ as client) =
  let treat () =
    match fork () with
    | 0 -> if fork () <> 0 then exit 0;
        close server; service client; exit 0
    | k -> ignore (restart (waitpid []) k)
  in
  try_final treat () close cl_fd

let install_tcp_server_socket addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    bind s addr;
    listen s 10;
    s
  with e -> close s; raise e

let tcp_server treat_connection addr =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
    let client = restart_on_EINTR accept server_sock in
    treat_connection server_sock client
  done

let server () =
  let port = 8080 in
  let host = (gethostbyname (gethostname())).h_addr_list.(0) in
  let addr = ADDR_INET (host, port) in
  let treat sock (client_sock, client_addr as client) =
    let service (s, _) =
      let response = "\
        HTTP/1.1 200 OK\r\n\
        Content-Type: text/html; charset=UTF-8\r\n\r\n\
        <html><head><title>Goodbye, world!</title>\
        <style>body { background-color: #0FF }\
        h1 { font-size:3em; color: black; }</style></head>\
        <body><h1>Goodbye, world!</h1></body></html>\r\n"
      in
      write s response 0 (String.length response);
    in
    double_fork_treatment sock service client
  in
  tcp_server treat addr

let _ =
  handle_unix_error server ()
