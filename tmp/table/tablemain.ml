let (<<<) = Table.(<<<)

let table = Table.empty
    <<< ("a", "the first alphabet")
    <<< ("b", "the second alphabet")
    <<< ("zzz", "sleeping noize")



let () = 
    match Table.retrieve "a" table with 
          Some x -> print_string x; print_newline ()
        | None -> ()

let table' = table <<< ("a", "an indefinite article")


let () = 
    match Table.retrieve "a" table' with 
          Some x -> print_string x; print_newline ()
        | None -> ()

    
let () =
    List.iter 
        (fun (key,body) ->
            print_string key;
            print_string ": ";
            print_string body;
            print_newline ())
        (Table.dump table')

