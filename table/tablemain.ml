open Base.Polymorphic
open Table.Table

let table = empty
    <<< ("a", "the first alphabet")
    <<< ("b", "the second alphabet")
    <<< ("zzz", "sleeping noize")

let () = print_string "table = \n" 
let () = match retrieve "a" table with 
          Some x        -> print_endline x
        | None          -> ()

let table' = table <<< ("a", "an indefinite article")

let () = match retrieve "a" table' with 
          Some x        -> print_endline x
        | None          -> ()
    
let () = List.iter 
        (fun (k,v)->print_string k;print_string": ";print_endline v;)
        (dump table')

