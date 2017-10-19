(* using modules ! *)


List.length [5;6;7];;
List.concat [[1;2];[3;4]];;

let q = Queue.create ();;
Queue.add 1 q; Queue.add 2 q;;
Queue.take q;; 
Queue.take q;;
(* Queue.take q;;   (* Exception *) *)

Array.make;;     (* int -> 'a -> 'a array *)
Array.make 4 'a';;
Array.init;;     (* int -> (int -> 'a) -> 'a array *)
Array.init 10 (fun i -> char_of_int (i + 48));;  (* [| '0'; '1'; ... ;'9' |] *)
Array.length;;
Array.append;;
Array.concat;;
Array.to_list [|1;2;3;4;5|];;
Array.of_list [1;2;3];;
Array.map (fun x -> x *2)  [| 1;2 |];;
Array.iter (print_string) [| "hello" ; "world" |];;

Printf.printf "pad with zero: %04d\n" 5;;
Printf.fprintf stdout "decimal: %d \nhexadecimal: %x\n string: %s\n" 110 110
"hello";;
Printf.printf "decimal: %d hexadecimal: %x string: %s\n" 110 110 "hello";;
Printf.sprintf "decimal: %d hexadecimal: %x string: %s\n" 110 110 "hello";;
Printf.sprintf;; (* return string *)
Printf.printf;;  (* return unit *)


Scanf.scanf;;
Scanf.sscanf;;
Scanf.fscanf;;
let f name age = name ^ (if age < 20 
                            then ", you cannot vote."
                            else ", you can vote.");;
let g str = Scanf.sscanf str "%s is %d years old." f;;
g "Igarashi is 20 years old.";;


