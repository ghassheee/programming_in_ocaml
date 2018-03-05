open Tk
open Printf
open Base



let i   = ref 100
let sprint_list l    = 
    let rec loop        = function
          []            -> ""
        | [x]           -> x
        | x::xs         -> x ^ "," ^ loop xs in
    "[" ^ loop l ^ "]"
let f lb () = 
    print_endline ("size of listbox is " ^ soi (Listbox.size lb));
    Listbox.insert lb ~index:`End ~texts:[soi !i];
    print_endline ( sprint_list (
            List.listr (fun x->Listbox.get lb ~index:x)(Listbox.curselection lb)));
    incr i

let top = openTk ()

let lb  = Listbox.create top ~selectmode:`Multiple
let b   = Button.create top 
                ~text:"Press me!"
                ~command:(f lb)

let () =
    pack [coe b; coe lb] ~side:`Top;
    mainLoop ()


