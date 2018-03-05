open Tk
open Base
open List

let get_element   = Listbox.get 
let to_intlist lb = foldr (fun x xs -> (ios(Listbox.get lb ~index:x))::xs) []  

let i           = ref 100
let f lb tv ()     = 
    Listbox.insert lb ~index:`End ~texts:[soi !i];
    incr i;
    Textvariable.set tv (soi (foldr (+) 0 (to_intlist lb (Listbox.curselection lb))))

    
    

let top         = openTk ()
let tv          = Textvariable.create ()
let frame       = Frame.create top
let lb          = Listbox.create frame ~selectmode:`Multiple
let sb          = Scrollbar.create frame ~orient:`Vertical
let add         = Button.create top ~text:"Add all" ~command:(f lb tv)
let entry       = Entry.create top ~textvariable:tv

let () = 
    Listbox.configure lb ~yscrollcommand:(Scrollbar.set sb);
    Scrollbar.configure sb ~command:(Listbox.yview lb);
    pack [coe lb;coe sb] ~side:`Left ~fill:`Y;
    pack [coe frame; coe add; coe entry] ~side:`Top;
    mainLoop ()

