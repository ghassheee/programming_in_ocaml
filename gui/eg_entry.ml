open Tk


let top = opentk()
let entry = Entry.create top ~show:'*'
let b   = Button.create top ~text:"Press me" 
                ~command:(fun()-> print_endline(Entry.get entry))
let () =
    pack [coe entry; coe b];
    mainLoop ()
