open Tk


let top = openTk()
let radiobutton = Radiobutton.create top ~text:"hoge"
let radiobutton2 = Radiobutton.create top ~text:"fuga"

let () =
    pack [coe radiobutton; coe radiobutton2] ~side:`Top;
    mainLoop()
