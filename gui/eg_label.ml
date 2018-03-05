open Tk

let top         = openTk()
let label       = Label.create top ~text:"This is a label with text"

let ()  =
    pack [coe label];
    mainLoop ()

