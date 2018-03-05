open Tk



let top             = openTk ()
let var             = Textvariable.create ()
let label           = Label.create top ~textvariable:var;;

let settext tv s    = Textvariable.set tv s;;

let () =
    settext var "hoge";
    pack [ coe label ];
    mainLoop ()

