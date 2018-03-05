open Tk
open Printf



(* Window *)
let top                 = openTk()


let frame_b             = Frame.create top
let label_b             = Label.create frame_b ~text:"hogehogefugafuga"


let () =
    pack [coe label_b]    ~side:`Left;
    pack [coe frame_b]    ~side:`Top;
    mainLoop()


