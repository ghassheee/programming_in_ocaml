open Tk
open Printf


(*
 *   +--------------------------------------------+
 *   |                  Window                    |
 *   +--------------------------------------------+
 *   |                +---------+                 |
 *   |                | BUtton  |                 |
 *   |                +---------+                 |
 *   +--------------------------------------------+
 *) 

let top         = openTk() 
let frame       = Frame.create top
let label       = Label.create frame ~text:"hoge"
let g ()        = Unix.sleep 2
let hoge_button = Button.create frame ~text:"hoge" ~command:(g);;
let f ()        = Button.invoke hoge_button 
let fuga_button = Button.create frame ~text:"fuga" ~command:(f);;
let h ()        = Button.flash fuga_button
let fooo_button = Button.create frame ~text:"fooo" ~command:(h);;

let () = 
    pack [coe label; coe fuga_button; coe hoge_button; coe fooo_button] ~side:`Left;
    pack [coe frame] ~side:`Top;
    mainLoop()
