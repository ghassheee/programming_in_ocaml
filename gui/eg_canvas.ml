open Tk

let top = openTk ();;
let canvas = Canvas.create top;;
let circle = Canvas.create_oval canvas 
                ~x1:10~y1:10~x2:100~y2:100 
                ~fill:`Blue ~width:5 ~outline:`Red;;
let b = Button.create top 
                ~text:"Change color!"
                ~command:(fun()->Canvas.configure_oval canvas circle ~fill:`Red);;

let () =
    pack [coe canvas; coe b];
    mainLoop () ;;
