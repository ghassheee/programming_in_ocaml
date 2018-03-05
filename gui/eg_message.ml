open Tk

let top = openTk();;

let msg = Message.create top 
                ~justify:`Center
                ~aspect: 150
                ~text:"Betty bought a bit better butter."

let () =
    pack [coe msg];
    mainLoop()
