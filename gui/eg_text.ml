open Tk ;;
let top     = openTk ();;
let mb      = Menubutton.create top ~name:"menu"
let text    = Text.create top;;




let () = 
    pack [coe mb; coe text] ;
    mainLoop ();;
