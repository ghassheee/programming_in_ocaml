open Tk


let top         = openTk ()
let sc          = Scale.create top 
                        ~max:100. 
                        ~min:(-100.) 
                        ~showvalue:true
                        ~tickinterval:10.
                        ~label:"scale"
                        ~length:400 
                        ~orient:`Horizontal;;
let btn         = Button.create top
                        ~text:"Preess me!"
                        ~command:(fun()->print_float(Scale.get sc);print_newline())


let ()  =
    pack [coe sc; coe btn] ~side:`Top;
    mainLoop ()
