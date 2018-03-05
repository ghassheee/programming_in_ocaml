open Tk


let top         = openTk()
let button      = Button.create top 


let ()  = 
    pack [coe button] ~side:`Top;
    mainLoop()
