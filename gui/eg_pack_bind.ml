open Tk

let top = openTk();;

let frame1  = Frame.create top;;
let button  = Button.create 
                    ~text:"Delete Frame" frame1 
                    ~command:( fun () -> Pack.forget[frame1]) (* forget erases *)
let text    = Text.create frame1;;
let canvas  = Canvas.create frame1;;
let radio   = Radiobutton.create top ~text:"radiobutton";;
let scale   = Scale.create top ;;
let mb      = Menubutton.create top ~text:"menu";;
let menu    = Menu.create mb;;
let tv      = Textvariable.create ();;


let config  = 
    Menu.add_command menu ~label:"hello" ~command:(fun()->print_endline"hello";
    flush stdout);
    Menu.add_checkbutton menu
                    ~label:"checkbutton" 
                    ~onvalue:"on"
                    ~offvalue:"off"
                    ~variable:tv;
    Menu.add_separator menu;
    Menubutton.configure ~menu:menu mb;
    Tk.bind ~action:(fun ev -> print_endline ev.ev_Char) 
            ~events:[`KeyPress] 
            ~fields:[`Char] top


let () =
    pack [coe button; coe text; coe canvas] ~anchor:`Se ~side:`Left;
    pack [coe frame1; coe radio; coe mb] ~side:`Top ~anchor:`Center;
    mainLoop ();;


