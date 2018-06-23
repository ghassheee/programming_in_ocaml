open Tk 

let top         = openTk ()
let mb          = Menubutton.create top ~text:"Menu"
let ()          = Menubutton.configure mb ~direction:`Above

let menu        = Menu.create mb
let submenu     = Menu.create menu

let loop  ()      = 
    Menu.add_command menu 
            ~label:"Press_me"
            ~command:(fun()->print_endline "Hello!");
    Menu.add_cascade menu 
            ~label:"Cascade"
            ~menu:submenu;
    Menu.add_command submenu
            ~label:"Don't Press me!"
            ~command:(fun()->closeTk(); exit 0);
    Menu.add_separator submenu;
    Menu.add_checkbutton submenu
            ~label:"Check"
            ~command:(fun ()-> print_endline "hoge");
    Menubutton.configure ~menu:menu mb;
    pack [mb]

let ()          = 
    loop ();
    mainLoop ()
open Tk

let top         = openTk ()
let menu        = Menu.create top
let submenu     = Menu.create menu


let () = 
    Menu.add_command menu ~label:"Press me!" ~command:(fun()->print_endline"hello");
    Menu.add_cascade menu ~label:"Cascade" ~menu:submenu;
    Menu.add_command submenu 
        ~label:"Don't press me!" 
        ~command:(fun()->closeTk();exit 0);
    Menu.add_separator submenu;
    Menu.add_checkbutton ~label:"Check" submenu;
    Toplevel.configure top ~menu:menu;
    mainLoop()
open Tk ;;
let top     = openTk ();;
let mb      = Menubutton.create top ~name:"menu"
let text    = Text.create top;;




let () = 
    pack [coe mb; coe text] ;
    mainLoop ();;
