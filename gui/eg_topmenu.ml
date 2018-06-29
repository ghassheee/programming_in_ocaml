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
