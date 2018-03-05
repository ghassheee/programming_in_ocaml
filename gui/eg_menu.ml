open Tk 

let top         = openTk ()
let mb          = Menubutton.create top ~text:"Menu"
let menu        = Menu.create mb
let submenu     = Menu.create menu

let ()  = 
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
    pack [mb];
    mainLoop ()
