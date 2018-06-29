open Tk 

(* Mb.create    : 'a Wg.widget      -> ~    -> Wg.mb Wg.widget  *) 
(* Mb.configure : Wg.mb Wg.widget   -> ~    -> unit             *)

(* type menubuttonDirection = [
                    | 'Above
                    | 'Below
                    | 'Left
                    | 'Right ]                                  *)

(* Mn.create    : 'a Wg.widget      -> ~    -> Wg.mn Wg.widget  *)
(* Mn.configure : Wg.mn Wg.widget   -> ~    -> unit             *)

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
