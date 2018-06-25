(* type text_index =                                                            *)
(*     [ `Atxy of int * int                                                     *)
(*     | `End                                                                   *)
(*     | `Image of font                                                         *)
(*     | `Linechar of int * int                                                 *)
(*     | `Mark of font                                                          *)
(*     | `Tagfirst of font                                                      *)
(*     | `Taglast of font                                                       *)
(*     | `Window of Widget.any Widget.widget ]                                  *)
(* type textModifier =                                                          *)
(*     [ `Char of num_index                                                     *)
(*     | `Line of num_index                                                     *)
(*     | `Lineend                                                               *)
(*     | `Linestart                                                             *)
(*     | `Wordend                                                               *)
(*     | `Wordstart ]                                                           *)
(* type textIndex = text_index * textModifier list                              *)
(* type num_index = int                                                         *)

open Tk 

let top             = openTk ()
let mb              = Menubutton.create top ~text:"Menu"
let ()              = Menubutton.configure mb ~direction:`Below
let menu            = Menu.create mb
let submenu         = Menu.create menu
let text            = Text.create top;;

let name            = ref "" 
let get_name ()     = "filename"

let read_file s     = name := s;open_in_gen  [Open_creat] 0o644 s
let write_file s    = open_out_gen [Open_creat;Open_wronly;Open_text] 0o644 s 

let rec text_of_read i txt =
    try     Text.insert(`End,[])(input_line i^"\n")txt; text_of_read i txt
    with    End_of_file -> ()

let rec write_of_text txt o = 
    try  output_string o (Text.get txt(`Atxy(0,0),[])(`End,[]))
    with End_of_file -> ()
let open_file ()    = text_of_read (read_file (get_name ())) text
let saveas ()       = write_of_text text (write_file(get_name()))
let save ()         = write_of_text text (write_file !name)

let config  ()      = 
    Menu.add_command menu ~label:"Open" ~command:(open_file);
    Menu.add_command menu ~label:"Save as"~command:(saveas);
    Menu.add_command menu ~label:"Save"   ~command:(save);
    Menu.add_cascade menu ~label:"Cascade" ~menu:submenu;
    Menu.add_command submenu ~label:"Quit!" ~command:(fun()->closeTk(); exit 0);
    Menu.add_separator submenu;
    Menu.add_checkbutton submenu~label:"Check"~command:(fun()->print_endline "hoge");
    Menubutton.configure ~menu:menu mb

let () =
    config ();
    pack [coe mb; coe text] ;
    mainLoop ();;
