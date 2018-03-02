(* Please include 
 * 'labltk.cma' in using this file;
 *
 * $ ocamlc -I path/to/library/of/labltk labltk.cma tk.ml 
 *)

open Tk


(* GUI composed of widgets *)
(* 
 *   GUI        := [widget, widget, .... ]
 *   widget     := widget
 *               | [widget, widget, ... ]
 *
 *   widget     := [attribute, attribute, ...]
 *
 *   Window     := TopLevel widget 
 *   Toplevel   := having no parent
 *
 *   Frame
 *   Label
 *   Message
 *   Button
 *   Radiobutton
 *   Checkbutton
 *   Entry
 *   Listbox        := 
 *   Scrollbar
 *   Scale
 *   Menubutton
 *   Menu
 *   Text
 *   Canvas
 *
 *
 *)
module TV = 
    struct
        open Textvariable 
        let handle  = handle 
        let coerce  = coerce
        let create  = create
        let set     = set
        let get     = get
        let name    = name 
        let free    = free
    end

module Frame = 
    struct 
        open Frame 
        let create          = create
        let configure       = configure
        let configure_get   = configure_get
    end

module Label =
    struct 
        open Label
        let create          = create
        let configure       = configure
        let configure_get   = configure_get
    end
    
module Message = 
    struct
        open Message
        let create          = create
        let configure       = configure
        let configure_get   = configure_get
    end

module Button =
    struct  
        open Button
        let create          = create
        let configure       = configure
        let configure_get   = configure_get
        let flash           = flash         (* turn on/off button *)
        let invoke          = invoke        (* the same with pushing the button*) 
    end



