open Tk
open MySupport
open Board

let c_height    = 1 (* the height of a square piece of grid *)
let c_width     = 2 (* the width  of a square piece of grid *)

(* color *)
let default_color       = `White
let push_color          = `Blue
let select_color        = `Color "#ffdfdf"

let color_of_state      = function 
    | Pressed               -> push_color
    | NotPressed            -> default_color
let relief_of_state     = function
    | Pressed               -> `Sunken
    | NotPressed            -> `Ridge

let toggle              = function
    | Pressed               -> NotPressed
    | NotPressed            -> Pressed

let rec string_of_spec separator    = function
    | []                    -> ""
    | [i]                   -> string_of_int i
    | i::is                 -> string_of_int i ^ separator 
                                ^ string_of_spec separator is;;


(* Widgets *)
(* MousePointer is ON/OFF the square piece of grid *)
let focus label _       = Label.configure label ~background:select_color
let unfocus label st _  = Label.configure label ~background:(color_of_state !st) 

(* Button is Pushed on the square piece *)
let pressed label st _  = 
            st := toggle !st;
            Label.configure label 
                    ~background:(color_of_state !st) 
                    ~relief:(relief_of_state !st) ;;

(* clear the square *)
let clear label st _    =
            st := NotPressed;
            Label.configure label 
                    ~relief:(relief_of_state !st)
                    ~background:(color_of_state NotPressed);;

let clear_all cells states  = 
    List.iter2 
        (fun(_::c_row)st_row ->
            List.iter2(fun c_row st_row -> clear c_row st_row())c_row st_row)
        cells states;;

let quit()          = closeTk(); exit 0;;

let check h_spec v_spec body label ()   =
    if is_solved h_spec v_spec body 
        then Label.configure label ~text:"Right Answer!" ~foreground:`Red
        else Label.configure label ~text:"Wrong Answer..."~foreground:`Blue;;

(* make Widgets *)
(* make One line of squares *)
let rec make_cells ?(width=c_width) ?(height=c_height) parent = function
    | []                    -> []
    | x::xs                 -> 
            let label = Label.create parent ~width ~height ~relief:`Ridge
                            ~background:(color_of_state !x) in
            bind ~events:[`Enter]       ~action:(focus label) label;
            bind ~events:[`Leave]       ~action:(unfocus label x) label;
            bind ~events:[`ButtonPress] ~action:(pressed label x) label;
            label :: make_cells ~width ~height parent xs;;

let make_vspec ?(spwidth=c_width) ~spheight speclist parent =
    List.map 
        (fun s -> Label.create parent ~width:spwidth ~height:spheight 
                            ~text:s ~anchor:`S ~relief:`Groove)
        (List.map (string_of_spec "\n") speclist);;

let make_row ~spwidth ?(height=c_height) ?(width=c_width) spec parent cell_list = 
    let s = string_of_spec " " spec in
    Label.create parent ~width:spwidth ~height ~text:s ~anchor:`E ~relief:`Groove
    :: (make_cells ~height ~width parent cell_list);;


let make_board {
    width=width;
    height=height;
    h_spec=h_spec;
    v_spec=v_spec;
    body=body } b_clear b_check parent =
        let spwidth = max(max_list(List.map List.length h_spec) * 2) 10 in
        let spheight = max(max_list(List.map List.length v_spec)) 4 in
        let f1 = Frame.create parent in 
        let corner = Label.create ~width:spwidth ~height:spheight f1 in
        let reset_corner _ = Label.configure corner ~relief:`Raised 
                    ~text:"Draw\nLogic" ~foreground:`Black in
        reset_corner ();
        bind ~events:[`ButtonPress] ~action:reset_corner corner;
        Button.configure b_check ~command:(check h_spec v_spec body corner);
        let row0 = corner::make_vspec v_spec ~spheight f1 in
        pack row0 ~side:`Left ~anchor:`S;
        let frame_rows = make_list height (fun()->Frame.create parent) in 
        pack (f1::frame_rows) ~side:`Top;
        let rows = map3 (make_row  ~spwidth) h_spec frame_rows body in 
        List.iter (pack ~side:`Left) rows;
        Button.configure b_clear ~command:(fun()->clear_all rows body);;

let () =
    if Array.length Sys.argv = 1 then failwith "Usage: ilogic filename" else 
    begin
        let top = openTk() in
        
        let fr_board = Frame.create top in
        let fr_buttons = Frame.create top in
        pack [fr_board; fr_buttons] ~side:`Left ~fill:`Y;
        (* make buttons *)
        let b_check = Button.create fr_buttons ~text:"Check Answer" in 
        let b_clear = Button.create fr_buttons ~text:"Redo" in
        let b_quit  = Button.create fr_buttons ~text:"Quit" ~command:quit in 
        pack [b_check;b_clear;b_quit] ~side:`Top ~fill:`X;
        (* Load information and make board *)
        let board = Input.input_board (Sys.argv.(1)) in
        make_board board b_clear b_check fr_board;
        Wm.title_set top "Drawing Logic";
        mainLoop()
    end;;


