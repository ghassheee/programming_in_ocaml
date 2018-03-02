open Tk
open Printf

let balance             = ref 0
let add_balance x       = balance := !balance + x
let print_balance tv    = if !balance<0 
        then (closeTk();exit 0)
        else let s = sprintf "Balance: ¥%8d" !balance in Textvariable.set tv s;;

(*
 *   +--------------------------------------------+
 *   |                  Window                    |
 *   +--------------------------------------------+
 *   |                +---------+                 |
 *   |                | Label A |                 |
 *   |                +---------+                 |
 *   +--------------------------------------------+
 *   | +--------+-------Frame-----------+--------+|
 *   | |        |         |+---Frame---+|        ||
 *   | |+------+|+-------+||RadioButton||+------+||
 *   | ||LabelB||| Entry ||+-----------+||Button|||
 *   | |+------+|+-------+||RadioButton||+------+||
 *   | |        |         |+-----------+|        ||
 *   | +--------+---------+-------------+--------+|
 *   +--------------------------------------------+
 *) 

(* Window *)
let top                 = openTk()
let var_1               = Textvariable.create()
let var_2               = Textvariable.create()

let label_a             = Label.create top ~textvariable:var_1 ~relief:`Raised;;

let frame_b             = Frame.create top
let label_b             = Label.create frame_b ~text:"¥"
let entry               = Entry.create frame_b
let rb_frame            = Frame.create frame_b (* rb : radio buttons *) 
let mk_radiobutton tv x = Radiobutton.create rb_frame ~text:x ~value:x ~variable:tv 
let radiobuttons        = List.map (mk_radiobutton var_2) ["Deposite";"Withdraw"];;
    (* Button *)
let action entry var_2 var_1 () =
    let input = int_of_string (Entry.get entry) in
    match Textvariable.get var_2 with 
          "Deposite"        -> add_balance input  ; print_balance var_1
        | "Withdraw"        -> add_balance(-input); print_balance var_1
        | _                 -> failwith "Cannot happen";;
let update              = action entry var_2 var_1
let button              = Button.create frame_b ~text:"Action" ~command:(update);;


(* main *)
(* Packing *)
(* Loop function *)
let () =
    pack radiobuttons                                       ~side:`Top;
    pack [coe label_b;coe entry;coe rb_frame;coe button]    ~side:`Left;
    pack [coe label_a;coe frame_b]                          ~side:`Top;
    print_balance var_1;
    mainLoop()

    (* Tk.coe (coersion) : 'a Widget.widget -> Widget.any Widget.widget *)
    (* coe               : 'a widget -> any widget *)

