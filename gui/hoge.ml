open Tk
open Printf

(* balance *)
let balance             = ref 0
let add_balance x       = balance := !balance + x

(* Widget *)
let top                 = openTk()
let tv_balance          = Textvariable.create ()
let label_a             = Label.create top 
                            ~textvariable:tv_balance 
                            ~relief:`Raised;;

let print_balance tv    = if !balance<0 
                            then (closeTk();exit 0)
                            else 
                                let s = sprintf "Balance: ¥%8d" !balance in
                                Textvariable.set tv s;;

let bot_frame           = Frame.create top
let entry               = Entry.create bot_frame
and label_b             = Label.create bot_frame ~text:"¥"
and rb_frame            = Frame.create bot_frame (* rb : radio buttons *) 

let tv_button           = Textvariable.create()
let mk_radiobutton x    = Radiobutton.create rb_frame 
                            ~text:x ~value:x ~variable:tv_button 
let radiobuttons        = List.map mk_radiobutton ["Deposite";"Withdraw"];;
let action entry tv_but tv_bal () =
    let y = int_of_string (Entry.get entry) in
    match Textvariable.get tv_but with 
          "Deposite"        -> add_balance y  ; print_balance tv_bal
        | "Withdraw"        -> add_balance(-y); print_balance tv_bal
        | _                 -> failwith "Cannot happen";;

let button              = Button.create bot_frame
                            ~text:"Action" 
                            ~command:(action entry tv_button tv_balance);;

let () =
    pack radiobuttons ~side:`Top;
    (* Tk.coe (coersion) : 'a Widget.widget -> Widget.any Widget.widget *)
    (* coe               : 'a widget -> any widget *)
    pack [coe label_b;coe entry;coe rb_frame;coe button] ~side:`Left;
    pack [coe label_a;coe bot_frame] ~side:`Bottom;
    print_balance tv_balance;
    mainLoop()


