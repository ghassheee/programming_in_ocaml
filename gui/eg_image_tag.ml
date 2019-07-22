open Tk

let top = openTk();;
let text = Text.create top;; 
let photo = Imagephoto.create ~file:"edit.png" ()
let image = Text.image_create ~index:(`Atxy(0,0),[]) text ~image:photo;;
let () = 
    Text.insert ~index:(`End,[]) ~text:"\nThis is an image.\napple\norange" text;
    Text.tag_add text ~tag:"red" ~start:(`Linechar(3,0),[]) ~stop:(`Linechar(4,0),[]);
    Text.tag_configure ~tag:"red" ~background:`Red text;
    Text.tag_add text~tag:"orange"~start:(`Linechar(4,0),[])~stop:(`Linechar(5,0),[]);
    Text.tag_configure~tag:"orange"~background:`Yellow text

let () = 
    pack [text];
    mainLoop()
