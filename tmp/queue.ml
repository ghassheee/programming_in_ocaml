type 'a mlist = MNil | MCons of 'a * 'a mlist ref

type 'a queue = {mutable head: 'a mlist; mutable tail: 'a mlist}

let create () = {head = MNil; tail = MNil};;

let q : int queue = create ();;

let append a = function 
      {head = MNil ; tail = MNil} as q -> 
            let c = MCons (a, ref MNil) in 
            q.head <- c; q.tail <- c
    | {tail = MCons(_,next)} as q -> 
            let c = MCons (a, ref MNil) in
            next := c; q.tail <- c
    | _ -> failwith "enqueue: input queue broken";;


let push a = function 
      {head = MNil; tail = MNil} as q ->
          let c = MCons (a, ref MNil) in
          q.head <- c; q.tail <- c
    | {head = qhead } as q ->
            let c = MCons (a, ref qhead) in
            q.head <- c 
    | _ -> failwith "enqueue: input queue broken";;


let peek = function 
      {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
    | {head = MCons(a, _) } -> a
    | _ -> failwith "hd: queue is broken";;


let pop = function
      {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
    | {head = MCons(a, next)} as q when !next=MNil -> q.head<-MNil;
    q.tail<-MNil; a
    | {head = MCons(a, next)} as q -> q.head <- !next; a
    | _ -> failwith "dequeue: queue is broken";;

