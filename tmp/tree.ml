#use "core.ml";;
#use "list.ml";;


(* Binary Tree *)
module BTree = 
    struct 
        type 'a btree = 
                  Lf 
                | Br of 'a * 'a btree * 'a btree;;

        let rec size = function 
              Lf                -> 0
            | Br (_,l,r)        -> 1 + size l + size r ;;
        let rec depth = function 
              Lf                -> 0
            | Br (_,l,r)        -> 1 + max(depth l)(depth r);;
        let rec preorder = function 
              Lf                -> []
            | Br (x,l,r)        -> x :: (preorder l) @ (preorder l);;
        let rec inorder = function 
              Lf                -> []
            | Br (x,l,r)        -> (inorder l) @ (x :: inorder r ) ;;
        let rec postorder = function 
              Lf                -> []
            | Br (x, l, r )     -> (postorder l) @ (postorder l) @ [x];;
        let rec preord t l = match t with 
              Lf                -> l
            | Br (x, l,r)       -> x :: (preord l ( preord r l));;
        let rec mem t x = match t with 
              Lf                -> false 
            | Br (y, l , r )    -> if x=y 
                                        then true 
                                        else if x<y then mem l x else mem r x;;
        let rec add t x = match t with 
              Lf                                -> Br(x,Lf,Lf)
            |(Br(y,l,r) as subtree) when x=y    -> subtree
            | Br(y,l,r)             when x<y    -> Br(y, add l x, r)
            | Br(y,l,r)                         -> Br(y, l, add r x);;
    end;;

module RTree = (* Rose Tree *)
    struct
        open BTree 
        type 'a rtree = 
              RLf 
            | RBr of 'a * 'a rtree list;;
        type ('a, 'b) xml = 
              XLf of 'b option 
            | XBr of 'a * ('a, 'b) xml list;;

        let rec string_of_xml = function
              XBr (tag, xml_list)   -> "<" ^ tag ^ ">" ^ 
                                      string_of_xmllist xml_list ^ "</" ^ tag ^ ">"
            | XLf  None             -> ""
            | XLf (Some s)          -> s
        and string_of_xmllist = function 
              []                    -> ""
            | xml :: rest           -> string_of_xml xml ^ string_of_xmllist rest ;;

        (* Rose Tree <--> Binary Tree *)
        let rec rtree_of_btree = function
              Lf                    -> RLf
            | Br (a,l,r)            -> RBr(a, map rtree_of_btree [l; r]);;
        let rec btree_of_rtree = function
              RLf                   -> Br (None,Lf,Lf)
            | RBr(a,rtrees)         -> Br (Some a, btree_of_rtreelist rtrees, Lf)
        and btree_of_rtreelist = function
              []                    -> Lf
            | rtree :: rest         -> let Br (a,l,Lf) = btree_of_rtree rtree in
                                       Br (a,l, btree_of_rtreelist rest);;
        (* e.g. *)     
        let addressbook =
            XBr ("addressbook",[
                XBr ("person", [
                    XBr ("name" , [XLf (Some "Atsushi Igarashi")]);
                    XBr ("tel" , [XLf (Some "075-123-4567")])]);
                XBr ("person", [XLf None]);
                XBr ("Person", [XLf None])]);;
        let rtree = 
            RBr ("a", [
                RBr ("b", [
                    RBr ("c", [RLf]);
                    RLf;
                    RBr ("d", [RLf])
                ]);
                RBr ("e", [RLf]);
                RBr ("f", [RLf])
            ]);;

    end;;


        

