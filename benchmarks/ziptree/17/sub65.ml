
type item = string
type tree =
 | LEAF of item
 | NODE of tree list
type zipper =
 | TOP
 | HAND of tree list * zipper * tree list (** older siblings, zipper for parent, younger siblings *)
type location = LOC of tree * zipper
exception NOMOVE of string
(** Basically, left is reversed! *)
let goLeft loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "left of top")
 | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
 | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

(**
  goRight: location -> location
  goUp: location -> location
  goDown: location -> location
*)
let goRight loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "right of top")
 | LOC(t, HAND(left, up, r :: right)) -> LOC(r, HAND(t :: left, up, right))
 | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
 | LOC(t, TOP) -> raise (NOMOVE "up of top")
 | LOC(t, HAND(left, up, right))-> LOC(NODE((List.rev left)@ (t::right)) , up)

let goDown loc = match loc with
 | LOC(LEAF(i), pzipper) -> raise (NOMOVE "down of leaf")
 | LOC(NODE([]) , pzipper) -> raise (NOMOVE "down of empty node")
 | LOC(NODE(hd :: tl), pzipper) ->  LOC(hd, HAND([],pzipper,tl))(** leftmost child*)

(** Testcases *)
(**
 let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"];
                       LEAF "+";
                       NODE [LEAF "c"; LEAF "*"; LEAF "d"]],
                 TOP)

 let (|>) g f = f g

 let a91 = loc1 |> goDown
 let a92 = loc1 |> goDown |> goDown
 let a93 = loc1 |> goDown |> goUp |> goDown
 let a94 = loc1 |> goDown |> goDown |> goRight
 let a95 = loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight
 let a96 = loc1 |> goDown |> goRight |> goRight |> goDown |> goRight
 let a97 =
     try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true
 ;;
*)
