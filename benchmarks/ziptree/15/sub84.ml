type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree*zipper
exception NOMOVE of string
let goLeft : location -> location = fun loc -> match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun loc -> match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, l::right)) -> LOC(l,HAND(t::left,up,right))
	| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp: location -> location = fun loc -> match loc with
	| LOC(t, TOP) -> raise (NOMOVE "top of top")
	| LOC(t, HAND(left,up,right)) ->
		let tempTreeList = (List.rev left)@[t]@right in
		LOC((NODE(tempTreeList),up))
let goDown: location -> location = fun loc ->
	match loc with
	| LOC(LEAF(i),_) -> raise (NOMOVE "bottom of bottom")
	| LOC(NODE([]),_) -> raise (NOMOVE "empty list")
	| LOC(NODE(head::tail),z) -> LOC(head,HAND([],z,tail))
(*

let tree = NODE [ NODE [LEAF "a";LEAF "b";LEAF "c"]; 
                      NODE [NODE [LEAF "d"]];
                      LEAF "e";
                      NODE [NODE[LEAF "f"; LEAF "g"];
                            NODE[LEAF "h"; NODE[LEAF "i"]; LEAF "j"];
                            NODE[]]]

let chkLeaf tre e = match tre with
  | LOC(LEAF(it), k) -> let _ = assert (e=it) in 1
  | LOC(NODE(it), k) -> let _ = assert (false) in 1
let z = LOC (tree, TOP)

let z = goDown z
let z = goDown z
let _ = chkLeaf z "a"
let z = goRight z
let _ = chkLeaf z "b"
let z = goRight z
let _ = chkLeaf z "c"
let z = goLeft z
let _ = chkLeaf z "b"
let z = goLeft z
let _ = chkLeaf z "a"
let z = goUp z
let z = goRight z
let z = goDown z
let z = goDown z
let _ = chkLeaf z "d"
let z = goUp z
let z = goUp z
let z = goRight z
let _ = chkLeaf z "e"
let z = goRight z
let z = goDown z
let z = goDown z
let _ = chkLeaf z "f"
let z = goRight z
let _ = chkLeaf z "g"
let z = goUp z
let z = goRight z
let z = goDown z
let _ = chkLeaf z "h"
let z = goRight z
let z = goDown z
let _ = chkLeaf z "i"
let z = goUp z
let z = goRight z
let _ = chkLeaf z "j"
(*let z = goDown z*) (*Error *)
let z = goUp z
let z = goRight z
(*let z = goDown z*) (*Error*)
let z = goUp z
let z = goUp z
(* once *)

(*let z = goUp z*) (*Error*) 
(*let z = goRight z*) (*Error*) 
let z = goLeft z (*Error*) 

let z = goDown z
let z = goDown z
let _ = chkLeaf z "a"
let z = goRight z
let _ = chkLeaf z "b"
let z = goRight z
let _ = chkLeaf z "c"
let z = goLeft z
let _ = chkLeaf z "b"
let z = goLeft z
let _ = chkLeaf z "a"
let z = goLeft z (* Error *)
let z = goUp z
let z = goRight z
let z = goDown z
let z = goDown z
let _ = chkLeaf z "d"
let z = goUp z
let z = goUp z
let z = goRight z
let _ = chkLeaf z "e"
let z = goRight z
let z = goDown z
let z = goDown z
let _ = chkLeaf z "f"
let z = goRight z
let _ = chkLeaf z "g"
let z = goRight z (* Error *) 
let z = goUp z
let z = goRight z
let z = goDown z
let _ = chkLeaf z "h"
let z = goRight z
let z = goDown z
let _ = chkLeaf z "i"
let z = goUp z
let z = goRight z
let _ = chkLeaf z "j"
let z = goUp z
let z = goUp z
let z = goUp z



*)
