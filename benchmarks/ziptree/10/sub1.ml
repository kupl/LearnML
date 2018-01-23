type tree =
  LEAF of item
| NODE of tree list
(*and item = string
exception NOMOVE;;*)

type zipper =
  TOP
| HAND of tree list * zipper * tree list

type location
 = LOC of tree * zipper

let goLeft loc = match loc with
  LOC(t, TOP) -> raise NOMOVE "left of top"
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([], up, right)) -> raise NOMOVE "left of first"

let goRight loc = match loc with
  LOC(t, TOP) -> raise NOMOVE "right of top" (* 맨 위에 있을 경우 *)
| LOC(t, HAND(left, up, [])) -> raise NOMOVE "right of first" (* 맨 오른쪽에 있을 경우 *)
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right)) (* 맨 위에도, 오른쪽에도 있지 않을 경우. *)

let getDown loc = match loc with
  LOC(LEAF t, _) -> raise NOMOVE "this is bottom" (* 맨 아래 있을 경우 *)
| LOC(NODE t, f) -> if List.length t == 0 then raise NOMOVE "empty node" else LOC( List.hd t, HAND([], f, List.tl t));;

let goUp loc = match loc with
  LOC(t, TOP) -> raise NOMOVE "this is top" (* 맨 위에 있을 경우 *)
| LOC(t, HAND(left, up, right)) -> LOC ( NODE ( left @ [t] @ right), up);;

(*let testloc1 =
LOC(LEAF "+",
    HAND(
	    [LEAF "c"],
			HAND(
				[],
				HAND(
					[LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
					TOP,
					[]
					),
				[LEAF "*"; LEAF "e"]
				),
		[LEAF "d"]
		)
	);;
	
let testloc2 =
LOC( NODE[LEAF "c"; LEAF "+"; LEAF "d"],
		HAND(
			[],
			HAND(
				[LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
				TOP,
				[]
				),
			[LEAF "*"; LEAF "e"]
			)
	);;
	
let testloc3 =
LOC(LEAF "c", 
    HAND(
	    [],
			HAND(
				[],
				HAND(
					[LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
					TOP,
					[]
					),
				[LEAF "*"; LEAF "e"]
				),
		[LEAF "+"; LEAF "d"]
		)
	);;*)
