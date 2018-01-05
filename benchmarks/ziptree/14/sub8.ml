type item = string
and tree = LEAF of item
| NODE of tree list
and zipper = TOP
| HAND of tree list * zipper * tree list
and location = LOC of tree * zipper;;

exception NOMOVE of string

let goLeft loc =
	match loc with
	    | LOC(t, TOP) -> raise (NOMOVE "left of top")
	    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first");;

let goRight loc =
    match loc with
        | LOC(t, TOP) -> raise (NOMOVE "right of top")
        | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
        | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first");;


let goUp loc =
    match loc with
        | LOC(t, TOP) -> raise (NOMOVE "up of top")
        | LOC(NODE t, HAND(treeList1, zipper, treeList2)) -> LOC((NODE (List.append (List.append t treeList1) treeList2)), zipper)
        | LOC(LEAF t, HAND(treeList1, zipper, treeList2)) -> LOC((NODE (List.append ((LEAF t)::treeList1) treeList2)), zipper);;

let goDown loc =
    match loc with
        | LOC(LEAF item, zipper) -> raise (NOMOVE "tree is leaf")
        | LOC((NODE []), zipper) -> raise (NOMOVE "no child tree")
        | LOC((NODE (first::treeList)), zipper) -> LOC(first, HAND([], zipper, treeList));;


(*
let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"];
                      LEAF "+";
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]],
                TOP)

let loc2 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
                HAND(
                     []
                     TOP
                     [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]
                     )
                )

let loc3 = LOC (LEAF"a",
                HAND(
                    [], 
                    HAND(
                        []
                        TOP
                        [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]
                     ),
                     [LEAF "*"; LEAF "b"]
                     ))
NODE [
    NODE [LEAF a; LEAF *; LEAF b];
    LEAF +;
    NODE [LEAF c; LEAF *; LEAF d]
]

LOC (
    LEAF *,
	HAND(
        [LEAF c],
	    HAND(
                [LEAF +; NODE [LEAF a; LEAF *; LEAF b]],
	            TOP,
	            []
            ),
	    [LEAF d]
        )
)
*)
