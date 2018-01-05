(* Exercise 8 *)
type item = string

type tree =
	LEAF of item
	| NODE of tree list

type zipper =
	TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
	match loc with
		LOC (t, TOP) ->
			raise (NOMOVE "Zipper locates its top")
		| LOC (t, HAND (l :: left, up, right)) ->
			LOC (l, HAND (left, up, t :: right))
		| LOC (t, HAND ([], up, right)) ->
			raise (NOMOVE "There is no node which places left side of zipper")

let goRight loc =
	match loc with
		LOC (t, TOP) ->
			raise (NOMOVE "Zipper locates its top")
		| LOC (t, HAND (left, up, r :: right)) ->
			LOC (r, HAND (t :: left, up, right))
		| LOC (t, HAND (left, up, [])) ->
			raise (NOMOVE "There is no node which places right side of zipper")

let goUp loc =
	match loc with
		LOC (t, TOP) ->
			raise (NOMOVE "Zipper locates its top")
		| LOC (t, HAND (left, up, right)) ->
			LOC (NODE ((List.rev left) @ [t] @ right), up)

let goDown loc =
	match loc with
		LOC (LEAF (leaf), zipper) ->
			raise (NOMOVE "Zipper locates its bottom")
		| LOC (NODE ([]), zipper) ->
			raise (NOMOVE "Tree is null so cannot move down")
		| LOC (NODE (head :: tail), zipper) ->
			LOC (head, HAND ([], zipper, tail))
			
