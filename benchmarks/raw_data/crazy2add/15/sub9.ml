type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let get_head : crazy2 -> int = fun(c) ->
	match c with
	| NIL -> 0
	| ZERO _c -> 0
	| ONE _c -> 1
	| MONE _c -> -1

let make_crazy2 : int * crazy2 -> crazy2 = fun(i,c) ->
	if i > 0 then ONE(c)
	else if i < 0 then MONE(c)
	else ZERO(c)

let get_tail : crazy2 -> crazy2 = fun(c) ->
	match c with
	| NIL -> NIL
	| ZERO _c -> _c
	| ONE _c -> _c
	| MONE _c -> _c


let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun(c1, c2) ->
	let rec my_add : crazy2 * crazy2 * crazy2 -> crazy2 = fun(c1,c2,cr) ->
	(
		if c1 = NIL && c2 = NIL then cr
		else if c1 = NIL && cr = NIL then c2
		else if c1 = NIL then my_add(c2,cr,c1)
		else if c2 = NIL && cr = NIL then c1
		else if c2 = NIL then my_add(c1,cr,c2)
		else if get_head(c1)+get_head(c2)+get_head(cr) > 1 then make_crazy2(get_head(c1)+get_head(c2)+get_head(cr)-2, my_add(get_tail(c1), get_tail(c2), ONE(NIL)))
		else if get_head(c1)+get_head(c2)+get_head(cr) < -1 then make_crazy2(get_head(c1)+get_head(c2)+get_head(cr)+2, my_add(get_tail(c1), get_tail(c2), MONE(NIL)))
		else make_crazy2(get_head(c1)+get_head(c2)+get_head(cr), my_add(get_tail(c1), get_tail(c2), NIL))
	) in my_add(c1,c2,NIL)

