

(*Ex7*)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
  and name = string

exception FalsE
exception Error of string
 
let (check : string list * string -> bool) =
	fun(lst, name) ->
	let isThere str = (name=str)
	in if (("")!=(List.find isThere lst))
				then true 
				else raise(Error "why does this happen!") 

let resist(lst, name) =
	if check(lst,name) then lst
						else (name::lst)

let (checkMetro : metro -> bool)
	= fun met ->
	let rec subchecker (lst, met) =
			match met with STATION name ->if check(lst,name) then true
															else raise FalsE
						 | AREA(name,metro) ->
							 subchecker(resist(lst,name),metro)
						 | CONNECT(met1,met2) -> subchecker(lst,met1) &&
						 subchecker(lst,met2)
	in subchecker([],met)

