type name = string
type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro

let rec checkName (n,mtr) =
	match mtr with
	| STATION s ->
		if n=s then true
		else false
	| AREA (n1,mtr1) ->
		checkName (n1,mtr1)
	| CONNECT (m1, m2) ->
		(match (m1, m2) with
		| (_, AREA(nm1, mt1)) ->
			checkName (n, mt1)
		| (AREA(nm2,mt2), _)->
			checkName (n, mt2)
		| (_, _) ->
			checkName (n, m1) || checkName (n, m2)
		)
		

let rec checkMetro m =
	match m with
	| AREA (n, mtr) ->
		checkName (n,mtr)
	| _ -> false

let a81 = checkMetro (AREA("a", STATION "a"))
let a82 = checkMetro (AREA("a", AREA("a", STATION "a")))
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))
let a85 = checkMetro (AREA("a", STATION "b"))
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
