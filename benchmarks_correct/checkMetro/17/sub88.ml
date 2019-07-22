type metro = 
	STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec isin : (name * name list) -> bool = fun (id, nlist) ->
	match nlist with
	| [] -> false
	| hd::tl -> if hd = id then true else isin (id, tl)

let rec checkList : (metro * name list) -> bool = fun (m, nlist) ->
	match m with
	| STATION id -> isin (id, nlist)
	| AREA (id, m_) -> checkList (m_, (id::nlist))
	| CONNECT (m1, m2) -> (checkList (m1, nlist)) && (checkList (m2, nlist))

let checkMetro : metro -> bool = fun m -> checkList (m, [])
