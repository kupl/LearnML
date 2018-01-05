type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro m =
	let rec checkElementInList (elem, lst) =
		match lst with
		| hd::tl -> if (elem = hd) then true else checkElementInList(elem, tl)
		| [] -> false
		in
	let rec subset inner_list outer_list = 
		match inner_list with 
		| hd::tl -> if( checkElementInList(hd, outer_list)) then (subset tl outer_list) else false
		| [] -> true
		in
	let rec makeAreaList m_eq a_lst =
		match m_eq with
		| STATION x -> a_lst
		| AREA(x, y) -> makeAreaList y (x::a_lst)
		| CONNECT(x, y) -> (makeAreaList x a_lst) @ (makeAreaList y [])
		in
	let rec makeStationList m_eq s_lst = 
		match m_eq with 
		| STATION x -> x::s_lst
		| AREA(x, y) -> makeStationList y s_lst
		| CONNECT(x, y) -> (makeStationList x s_lst) @ (makeStationList y [])
		in
	subset (makeStationList m []) (makeAreaList m [])

