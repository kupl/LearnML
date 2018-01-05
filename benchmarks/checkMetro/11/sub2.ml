type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro

and name = string


let rec checkMetro metro_input = 
	let 
		rec listStation m_input =
			match m_input with
			| STATION(n) -> [n]
			| AREA(n,m) -> deleteAll (listStation(m), n)
			| CONNECT(m1,m2) -> listStation(m1)@listStation(m2)
	and
		deleteAll (list_input,target) =
			match list_input with
			| l::remain_list -> 
				if l=target then deleteAll(remain_list, target) 
				else l::deleteAll(remain_list, target)
			| [] -> []
	in
		if listStation(metro_input) = [] then true else false