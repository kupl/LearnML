type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro m=
	let rec make_area_list m=
		match m with 
			| STATION(n)-> []
			| AREA(n, m1)-> n::(make_area_list m1)
			| CONNECT(m1, m2)-> (make_area_list m1)@(make_area_list m2)
	in
	let rec match_list_with_station m l=
		match m with
			| AREA(n,m1)->match_list_with_station m1 l
			| CONNECT(m1, m2)->(match_list_with_station m1 l)&&(match_list_with_station m2 l)
			| STATION(n)->
				if(List.exists (fun x->(x=n)) l ) then true
				else false
	in
	match_list_with_station m (make_area_list m)
	
		