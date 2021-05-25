type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check m=
	let rec make_area_list m=
		match m with 
			| V(n)-> []
			| P(n, m1)-> n::(make_area_list m1)
			| C(m1, m2)-> (make_area_list m1)@(make_area_list m2)
	in
	let rec match_list_with_station m l=
		match m with
			| P(n,m1)->match_list_with_station m1 l
			| C(m1, m2)->(match_list_with_station m1 l)&&(match_list_with_station m2 l)
			| V(n)->
				if(List.exists (fun x->(x=n)) l ) then true
				else false
	in
	match_list_with_station m (make_area_list m)
	
		