type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let rec checkMetro mvar = let rec find (l, n1) = match l with
						h1::t1 -> if (h1 = n1) then true else find (t1, n1)
						|[] -> false in
			  let l1 = ref [] in
			  let rec check m1 = match m1 with
				
				CONNECT(m1,m2) -> check m1 && check m2
				|AREA(n1, m1) -> l1 := n1::!l1  ; check m1
  				|STATION(n2) ->   find (!l1, n2)  in
  			  check mvar;;