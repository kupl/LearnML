(* HW 1-7 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro

and name = string


let checkMetro m =

	let rec valify (name_, covers) =
		match covers with STATION n -> false
		                | AREA (n, m_) -> ( if n = name_ then
							true
						    else
							valify (name_, m_) )
				| CONNECT (m1, m2) -> false (* 이 경우에 도달하는 일은 없다. *)
				  (* covers는 무조건 AREA( n, AREA (n, ....   STATION " ")... *)
				  (* covers가 CONNECT를 포함할 순 없다. *)
	in

	let rec findStation (cur, covers) =
		match cur with STATION n -> valify (n, covers)
			     | AREA (n, m_) -> findStation (m_, AREA (n, covers))
			     | CONNECT (m1, m2) -> 
				(findStation (m1, covers)) && (findStation (m2, covers))
	in	

	findStation (m, STATION " ")