(* HW 1-7 / 2007-11603 / ��ǻ�Ͱ��к� / �̿��� *)

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
				| CONNECT (m1, m2) -> false (* �� ��쿡 �����ϴ� ���� ����. *)
				  (* covers�� ������ AREA( n, AREA (n, ....   STATION " ")... *)
				  (* covers�� CONNECT�� ������ �� ����. *)
	in

	let rec findStation (cur, covers) =
		match cur with STATION n -> valify (n, covers)
			     | AREA (n, m_) -> findStation (m_, AREA (n, covers))
			     | CONNECT (m1, m2) -> 
				(findStation (m1, covers)) && (findStation (m2, covers))
	in	

	findStation (m, STATION " ")