type metro = STATION of name
			  | AREA of name * metro
			  | CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec checkMetro_ m (al, nl) =
		match m with
			| STATION n -> (List.for_all (fun x -> List.mem x al) (n::nl))
			| AREA (n, m_) -> checkMetro_ m_ (n::al, nl)
			| CONNECT (m_1, m_2) -> (checkMetro_ m_1 (al, nl)) && (checkMetro_ m_2 (al, nl)) in
	checkMetro_ m ([], [])
	
(* TEST SET *)
(*
checkMetro (STATION "a");;
checkMetro (AREA("a", STATION "a"));;
checkMetro (AREA("a", AREA("b", AREA ("c", CONNECT(STATION "c", STATION "c")))));;
checkMetro (AREA("b", CONNECT(STATION "a", STATION "b")));;
checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", AREA ("b", STATION "b")))));;
checkMetro (AREA("a", AREA("a", STATION "a")));;
checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));;
checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));;
checkMetro (AREA("a", STATION "b"));;
checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));;
checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))));;
*)