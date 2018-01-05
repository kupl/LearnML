type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
			and name = string

let checkMetro metro =
		let rec loop mt li =
			match mt with
			| STATION name -> List.exists (fun x -> x=name) li
			| AREA (name, metro) -> loop metro (li @ [name])
			| CONNECT (mt1, mt2) -> (loop mt1 li) && (loop mt2 li) in
				loop metro []

