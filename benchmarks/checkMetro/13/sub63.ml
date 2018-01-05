type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let rec asdf met l1 = 
	match met with
	| STATION a -> if (List.mem a l1)=true then true else false
	| CONNECT (m1, m2) -> (asdf m1 l1)&&(asdf m2 l1)
	| AREA (ne, mt) -> (asdf mt (ne::l1))

let rec checkMetro m =
        match m with
        | STATION a -> false
        | AREA (name, metro) -> asdf m []
        | CONNECT (m1, m2) -> (asdf m1 [])&&(asdf m2 [])


