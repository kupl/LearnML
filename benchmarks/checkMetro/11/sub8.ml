(* programming language, exercise07, 200611810 *)

type metro = STATION of name | AREA of name*metro | CONNECT of metro*metro
and name = string

let rec checkMetro (met) =
	match met with
	| STATION (n) -> true
	| CONNECT (met1, met2) -> 
		 checkMetro(met1) && checkMetro(met2)
	| AREA (stn0, met0) ->
		let check (a, b) =
			if a=b then true
			else false
		in
				
		let rec chstn (stn, met) =
			match met with	
			|STATION (stn') -> check(stn, stn')
			|CONNECT (n1, n2) -> chstn(stn,n1)|| chstn(stn,n2) 
			|AREA (st, n3) -> check(stn, st) || chstn(stn,n3)
		in
		
		chstn(stn0,met0)

		

	
 
