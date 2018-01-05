type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec my_check metro check =
	match metro with
	|STATION x -> List.mem x check
	|AREA (name, met) -> my_check met (check @ [name])
	|CONNECT (met1, met2) ->
		if (my_check met1 check)=true then my_check met2 check
		else false



let checkMetro m = my_check m []
