type metro =	 STATION of name
		|AREA of name * metro
		|CONNECT of metro * metro
and name = string

let rec list_finder: 'a list * 'a -> bool = fun(list_, element_) ->
        (
        match list_ with
            [] -> false
            | hd::tail -> ( if(hd = element_) then (true) else (list_finder(tail,element_)))
        )

let rec checkMetro_helper: metro * string list -> bool = fun(met,env) ->
(
          match met with
                STATION(st_name) -> list_finder(env,st_name)
                |AREA(ar_name,metro_) -> checkMetro_helper(metro_,ar_name::env )
                |CONNECT(metro1_,metro2_) -> checkMetro_helper(metro1_,env) && checkMetro_helper(metro2_,env)

)

let rec checkMetro: metro -> bool = fun(met)
-> 
	(
		checkMetro_helper(met,[])
	)



