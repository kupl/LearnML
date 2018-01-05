type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro*metro
and name=string

let rec innermetro: name list * metro -> bool =
	fun(area_list, input) ->
	match input with
	| CONNECT (a,b) -> innermetro(area_list, a) && innermetro(area_list, b)
	| AREA (a,b) -> 
		if (List.exists (fun x-> a=x) area_list) then innermetro(area_list, b)
		else innermetro(a::area_list, b)
	| STATION a ->
		if (List.exists (fun x-> a=x) area_list) then true
		else false

let checkMetro: metro -> bool =
	fun(input) ->
	let area_list:name list=[] in
	innermetro (area_list, input)
