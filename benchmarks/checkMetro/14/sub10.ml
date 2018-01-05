type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec exists (l, a) = 
        match l with
        | [] -> false
        | h::t -> exists (t, a) || (a = h)

let rec _checkMetro a metro = 
	match metro with
	| STATION name -> exists (a, name)
	| AREA (name, metro) -> _checkMetro (name::a) metro
	| CONNECT (metro1, metro2) -> _checkMetro a metro1 && _checkMetro a metro2

let checkMetro = fun a ->
         _checkMetro [] a;;