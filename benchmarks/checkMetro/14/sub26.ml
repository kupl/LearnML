(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 2 - 1 *)
type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

type passlist = ONE of string
	| LIST of passlist * string
	
let rec stationCheck : passlist * string -> bool =
	fun(l, s) ->
	match l with
	| ONE(a) -> a = s
	| LIST(a, b) -> (b = s) || stationCheck(a, s)
	
let rec checkName : passlist * metro -> bool =
	fun(l, m) ->
	match m with
	| STATION(a) -> stationCheck(l, a)
	| AREA(a, b) -> checkName(LIST(l, a), b)
	| CONNECT(a, b) -> checkName(l, a) && checkName(l, b)
	
let rec checkMetro : metro -> bool =
	fun m ->
	match m with
	| STATION(a) -> false
	| AREA(a, b) -> checkName(ONE a, b)
	| CONNECT(a, b) -> checkMetro a && checkMetro b
