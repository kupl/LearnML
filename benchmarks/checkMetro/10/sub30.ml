type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

module NameSet = Set.Make(struct type t = name let compare = compare end)

type t = NameSet.t

let rec checkMetro_sub metro_arg envset =
 match metro_arg with
 (STATION str) -> (NameSet.mem str envset)
 |(AREA (str, s_metro)) -> (checkMetro_sub s_metro (NameSet.add str envset)) 
 |(CONNECT (s_metro1, s_metro2)) -> (checkMetro_sub s_metro1 envset) && (checkMetro_sub s_metro2 envset)

let checkMetro metro_arg =
 (checkMetro_sub metro_arg NameSet.empty)

