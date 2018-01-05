(*real code start*)
module Sset = Set.Make(String)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec unresolved_symbol(m : metro) : Sset.t = 
match m with
|STATION(str) -> Sset.singleton str
|AREA(str, m2) ->
 let temp_set = unresolved_symbol(m2) in
 Sset.remove str temp_set
|CONNECT(m1, m2) ->
 let s1 = unresolved_symbol(m1) in
 let s2 = unresolved_symbol(m2) in
 Sset.union s1 s2

let rec checkMetro (m : metro) : bool = Sset.is_empty(unresolved_symbol(m))
(*end*)
