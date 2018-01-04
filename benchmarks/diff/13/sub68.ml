type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list
exception InvalidArgument
let rec diff (arg,var) = match arg with
| (CONST a)->CONST 0
| (VAR a)->
if a = var then CONST 1
else CONST 0
| POWER (a,b) ->
if a = var then TIMES(CONST b::POWER(a, b-1)::[])
else CONST 0
| (TIMES a) -> 
if List.length(a) = 0 then raise InvalidArgument
else if List.length(a) = 1 then diff(List.hd(a), var)
else
SUM(TIMES(diff(List.hd(a),var)::List.tl(a))::TIMES(List.hd(a)::diff(TIMES(List.tl(a)),var)::[])::[])
| (SUM a) -> 
if List.length(a) = 0 then raise InvalidArgument
else if List.length(a) = 1 then diff(List.hd(a), var)
else
let diff2 x = diff(x, var)
in SUM (List.map diff2 a)