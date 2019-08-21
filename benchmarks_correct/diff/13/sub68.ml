type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list
exception InvalidArgument
let rec diff (arg,var) = match arg with
| (Const a)->Const 0
| (Var a)->
if a = var then Const 1
else Const 0
| Power (a,b) ->
if a = var then Times(Const b::Power(a, b-1)::[])
else Const 0
| (Times a) -> 
if List.length(a) = 0 then raise InvalidArgument
else if List.length(a) = 1 then diff(List.hd(a), var)
else
Sum(Times(diff(List.hd(a),var)::List.tl(a))::Times(List.hd(a)::diff(Times(List.tl(a)),var)::[])::[])
| (Sum a) -> 
if List.length(a) = 0 then raise InvalidArgument
else if List.length(a) = 1 then diff(List.hd(a), var)
else
let diff2 x = diff(x, var)
in Sum (List.map diff2 a)