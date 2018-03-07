type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec ifexist : var*string list-> bool
=fun (v,lst)->
match lst with
[] -> false
|h::t -> if(v=h) then true else ifexist(v,t)

let rec helpcheck : exp * string list -> bool
=fun (e,lst) ->
match e with
| V v-> ifexist(v,lst)
| P (v,ex) -> helpcheck(ex,v::lst)
| C (v1,v2) -> if(helpcheck(v1,lst)=true) then (if(helpcheck(v2,lst)=true) then true else false) else false

let check : exp -> bool
=fun e ->  (* TODO *)
