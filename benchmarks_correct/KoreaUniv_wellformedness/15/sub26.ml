type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string

let rec ifexist : var*string list-> bool
=fun (v,lst)->
match lst with
[] -> false
|h::t -> if(v=h) then true else ifexist(v,t)

let rec helpcheck : lambda * string list -> bool
=fun (e,lst) ->
match e with
| V v-> ifexist(v,lst)
| P (v,ex) -> helpcheck(ex,v::lst)
| C (v1,v2) -> if(helpcheck(v1,lst)=true) then (if(helpcheck(v2,lst)=true) then true else false) else false

let check : lambda -> bool
=fun e -> helpcheck (e,[]) (* TODO *)
