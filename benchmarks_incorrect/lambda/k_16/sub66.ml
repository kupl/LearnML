type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec helpcheck : lambda -> lambda list
= fun lambda ->
match lambda with
| P(a,b) -> (V a)::helpcheck(b)
| C(a,b) -> helpcheck(a)@helpcheck(b)
| V(a) -> []

let rec varcheck : lambda -> lambda list
= fun lambda ->
match lambda with
| V(a) -> (V a)::[]
| C(a,b) -> varcheck(a)@varcheck(b)
| P(a,b) -> varcheck(b)

let rec findvar : lambda -> lambda list
= fun lambda ->
match lambda with 
| V(a) -> (V a)::[]
| P(a,b) -> (V a)::findvar(b)
| C(a,b) -> findvar(a)@findvar(b)

let rec helpcheck2 : lambda * lambda -> lambda list
= fun (ex, lambda) ->
match ex with
| P(a,b) -> if b=lambda then (V a)::helpcheck2(b,lambda) else (V a)::helpcheck2(b,lambda)
| C(a,b) -> if a=lambda then [] else if b=lambda then helpcheck2(a,lambda) else helpcheck2(a,lambda)@helpcheck2(b,lambda)
| V(a) -> []

let rec confi : lambda list * lambda -> bool
= fun (a, b) ->
match a with
|hd::tl -> if hd = b then true else confi(tl,b) 
|_ -> false

let rec last : lambda list * lambda -> bool
= fun (a,b) ->
match a with
| hd::tl -> confi(helpcheck2(b, hd), hd) && last(tl, b)
| [] -> true

let rec check : lambda -> bool
  = fun lambda ->  (* TODO *)
	if last(varcheck(lambda), lambda) then true else false

