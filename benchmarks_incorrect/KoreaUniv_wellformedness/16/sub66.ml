type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec helpcheck : exp -> exp list
= fun exp ->
match exp with
| P(a,b) -> (V a)::helpcheck(b)
| C(a,b) -> helpcheck(a)@helpcheck(b)
| V(a) -> []

let rec varcheck : exp -> exp list
= fun exp ->
match exp with
| V(a) -> (V a)::[]
| C(a,b) -> varcheck(a)@varcheck(b)
| P(a,b) -> varcheck(b)

let rec findvar : exp -> exp list
= fun exp ->
match exp with 
| V(a) -> (V a)::[]
| P(a,b) -> (V a)::findvar(b)
| C(a,b) -> findvar(a)@findvar(b)

let rec helpcheck2 : exp * exp -> exp list
= fun (ex, exp) ->
match ex with
| P(a,b) -> if b=exp then (V a)::helpcheck2(b,exp) else (V a)::helpcheck2(b,exp)
| C(a,b) -> if a=exp then [] else if b=exp then helpcheck2(a,exp) else helpcheck2(a,exp)@helpcheck2(b,exp)
| V(a) -> []

let rec confi : exp list * exp -> bool
= fun (a, b) ->
match a with
|hd::tl -> if hd = b then true else confi(tl,b) 
|_ -> false

let rec last : exp list * exp -> bool
= fun (a,b) ->
match a with
| hd::tl -> confi(helpcheck2(b, hd), hd) && last(tl, b)
| [] -> true

let rec check : exp -> bool
  = fun exp ->  (* TODO *)
	if last(varcheck(exp), exp) then true else false

