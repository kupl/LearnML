
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->  (* TODO *)
	if last(varcheck(exp), exp) then true else false
and helpcheck : exp -> exp list
= fun exp ->
match exp with
| P(a,b) -> (V a)::helpcheck(b)
| C(a,b) -> helpcheck(a)@helpcheck(b)
| V(a) -> []
and varcheck : exp -> exp list
= fun exp ->
match exp with
| V(a) -> (V a)::[]
| C(a,b) -> varcheck(a)@varcheck(b)
| P(a,b) -> varcheck(b)
and findvar : exp -> exp list
= fun exp ->
match exp with 
| V(a) -> (V a)::[]
| P(a,b) -> (V a)::findvar(b)
| C(a,b) -> findvar(a)@findvar(b)
and helpcheck2 : exp * exp -> exp list
= fun (ex, exp) ->
match ex with
| P(a,b) when b=exp -> (V a)::helpcheck2(b,exp)
| P(a,b) -> (V a)::helpcheck2(b,exp)
| C(a,b) when a=exp -> []
| C(a,b) when b=exp -> helpcheck2(a,exp)
| C(a,b) -> helpcheck2(a,exp)@helpcheck2(b,exp)
| V(a) -> []
and confi : exp list * exp -> bool
= fun (a, b) ->
match a with
|hd::tl when hd = b -> true 
|hd::tl -> confi(tl,b) 
|_ -> false
and last : exp list * exp -> bool
= fun (a,b) ->
match a with
| hd::tl -> confi(helpcheck2(b, hd), hd) && last(tl, b)
| [] -> true
