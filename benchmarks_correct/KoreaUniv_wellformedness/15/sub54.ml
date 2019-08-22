type lambda = 
V of var
|P of var * lambda
|C of lambda * lambda
and var = string;;

let rec isinlist : ((string list) * string) -> bool
= fun (f, s) ->
match f with
|[] -> false
|hd::tl -> if hd = s then true else isinlist(tl, s);;

let rec check2 : (lambda * (string list)) -> bool
= fun (e, state) -> match e with
|V str -> isinlist (state, str)
|P (str, e) -> check2 (e, (str::state))
|C (e1, e2) -> (check2 (e1,state) && check2 (e2, state));;

let rec check : lambda -> bool
= fun e -> check2 (e, []);;

