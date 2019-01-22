type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec envmt : exp -> string list
= fun e ->
let lst =[] in
match e with 
V v -> lst
|P (v, expr) -> v::(envmt expr)@lst
|C (exp1, exp2) -> (envmt exp1)@(envmt exp2)@lst

let rec exist : exp * string list -> int 
= fun (e, lst) ->
match e with
V v -> (match lst with
	[] -> 0
	|hd::tl -> if hd = v then 1 + (exist (e,tl)) else (exist (e, tl)))
|P (v, exp1) -> (exist (exp1, lst))
|C (exp1, exp2) -> (exist (exp1, lst)) +  (exist (exp2, lst))

let check : exp -> bool
=fun e -> 
let env = (envmt e) in
if (exist (e, env)) = 0 then false
else if List.length env > (exist (e, env)) then false
else true
