module type ZEXPR =
sig
exception Error of string
type id = string
type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list
| VAR of id
| LET of id * expr * expr
type environment
type value
val emptyEnv: environment
val eval: environment * expr -> value
end

module Zexpr : ZEXPR = struct
exception Error of string
type id = string
type expr = NUM of int
|PLUS of expr * expr
|MINUS of expr * expr
|MULT of expr * expr
|DIVIDE of expr * expr
|MAX of expr list
|VAR of id
|LET of id * expr * expr
type environment = (id * int) list 
type value = int

let emptyEnv = []

let eval (env, expr) =
let finish ans =
print_int(ans);
print_string("\n");
ans
in
let rec real_eval env expr = try(
let rec iter env lst =
match lst with
|[] -> []
|hd::tl -> (real_eval env hd)::(iter env tl)
in
match expr with
|NUM i -> i
|PLUS (exp1, exp2) -> (real_eval env exp1) + (real_eval env exp2)
|MINUS (exp1, exp2) -> (real_eval env exp1) - (real_eval env exp2)
|MULT (exp1, exp2) -> (real_eval env exp1) * (real_eval env exp2)
|DIVIDE (exp1, exp2) -> (real_eval env exp1) / (real_eval env exp2)
|MAX [] -> 0
|MAX lst -> List.hd (List.sort (fun x y -> if(x>y) then 0 else 1) (iter env lst))
|VAR id -> (List.assoc id env)
|LET (id,exp1,exp2) -> (let item = (real_eval env exp1) in
			(real_eval ((id,item)::env) exp2))
) with Division_by_zero -> raise (Error "Division_by_zero")
|e -> raise (Error "Unbounded Variable")
in
(finish (real_eval env expr))
end


