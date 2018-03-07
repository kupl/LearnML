
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec var_find exp =
match exp with
|V a -> []
|P (a, exp1) -> a::(var_find exp1)
|C (exp1, exp2) -> (var_find exp1)@(var_find exp2)

let rec exp_find exp =
match exp with
|V a -> [a]
|P (a, exp1) -> exp_find exp1
|C (exp1,exp2) -> (exp_find exp1)@(exp_find exp2)

let rec find s l =
match l with
|[] -> false
|hd::tl -> if s = hd then true else find s tl

let rec find_match l1 l2 = 
match l2 with
|[] -> true
|hd::tl -> (find hd l1)&&(find_match l1 tl)

 let check : exp -> bool
= fun exp ->
find_match (var_find exp) (exp_find exp)
