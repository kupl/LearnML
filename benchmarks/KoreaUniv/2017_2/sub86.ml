(*problem 1*)
type btree = Empty | Node of int*btree*btree
 
let rec mirror: btree -> btree
= fun t -> 
match t with
| Empty -> Empty
| Node (x, left, right) -> Node (x, mirror right, mirror left)
 
(*problem 2*) 
type nat = ZERO | SUCC of nat
 
let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n2 with
| ZERO -> n1
| SUCC n2_decrease -> natadd (SUCC n1) n2_decrease
 
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
| ZERO -> ZERO
| SUCC n2_decrease -> natadd n1 (natmul n1 n2_decrease) 
 
let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
| ZERO -> SUCC ZERO
| SUCC n2_decrease -> natmul n1 (natexp n1 n2_decrease)
 
(*problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> 
let rec make_env f env =
        match f with
        | True -> env
        | False -> env
        | Var x ->  let rec put_var x env =
                        match env with
                         | [] -> x::env
                         | hd::tl -> hd::(put_var x tl)
                        in put_var x env
        | Neg (x) -> make_env x env
        | And (x, y) -> make_env x (make_env y env)
        | Or (x, y) -> make_env x (make_env y env)
        | Imply (x, y) -> make_env x (make_env y env)
        | Iff (x, y) -> make_env x (make_env y env)
in
let rec is_other_element env =
match env with
| [] -> false
| hd::tl -> if List.mem hd env = true then true else (is_other_element tl)
in
let rec is_same_element env =
match env with
| [] -> true
| hd::tl -> if List.mem hd env = true then true else (is_other_element tl)
in
let rec making_cases env case_env =
        match env with
        | [] -> case_env
        | hd::tl -> making_cases tl ((hd, true)::case_env)
in 
let rec eval f env =
        match f with
        | True -> true
        | False -> false
        | Var x -> let rec get_var x env = (match env with
                                          | [] -> raise (Failure "something go wrong!")
                                         | (a, bvalue)::tl -> if x = a then bvalue else get_var x tl)
                                        in get_var x env
        | Neg (x) -> (match eval x env with
              | true -> false
              | false -> true)
        | And (x, y) -> (match (eval x env, eval y env) with
              | (true, true) -> true
              | (_, false) -> false
              | (false, _) -> false)
        | Or (x, y) -> (match (eval x env, eval y env) with
              | (true, true) -> true
              | (true, false) -> true
              | (false, true) -> true
              | (false, false) -> false)
        | Imply (x, y) -> (match (eval x env, eval y env) with
              | (true, true) -> true
              | (true, false) -> false
              | (false, true) -> true
              | (false, false) -> true)
        | Iff (x, y) -> (match (eval x env, eval y env) with
              | (true, true) -> true
              | (true, false) -> false
              | (false, true) -> false
              | (false, false) -> true)
in
let element_env = (make_env f [])
in
let case_env = (making_cases (make_env f []) [])
in
let rec eval_f f case_env =
if eval f case_env = true then true
else let rec is_needed_moretest env =
        match env with
        | [] -> false
        | (x, bvalue)::tl -> if bvalue = true then true else is_needed_moretest tl
      in if is_needed_moretest case_env = true then 
      let rec plus_cases env =
        match env with
        | (x, true)::tl -> (x, false)::tl
        | (x, false)::tl -> (x, true)::(plus_cases tl)
        in eval_f f (plus_cases case_env) else false
in 
if is_same_element element_env = false then true
else eval_f f case_env

(*problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
 
let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
| Const n -> Const 0
| Var a -> let is_x a x = if a = x then Const 1 else Const 0 in is_x a x
| Power (a, n) -> if a = x then let diff_var (a, n) = Times[Const n; Power(a, n - 1)] in diff_var (a,n) else Const 0
| Times [] -> Const 0
| Times (hd::tl) -> if tl = [] then diff (hd, x) else Sum[Times(diff(hd, x)::tl); Times [hd;diff(Times tl, x)]]
| Sum [] -> Const 0
| Sum (hd::tl) -> if tl = [] then diff (hd, x) else Sum[diff(hd, x); diff(Sum tl, x)]
 
(*problem 5*)
 type exp =
   X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec calculator : exp -> int
= fun e ->
match e with
| INT n -> n
| ADD (a,b) -> calculator a + calculator b
| SUB (a,b) -> calculator a - calculator b
| MUL (a,b) -> calculator a * calculator b
| DIV (a,b) -> if calculator b = 0 then raise (Failure "divided by zero")   
  else calculator a / calculator b
| X -> raise (Failure "no value in the X") 
| SIGMA (a,b,exp) -> 
let rec eval exp env  = 
match (exp, env) with
| (INT n, _) -> n
| (ADD (a, b), env) -> eval a env + eval b env
| (SUB (a, b), env) -> eval a env - eval b env
| (MUL (a, b), env) -> eval a env * eval b env
| (DIV (a, b), env) -> if eval b env = 0 then raise (Failure "divided by zero")
                       else eval a env / eval b env
| (X, []) -> raise (Failure "no value in the X") 
| (X, hd::tl) -> hd 
| (SIGMA (a, b, ex), env) -> let s = eval a env in let e = eval b env in
    if s > e then raise (Failure "starting point is bigger than the ending point")
    else if s = e then (eval ex (s::env))
    else (eval ex (s::env)) + (eval (SIGMA (INT (s+1),INT e, ex)) env)
in
eval (SIGMA (a,b,exp)) []
 
(*problem 6*)
 
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
 
let rec balanced : mobile -> bool
= fun m -> 
let rec sum_weight br =
match br with
| SimpleBranch (l, w) -> w
| CompoundBranch (l, (sub1, sub2)) -> sum_weight sub1 + sum_weight sub2
in
let rec mul_weight br =
match br with
| SimpleBranch (l, w) -> l * w
| CompoundBranch (l, (sub1, sub2)) -> l * (sum_weight sub1 + sum_weight sub2)
in
match m with
| (topleft, topright) -> match topleft with
		| SimpleBranch (left_leng, left_weight) -> 
			(match topright with
			| SimpleBranch (right_leng, right_weight) -> if mul_weight topleft = mul_weight topright then true
							else false
			| CompoundBranch (right_leng, (left_sub, right_sub)) -> if (balanced (left_sub, right_sub) = true) then 
									if mul_weight topleft = mul_weight topright then true else false
								else false)
		| CompoundBranch (left_leng, (left_sub1, right_sub1)) -> if (balanced (left_sub1, right_sub1) = false) then false 
							else
							(match topright with
							| SimpleBranch (right_leng,right_weight) -> if mul_weight topleft = mul_weight topright then true
											else false
							| CompoundBranch (right_leng, (left_sub2, right_sub2)) -> if (balanced (left_sub2, right_sub2) = false) then false
											else if mul_weight topleft = mul_weight topright then true
											else false)
 
(*problem 7*)
 
type digit = ZERO | ONE
type bin = digit list
 
let bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec bi_to_dec bexp n =
match bexp with
| [] -> 0
| hd::[] -> if hd=ZERO then n*2+0 else n*2+1
| hd::tl -> if hd=ZERO then bi_to_dec tl (n*2) else bi_to_dec tl (n*2+1)
in
let rec dec_to_bi dexp b_list =
if dexp = 1 then [ONE]@b_list
else if dexp = 0 then [ZERO]@b_list
else if dexp mod 2 = 1 then dec_to_bi ((dexp-1)/2) [ONE]@b_list
else dec_to_bi (dexp/2) [ZERO]@b_list 
in
let ex1 = bi_to_dec b1 0 in let ex2 = bi_to_dec b2 0 in
let dec_result = ex1 * ex2 
in
dec_to_bi dec_result []