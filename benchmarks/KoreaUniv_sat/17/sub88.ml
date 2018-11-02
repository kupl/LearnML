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
