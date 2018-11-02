(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

type sat_env = (string * bool) list

let rec sat_append_var x l =
        match l with
        | [] -> [x]
        | hd::tl -> if hd = x then l else hd::(sat_append_var x tl)

let rec sat_find_var f l =
        match f with
        | True -> l
        | False -> l
        | Var x -> sat_append_var x l
        | Neg (e) -> sat_find_var e l
        | And (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Or (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Imply (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Iff (e1, e2) -> 
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1

let rec sat_apply_env x env =
        match env with
        | [] -> raise (Failure "Error")
        | (y, v)::tl -> if x = y then v else sat_apply_env x tl

let rec sat_make_env_all_f : string list -> sat_env -> sat_env
= fun var r ->
        match var with
        | [] -> r
        | hd::tl -> sat_make_env_all_f tl ((hd, false)::r)

let rec sat_next_case env c =
        match c with
        | true ->
                (match env with
                | [] -> raise (Failure "No Next Case") 
                | (x, true)::tl -> (x, false)::(sat_next_case tl true)
                | (x, false)::tl -> (x, true)::tl)
        | false -> env

let rec sat_has_next_case env =
        match env with
        | [] -> false
        | (_, b)::tl -> if b then sat_has_next_case tl else true

let rec sat_eval f env =
        match f with
        | True -> true
        | False -> false
        | Var x -> sat_apply_env x env
        | Neg (e) ->
                (match sat_eval e env with
                | true -> false
                | false -> true)
        | And (e1, e2) ->
                (match sat_eval e1 env with
                | true -> sat_eval e2 env
                | false -> false)
        | Or (e1, e2) ->
                (match sat_eval e1 env with
                | true -> true
                | false -> sat_eval e2 env)
        | Imply (e1, e2) -> sat_eval (Or ((Neg e1), e2)) env
        | Iff (e1, e2) -> sat_eval (Or (And (e1, e2), Neg(Or (e1, e2)))) env

let rec sat_rec f env =
        if sat_eval f env then true
        else if sat_has_next_case env then sat_rec f (sat_next_case env true)
        else false
let sat : formula -> bool
= fun f -> sat_rec f (sat_make_env_all_f (sat_find_var f []) [])