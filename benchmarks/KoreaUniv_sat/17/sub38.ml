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

type env2 = (string * bool) list
let empty_env2 = []
let empty_env3 = []
let extend_env2 (x,v) e = (x,v)::e
let rec apply_env2 x env = match env with
                      | [] -> apply_env2 x (extend_env2 (x,true) env)
                      | (y,v)::tl -> if x = y then v 
                                     else apply_env2 x tl

let rec help_sat : formula -> env2 -> bool
= fun f env -> match f with
                | True -> true
                | False -> false
                | Var(x) -> apply_env2 x env
                | Neg(x) -> if (help_sat x env) = true then false
                            else true
                | And(x,y) -> (help_sat x env) && (help_sat y env)
                | Or(x,y) -> (help_sat x env) || (help_sat y env)
                | Imply(x,y) -> if (help_sat x env) = false then true
                                else if (help_sat y env) = true then true
                                else false 
                | Iff(x,y) -> if (help_sat x env) = true && (help_sat y env) = true then true
                              else if (help_sat x env) = false && (help_sat y env) = false 
                                then true
                              else false

let rec sat : formula -> bool
= fun f -> let rec help_sat2 f env = 
             match f with
             | True -> true
             | False -> false
             | Var(x) -> false
             | Neg(x) -> if (help_sat2 x env) = true then false
                         else true
             | And(x,y) -> (help_sat2 x env) && (help_sat2 y env)
             | Or(x,y) -> (help_sat2 x env) || (help_sat2 y env)
             | Imply(x,y) -> if (help_sat2 x env) = false then true
                             else if (help_sat2 y env) = true then true
                             else false
             | Iff(x,y) -> if (help_sat2 x env) = true && (help_sat2 y env) = true then true
                           else if (help_sat2 x env) = false && (help_sat2 y env) = false then true
                           else false
            in (help_sat f empty_env2) || (help_sat2 f empty_env3)
