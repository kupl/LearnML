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

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

let rec add_var
= fun v env -> match env with 
  [] -> [(v,False)]
| hd::tl -> 
    let str = fst hd in
      if str = v then env
      else hd::(add_var v tl)

let rec lookup_var
= fun f env -> match f with
  True -> env
| False -> env
| Var v -> add_var v env
| Neg f1 -> lookup_var f1 env
| And (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Or (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Imply (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Iff (f1,f2) -> lookup_var f2 (lookup_var f1 env)

let rec find_val
= fun v env -> match env with 
  [] -> False
| hd::tl -> 
    let str = fst hd in
      if str = v then snd hd
      else find_val v tl

let rec cal_formula
= fun f env -> match f with
  True -> true
| False -> false
| Var v -> cal_formula (find_val v env) env
| Neg f1 -> not(cal_formula f1 env)
| And (f1,f2) -> (cal_formula f1 env) && (cal_formula f2 env)
| Or (f1,f2) -> (cal_formula f1 env) || (cal_formula f2 env)
| Imply (f1,f2) -> (not(cal_formula f1 env)) || (cal_formula f2 env)
| Iff (f1,f2) -> cal_formula ( Or( And(f1,f2), Neg( Or(f1,f2)))) env

let rec gen_states
= fun f envs enve -> match envs with
  [] -> cal_formula f enve
| hd::tl -> 
    let cf = gen_states f tl (((fst hd),(False))::enve) in
    let ct = gen_states f tl (((fst hd),(True))::enve) in
      if (cf = true) || (ct = true) then true
      else false

let rec sat : formula -> bool
= fun f -> 
  let env = lookup_var f [] in
  gen_states f env []
  
