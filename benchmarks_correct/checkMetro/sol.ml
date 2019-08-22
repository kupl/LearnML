type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec is_mem : var list -> var -> bool
= fun vars var ->
  match vars with
  | [] -> false
  | hd::tl -> if (hd = var) then true else is_mem tl var

let rec sub_check : lambda -> var list -> bool
= fun met vars ->
  match met with
  | V n -> is_mem vars n
  | P (n, m) -> sub_check m (n::vars)
  | C (m1, m2) -> (sub_check m1 vars) && (sub_check m2 vars)

let rec check : lambda -> bool
= fun met ->
  sub_check met []