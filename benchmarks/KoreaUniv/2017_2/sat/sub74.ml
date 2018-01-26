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

type env = (string * formula) list

let rec find_env x env = 
  match env with
  | [] -> Var x
  | (y,v)::t -> if x = y then v
        else find_env x t

let update_env (x,v) e = (x,v)::e

let rec sat_env : formula -> env -> bool
= fun f env -> (match f with
           | True -> true
           | False -> false
           | Var str -> (match (find_env str env) with
                        | True -> true
                        | False -> false
                        | _ -> raise (Failure "Error"))
           | Neg form1 -> (match form1 with
                          | _ -> not (sat_env form1 env))
           | And (form1, form2) -> (sat_env form1 env)&&(sat_env form2 env)
           | Or (form1, form2) -> (sat_env form1 env)||(sat_env form2 env)
           | Imply (form1, form2) -> (match (sat_env form1 env) with
                                      | false -> true
                                      | true -> (sat_env form2 env))
           | Iff (form1, form2) -> if ((sat_env (Imply (form1, form2)) env)&&(sat_env (Imply (form2, form1)) env)) then true else false
           )


let rec var_check : formula -> env -> env
= fun f env -> (match f with
                | True | False -> env
                | Var str -> (match (find_env str env) with
                              | Var x -> (update_env (str, True) env)
                              | _ -> env)
                | Neg form1 -> var_check form1 env
                | And (form1, form2) | Or (form1, form2) | Imply (form1, form2) | Iff (form1, form2) -> (var_check form2 (var_check form1 env))
)

let bool_flop : (string * formula) -> (string * formula)
= fun env -> (match env with
            | (x, y) -> if (y=True) then (x, False) else (x, True))

let rec var_set : formula -> env -> env -> bool
= fun f first_env setting_env -> (match first_env with
                                  | [] -> (sat_env f setting_env)
                                  | (hd::tl) -> ((var_set f tl (setting_env@[hd]))||(var_set f tl (setting_env@[(bool_flop hd)])))
                                  )

let sat : formula -> bool
= fun f -> var_set f (var_check f []) []