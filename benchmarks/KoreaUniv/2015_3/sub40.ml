(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let len b = match b with
    | SimpleBranch(length, _) -> length
    | CompoundBranch(length, _) -> length
  ;;

  let left b = match b with
  | (left, _) -> left
  ;;
 
  let right b = match b with
  | (_, right) -> right
  ;;

  let rec calc b = match b with
    | SimpleBranch (len, weight) -> weight
    | CompoundBranch (len, mobile) -> (calc (left mobile)) + (calc (right
    mobile))
  ;;
  let balanced : mobile -> bool
  =fun (lb,rb) -> ((len lb) * (calc lb) == (len rb) * (calc rb))
  ;;

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec free_var e = match e with
  | V(v) -> [v]
  | P(v, t) -> List.filter (fun x -> x <> v) (free_var t)
  | C(t, u) ->
      let f_t = free_var t in
      let f_u = free_var u in
        List.append f_t (List.filter (fun x -> not (List.mem x f_t)) f_u)

  let check : exp -> bool
  =fun e -> match (free_var e) with
  |[] -> true
  |_::_ -> false
  ;;
end

(***********************************)
(**            Problem 3          **)
(***********************************)
module Problem3 = struct
  type program = exp
  and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string

  type value = Int of int | Bool of bool
             | Procedure of var * exp * env
             | RecProcedure of var * var * exp * env
  and env = var -> value

  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x

  let rec eval : exp -> env -> value
  = fun exp env ->
    match exp with
    | CONST(x) -> Int x
    | VAR(x) -> apply_env env x
    | ADD(x, y) ->
        let v1 = eval x env in
        let v2 = eval y env in
        (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 + n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | SUB(x, y) ->
        let v1 = eval x env in
        let v2 = eval y env in
        (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 - n2)
        | _ -> raise (Failure "Type Error:non-numeric values"))
    | ISZERO(e) ->
        (match eval e env with
        | Int n when n = 0 -> Bool true
        | _ -> Bool false)
    | IF(e1,e2,e3) ->
        (match eval e1 env with
        | Bool true -> eval e2 env
        | Bool false -> eval e3 env
        | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | LET (x, e1, e2) ->
        let v1 = eval e1 env in
          eval e2 (extend_env (x, v1) env)
    

  let run : program -> value
  = fun pgm -> eval pgm empty_env
  ;;
end

(***********************************)
(**            Problem 4          **)
(***********************************)

module Problem4 = struct
  type program = exp
  and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string

  type nl_program = nl_exp
  and nl_exp =
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp
    | NL_CALL of nl_exp * nl_exp

  let translate : program -> nl_program
  =fun pgm -> NL_CONST 0 (* TODO *)
end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int
                | NL_Bool of bool
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 (* TODO *)
end
