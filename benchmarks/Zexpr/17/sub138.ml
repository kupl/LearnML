(* 컴퓨터공학과/2017-34165/김성국/2-7 *)
module type ZEXPR = sig
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

  val emptyEnv : environment
  val eval : environment * expr -> value

  val print_value : value -> unit
end

module Zexpr : ZEXPR = struct
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
  type value = int
  type environment = id -> value

(*  let emptyEnv = fun x -> raise (Error ("free variable: " ^ x)) *)
  let emptyEnv = fun x -> raise (Error "FreeVariable")
  let rec eval (env, expr) =
    match expr with
    | NUM i -> i
    | PLUS(e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS(e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT(e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE(e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX lst ->
      (match (List.rev_map (fun e -> eval (env, e)) lst) with
       | [] -> 0
       | hd::tl -> List.fold_left max hd tl)
    | VAR x -> env(x)
    | LET(x, e1, e2) ->
      let v1 = (eval (env, e1)) in
      eval ((fun id -> if id = x then v1 else env(id)), e2)

  let print_value = print_int
end
