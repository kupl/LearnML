(* Homework 2 - Exercise 7
 * 2011-10492 Jaeyeong Yang *)
module type ZEXPR =
sig
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
  val emptyEnv: environment
  val eval: environment * expr -> value
  val print_value: value -> unit
end

module Zexpr: ZEXPR =
struct
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
  type environment = (id * value) list

  let emptyEnv: environment = []

  let rec eval: environment * expr -> value = fun (env, exp) ->
    let rec getValue: environment * id -> value = fun (e, x) ->
      match e with
      | [] -> raise (Error "FreeVariable")
      | (xx, n) :: t -> if xx = x then n else getValue (t, x)
    in
    let rec max: expr list -> value = fun es ->
      match es with
      | [] -> 0
      | e :: [] -> eval (env, e)
      | e1 :: l ->
        let v1 = eval (env, e1) in
        let v2 = max l in
        if v1 > v2 then v1 else v2
    in
    match exp with
    | NUM n -> n
    | PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
    | MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
    | MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
    | DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
    | MAX el -> max el
    | VAR x -> getValue (env, x)
    | LET (x, v, e) -> eval ((x, eval(env, v)) :: env, e)

  let print_value: value -> unit = fun v ->
    print_int v; print_newline ()
end
