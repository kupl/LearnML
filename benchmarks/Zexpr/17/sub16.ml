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
  val emptyEnv: environment
  val eval: environment * expr -> value
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

  type environment = (string * int) list
  type value = int

  let emptyEnv: environment = []
  let rec eval: environment * expr -> value = fun (en, exp) ->
    match exp with
    | NUM x -> x
    | PLUS (e1, e2) -> eval(en, e1) + eval(en, e2)
    | MINUS (e1, e2) -> eval(en, e1) - eval(en, e2)
    | MULT (e1, e2) -> eval(en, e1) * eval(en, e2)
    | DIVIDE (e1, e2) -> eval(en, e1) / eval(en, e2)
    | MAX exp_list ->
      (match exp_list with
       | [] -> 0
       | now :: [] -> eval(en, now)
       | now :: rest ->
         if(eval(en, now) > eval(en, MAX rest)) then eval(en, now)
         else eval(en, MAX rest))
    | VAR id ->
      (match en with
       | [] -> raise (Error "FreeVariable")
       | (name, value) :: rest ->
         if(id = name) then value
         else eval(rest, VAR id))
    | LET (id, e1, e2) -> eval((id, eval(en, e1)) :: en, e2)

  let print_value : value -> unit = fun v -> print_endline(string_of_int v)
end
