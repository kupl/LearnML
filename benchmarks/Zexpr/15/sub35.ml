module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr =
      NUM of int
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

module Zexpr : ZEXPR =
struct
  exception Error of string
  type id = string
  type expr =
      NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

  type environment = (string * int) list
  type value = int

  let emptyEnv = []
  let rec eval (env, exp) =
    match exp with
      NUM i -> i
    | PLUS (subExp1, subExp2) -> eval (env, subExp1) + eval (env, subExp2)
    | MINUS (subExp1, subExp2) -> eval (env, subExp1) - eval (env, subExp2)
    | MULT (subExp1, subExp2) -> eval (env, subExp1) * eval (env, subExp2)
    | DIVIDE (subExp1, subExp2) -> eval (env, subExp1) / eval (env, subExp2)
    | MAX expList ->
      if List.length expList == 0 then 0
      else
        let mappedList = List.map (fun e -> eval (env, e)) expList in
        List.fold_left max (List.hd mappedList) mappedList
    | VAR id ->
      (try
        snd @@ List.find (fun (name, value) -> name = id) env
      with
        Not_found -> raise @@ Error "FreeVariable")
    | LET (id, subExp1, subExp2) ->
      let value = eval (env, subExp1) in
      eval ((id, value) :: env, subExp2)

  let print_value v = print_endline @@ string_of_int v
end
