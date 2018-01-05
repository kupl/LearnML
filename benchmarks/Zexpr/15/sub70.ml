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
  
  val print_value : value -> unit
end

module Zexpr : ZEXPR =
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
  type environment = (id*value) list
  let emptyEnv : environment = []
  let rec eval: (environment * expr) -> value = fun (env, e) ->
  match e with
    | NUM k -> k
    | PLUS (l,r) -> (eval (env, l)) + (eval (env, r))
    | MINUS (l,r) -> (eval (env, l)) - (eval (env, r))
    | MULT (l,r) -> (eval (env, l)) * (eval (env, r))
    | DIVIDE (l,r) -> (eval (env, l)) / (eval (env, r))
    | MAX [] -> 0
    | MAX (t::[]) -> eval (env, t)
    | MAX (t::tl) -> let lv = eval (env, MAX tl) in
        let tv = eval (env, t) in
        if lv < tv then tv else lv
    | VAR k -> let idList = List.filter (fun (a,b) -> a=k) env in
        (match idList with
        | [] -> raise (Error "FreeVariable")
        | (s,v)::l -> v
        )
    | LET (n,x,k) -> eval ( (n,(eval (env, x)))::env ,k)
  let print_value v = print_endline( string_of_int v)
end

