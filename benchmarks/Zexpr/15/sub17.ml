module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr =
    | NUM of int
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
    | NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr

  type environment = (string * int) list
  type value = int

  let emptyEnv : environment = []
  let rec eval ((env: environment), (exp: expr)): value =
    match exp with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX l -> (let evaledList = List.map (fun exp' -> eval (env, exp')) l in
                List.hd (List.rev (List.sort (fun x y -> x-y) evaledList)))
    | VAR id -> (if (List.exists (fun x -> id == (fst x)) env)
                then snd (List.find (fun x -> id == (fst x)) env)
                else raise (Error "FreeValuable"))
    | LET (id, exp1, exp2) -> (if (List.exists (fun x -> id == (fst x)) env)
                              (* *)then raise (Error "Value is already defined")
                              else eval ((id, (eval (env, exp1)))::env, exp2))

  let print_value (v: value): unit = print_endline (string_of_int v)
end

(*
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.MAX [Zexpr.NUM 3; Zexpr.NUM 12; Zexpr.NUM 6]))
*)