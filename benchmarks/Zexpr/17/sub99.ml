(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-7 *)
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
  type value = int ref
  type environment = (id * value) list
  let emptyEnv : environment = []
  let rec find : environment * id -> value = fun (env, s) ->
    match env with
    | [] -> raise (Error "FreeVariable")
    | (i, v)::t -> if i = s then v else find (t, s)
  let rec eval : environment * expr -> value =
    fun (env, expr) -> match expr with
    | NUM a -> ref a
    | PLUS (e1, e2) -> ref (!(eval(env, e1)) + !(eval(env, e2)))
    | MINUS (e1, e2) -> ref (!(eval(env, e1)) - !(eval(env, e2)))
    | MULT (e1, e2) -> ref (!(eval(env, e1)) * !(eval(env, e2)))
    | DIVIDE (e1, e2) -> ref (!(eval(env, e1)) / !(eval(env, e2)))
    | MAX el -> let rec max : environment * expr list -> value =
        fun (env, el) -> 
        (match el with
         | [] -> ref 0
         | [h] -> eval (env, h)
         | h::t -> let h' = eval (env, h)
            and t' = max (env, t) in
            (if h' > t' then h' else t'))
        in max(env, el)
    | VAR s -> find (env, s)
    | LET (s, e, f) -> eval ((s, eval(env, e))::env, f)
  let print_value : value -> unit = fun v -> 
    print_endline(string_of_int(!v))
end

(* Test Code
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1))
*)
