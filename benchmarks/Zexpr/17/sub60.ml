(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-7.ml *)

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
  type environment = (string * int) list
  type value = int

  val emptyEnv : environment
  val eval : environment * expr -> value
  val print_value : value -> unit
end

let rec map (eval, env, l) =
  match l with
  | [] -> []
  | h::t -> (eval (env, h))::(map (eval, env, t))

let rec find (env, v) =
  match env with
  | [] -> ("", 0)
  | h::t ->
      if (fst h) = v then h
      else find (t, v)

let rec max l =
  match l with
  | [] -> 0
  | [i] -> i
  | h::t ->
      if h > (max t) then h
      else max t

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
  type environment = (string * int) list
  type value = int

  let emptyEnv = []
  let rec eval = fun (env, exp) ->
    match (env, exp) with
    | (_, NUM i) -> i
    | (env, PLUS (i, j)) -> (eval (env, i)) + (eval (env, j))
    | (env, MINUS (i, j)) -> (eval (env, i)) - (eval (env, j))
    | (env, MULT (i, j)) -> (eval (env, i)) * (eval (env, j))
    | (env, DIVIDE (i, j)) -> (eval (env, i)) / (eval (env, j))
    | (env, MAX l) -> max (map (eval, env, l))
    | (env, VAR v) ->
	if (fst (find (List.rev env, v))) = "" then raise (Error "FreeVariable")
	else snd (find (List.rev env, v))
    | (env, LET (i, e1, e2)) -> eval ((List.append env [(i, eval (env, e1))]), e2)
  let print_value = fun x -> (print_int x); (print_newline ())
end
