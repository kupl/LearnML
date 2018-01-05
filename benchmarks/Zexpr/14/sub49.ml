

(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q6: ZEXPR *)

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

  val int_of_value : value -> int
end


module Zexpr =
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
  let emptyEnv: environment = []

  let int_of_value x = x

  let rec find_max lst default =
    match lst with
    | [] -> default
    | x :: xs ->    if (x > default)
    then find_max xs x
                          else find_max xs default ;;
  let rec add_to_env env id value =
    match env with
    | ((name, _) :: xs) when name = id -> (name, value) :: xs
    | (x :: xs) -> x :: (add_to_env xs id value)
    | [] -> (id, value) :: [] ;;


  let rec eval arg =
    let (env, e) = arg in
    match e with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX xs ->     let values = List.map (fun x -> (eval (env, x))) xs in
    (find_max values 0)
    | VAR id -> (List.assoc id env)
    | LET (id, e1, e2) ->   let value = (eval (env, e1)) in
    (eval ((add_to_env env id value), e2) )

end



