(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 30, 2014
 *)

(* Exercise 6 : Zexpr *)
module type ZEXPR = sig
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

module Zexpr = struct
  exception Error of string
  exception FreeVariable
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

  type environment = ((string * int) list) list
  type value       = int

  let emptyEnv : environment = []
  let rec eval : environment * expr -> value = fun (env,e) ->
    let rec lookup env id =
     	match env with
     	| [] -> raise (Error "FreeVariable")
     	| hd_env::tl_env -> try (List.assoc id hd_env) with Not_found -> (lookup tl_env id)
    in
    let rec max_list l = match l with
      | [] -> 0
      | h :: t -> max h (max_list t) in
   	match e with
   	| NUM n -> n
   	| PLUS   (e1,e2) -> (eval (env,e1)) + (eval (env,e2))
   	| MINUS  (e1,e2) -> (eval (env,e1)) - (eval (env,e2))
   	| MULT   (e1,e2) -> (eval (env,e1)) * (eval (env,e2))
   	| DIVIDE (e1,e2) -> (eval (env,e1)) / (eval (env,e2))
   	| MAX    elist   -> if elist = [] then 0 else
      		let vlist = List.fold_right (fun e s -> (eval (env,e))::s) elist []
      		in (max_list vlist)
   	| VAR    id      -> (match env with
 			    | [] -> raise (Error "FreeVariable")
 			    | _::_ -> lookup env id
   			)
   	| LET    (id,e1,e2) -> eval([(id,eval(env,e1))]::env, e2)

  let int_of_value : value -> int = fun v -> v
end
