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

  type environment = (id * int) list
  type value = expr

  let emptyEnv = []

  let int_of_value v =
  match v with
  | NUM i -> i
  | _ -> raise (Error "not calculated")

  let eval (env, expr) =
 
  let rec mapfunc l f r =
  match l with
  | [] -> r
  | hd::tl -> mapfunc tl f ((f hd)::r)
  in

  let rec mem_assocfunc id env =
  match env with
  | [] -> false
  | (k, v)::tl -> if id = k then true else mem_assocfunc id tl
  in

  let rec assocfunc id env =
  match env with
  | [] -> raise (Error "FreeVariable")
  | (k, v)::tl -> if id = k then v else assocfunc id tl
  in
(*
  let rec print_list = function 
  [] -> ()
  | (id, v)::l -> print_string id; print_string ","; print_int v ; print_string " " ; print_list l
  in
*)
  let rec evalfunc env expr =
  
  (*  print_list env; *)
  match expr with
  | NUM i -> NUM i
  | PLUS (e1, e2) -> NUM ((int_of_value (evalfunc env e1)) + (int_of_value (evalfunc env e2)))
  | MINUS (e1, e2) -> NUM ((int_of_value (evalfunc env e1)) - (int_of_value (evalfunc env e2)))
  | MULT (e1, e2) -> NUM ((int_of_value (evalfunc env e1)) * (int_of_value (evalfunc env e2)))
  | DIVIDE (e1, e2) -> NUM ((int_of_value (evalfunc env e1)) / (int_of_value (evalfunc env e2)))
  | MAX el -> 
    if el == [] then NUM 0
    else(
      let el1 = mapfunc el (evalfunc env) [] in
      let el2 = mapfunc el1 int_of_value [] in
      NUM (List.hd (List.rev (List.sort compare el2)))
    )
  | VAR id -> if (mem_assocfunc id env) then NUM (assocfunc id env) else raise (Error "FreeVariable")
  | LET (id, e1, e2) ->
    let id_value = int_of_value (evalfunc env e1) in
    evalfunc ((id, id_value)::env) e2
  in

  evalfunc env expr

end

