let max_of_list lst =
  if lst = [] then 0
  else (List.fold_left max (List.hd lst) (List.tl lst))

module ListSet =
  struct
    type element = string * int
    type set = element list
    let contains (set, key) =
      List.exists (fun item -> (fst item) = key) set
    let get (set, key) =
      snd (List.find (fun item -> (fst item) = key) set)
    let empty_set = []
    let put (set, key, value) =
      let rec replace(set, key, value) =
        if set = [] then [(key, value)]
        else
          if (fst (List.hd set)) = key then
            [(key, value)] @ (List.tl set)
          else
            [(List.hd set)] @ (replace((List.tl set), key, value))
      in
      replace(set, key, value)
  end
(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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

module Zexpr : ZEXPR = struct
    
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

    type environment = ListSet.set
    type value = int
    
    let emptyEnv = ListSet.empty_set
    let rec eval (env, e) =
      match e with
      | NUM i -> i
      | PLUS (expr1, expr2) -> (eval (env, expr1)) + (eval (env, expr2))
      | MINUS (expr1, expr2) -> (eval (env, expr1)) - (eval (env, expr2))
      | MULT (expr1,  expr2) -> (eval (env, expr1)) * (eval (env, expr2))
      | DIVIDE (expr1, expr2) -> (eval (env, expr1)) / (eval (env, expr2))
      | MAX expr_list -> max_of_list (List.map (fun x -> (eval (env, x))) expr_list) 
      | VAR id -> if ListSet.contains (env, id) then ListSet.get (env, id) else raise (Error "FreeVariable")
      | LET (id, expr_value, expr_body) -> eval (ListSet.put (env, id, (eval (env, expr_value))), expr_body) (* ∏∏¿œ  *)
    let int_of_value v = v
end
