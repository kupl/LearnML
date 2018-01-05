(*
 * Programming Languages, 2013 Fall.
 * Solution for Homework 2 : Zexpr.
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 * Misc changes by Jaeseung Choi (2015 TA)
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

  val print_value : value -> unit
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

  type environment = (string * int) list
  type value = int
  
  let emptyEnv = []

  let rec eval (env, e) =
    match e with
    |NUM n -> n
    |PLUS (e1, e2) -> eval (env, e1) + eval (env, e2) 
    |MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
    |MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
    |DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
    |MAX exp_list ->
      (* if exp_list is empty, must be evaluated to 0 *)
      (* Caution : [NUM -1] should be evaluted to -1, not 0 *)
      if List.length exp_list = 0 then
        0
      else
        let first_v = eval (env, List.hd exp_list) in
        let tail_exp_list = List.tl exp_list in
        List.fold_left
          (fun cur_max e ->
            let v = eval (env, e) in
            if v > cur_max then v else cur_max
          ) first_v tail_exp_list 
    |VAR id -> 
      (* List.find returns first element *)
      (try snd (List.find (fun (x, _ ) -> id = x) env) with
      Not_found -> raise (Error "FreeVariable"))
    |LET (id, e1, e2) ->
      let v = eval (env, e1) in
      let new_env = (id, v)::env in
      eval (new_env, e2)

    let print_value v = 
      print_endline (string_of_int v)
end

