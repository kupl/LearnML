(* 2015-11380 박찬양 HW2_7 *)

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
  exception Error of string (*"FreeVariable" *)
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
  type environment = string list * int list
  type value = int
  let emptyEnv : environment = ([], [])
  let rec eval : environment * expr -> value = fun (env, exp) ->
    match exp with
    | NUM a -> a
    | PLUS (a, b) -> eval(env, a) + eval(env, b)
    | MINUS (a, b) -> eval(env, a) - eval(env, b)
    | MULT (a, b) -> eval(env, a) * eval(env, b)
    | DIVIDE (a, b) -> 
      (let realb = eval(env, b) in
        (if realb = 0 then raise (Error "FreeVariable")
        else eval(env, a) / realb))
    | MAX a ->
      if a=[] then 0 else
      (let rec getReal l = 
        match l with
        | [] -> []
        | h::[] -> eval(env, h)::[]
        | h::t -> eval(env, h)::getReal(t) in
        (let reala = getReal a in
          match reala with
          | [] -> 0
          | h::[] -> h
          | h::t ->
            (let lh::lt = List.rev (List.sort compare reala) in lh)))
    | VAR a -> (
      let rec idxa(b, l, i) =
        (match l with
        | [] -> i
        | h::t -> if h=b then i else idxa(b, t, i+1)) in
          (match env with
          | ([], []) -> raise (Error "FreeVariable")
          | (idl, varl) ->
            (if List.mem a idl then List.nth varl (idxa(a, idl, 0)) 
            else raise (Error "FreeVariable"))))
    | LET (id, a, b) ->
      match env with
      | ([], []) -> eval((id::[], eval(env, a)::[]), b)
      | (idl, varl) -> eval((id::idl, eval(env, a)::varl), b)
  let print_value : value ->  unit = fun (var) ->
    print_string (string_of_int var)
end