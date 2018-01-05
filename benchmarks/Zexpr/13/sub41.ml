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

    type environment = (id * expr) list
    type value = expr
    
    let emptyEnv = []
    let rec eval (env, e) =
      match e with
      | NUM (i) -> e
      | PLUS (e1, e2) -> PLUS (eval (env, e1), eval (env, e2))
      | MINUS (e1, e2) -> MINUS (eval (env, e1), eval (env, e2))
      | MULT (e1, e2) -> MULT (eval (env, e1), eval (env, e2))
      | DIVIDE (e1, e2) -> DIVIDE (eval (env, e1), eval (env, e2))
      | MAX (l) -> MAX (List.map (fun exp -> eval (env, exp)) l)
      | VAR (id) -> 
          (try
             snd (List.find (fun en -> (fst en)=id) env)
           with
             _ -> raise (Error "FreeVariable"))
      | LET (id, e1, e2) -> LET (id, eval (env, e1), eval (((id, e1)::env), e2))

    let rec int_of_value v =
      match v with
      | NUM (i) -> i
      | PLUS (e1, e2) -> (int_of_value e1) + (int_of_value e2)
      | MINUS (e1, e2) -> (int_of_value e1) - (int_of_value e2)
      | MULT (e1, e2) -> (int_of_value e1) * (int_of_value e2)
      | DIVIDE (e1, e2) -> (int_of_value e1) / (int_of_value e2)
      | MAX [] -> 0
      | MAX (hd::tl) ->
          (match tl with
           | [] -> int_of_value hd
           | h::t ->
               if (int_of_value hd)>(int_of_value h) then int_of_value (MAX (hd::t))
               else int_of_value (MAX tl))
      | VAR _ -> raise (Error "FreeVariable")
      | LET (id, e1, e2) -> int_of_value (eval (((id, e1)::emptyEnv), e2))

end
