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

  type value = int
  type environment = (id * value) list

  let emptyEnv : environment = []

  let rec eval : environment * expr -> value =
    fun (env, exp) ->
      match exp with
      | NUM n            -> n
      | PLUS (e1, e2)    -> eval (env, e1) + eval (env, e2)
      | MINUS (e1, e2)   -> eval (env, e1) - eval (env, e2)
      | MULT (e1, e2)    -> eval (env, e1) * eval (env, e2)
      | DIVIDE (e1, e2)  -> eval (env, e1) / eval (env, e2)
      | MAX lst          -> 
      (
        match lst with
        | [] -> 0
        | _ ->
          let rec maxSub : (environment * expr list) -> value =
            fun (env, lst) ->
              match lst with
              | [] -> 0    (* don't happen *)
              | a::[] -> eval (env, a)
              | a::t -> 
                let v1 = eval (env, a) in
                let v2 = maxSub (env, t) in
                max v1 v2
          in
          maxSub (env, lst)
      )
      | VAR id           -> 
      (
        try
          snd (List.find (fun x -> (fst x) = id) env)
        with Not_found -> raise (Error "FreeVariable")
      )
      | LET (id, e1, e2) -> 
        let v = eval (env, e1) in
        eval ((id, v) :: env, e2)

  let int_of_value : value -> int =
    fun value -> value

end
