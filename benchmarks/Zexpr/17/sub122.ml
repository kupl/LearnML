
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
  let emptyEnv = []
  let rec eval (env, exp) = (
    match exp with
      | NUM n -> n
      | PLUS (exp1, exp2) -> (eval (env, exp1)) + (eval (env, exp2))
      | MINUS (exp1, exp2) -> (eval (env, exp1)) - (eval (env, exp2))
      | MULT (exp1, exp2) -> (eval (env, exp1)) * (eval (env, exp2))
      | DIVIDE (exp1, exp2) -> (eval (env, exp1)) / (eval (env, exp2))
      | MAX li -> (
          let rec max li v =
            match li with
              | [] -> v
              | n :: tl -> let va = eval (env, n) in
                    if va > v then max tl va else max tl v
          in
            match li with
              | [] -> max li 0
              | f :: tl -> max tl (eval (env, f))
        )
      | VAR i -> (
          let rec findValue x env =
            match env with
              | [] -> raise (Error "FreeVariable")
              | t :: tl -> match t with (s, va) ->
                if s = x then va else findValue x tl
          in
            findValue i env
        )
      | LET (i, exp1, exp2) -> (
          eval ((i, eval (env, exp1)) :: env, exp2)
        )
  )
  let print_value va = print_int va; print_newline ()
end
