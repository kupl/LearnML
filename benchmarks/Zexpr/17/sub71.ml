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

  type environment = (id * expr) list
  type value = int 

  let emptyEnv : environment = []
  let rec eval ((env : environment), (exp : expr)) : value =
    let rec max_helper ((env : environment), (l : expr list)) : value =
        match (env, l) with
        |(e, []) -> 0
        |(e, head::[]) -> eval(e, head)
        |(e, head::tail) -> 
          let rest = max_helper(env, tail) in
          if eval(e, head) > rest then eval(e, head) else rest in
    let rec var_helper ((env : environment), (s : string)) : value =
        match (env,s) with
        |([], var) -> raise (Error "FreeVariable")
        |(h::e, var) -> 
          if (fst h = var) then eval(env, snd h) else var_helper(e, var) in
    match (env,exp) with
    |(e, NUM(n)) -> n
    |(e, PLUS(e1, e2)) -> eval(e, e1) + eval(e, e2)
    |(e, MINUS(e1, e2)) -> eval(e, e1) - eval(e, e2)
    |(e, MULT(e1, e2)) -> eval(e, e1) * eval(e, e2)
    |(e, DIVIDE(e1, e2)) -> eval(e, e1) / eval(e, e2)
    |(e, MAX(l)) -> max_helper(e, l)
    |(e, VAR(s)) -> var_helper(e, s)
    |(e, LET(s, e1, e2)) -> eval((s, e1)::e, e2)

  let print_value (v : value) : unit = print_int(v)

end