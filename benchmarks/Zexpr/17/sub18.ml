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
  type value = int

  let emptyEnv = []
  let rec eval ((env: environment), (exp: expr)) = 
    match exp with
    | NUM i -> i
    | PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
    | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
    | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
    | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2)
    | MAX l -> (
      let rec max l' = 
        match l' with
        | [] -> min_int
        | a :: tail -> (
          let b = max(tail) in
          if(eval(env, a) > b) then eval(env, a)
          else b
        )
      in
      if List.length l = 0 then 0 else max l
    )
    | VAR i -> (
      if List.mem_assoc i env then List.assoc i env else raise (Error "FreeVariable")
    )
    | LET (i, e1, e2) -> eval((List.append [(i, eval(env, e1))] env), e2)


  let print_value (v: value) = print_endline(Pervasives.string_of_int v)

end 