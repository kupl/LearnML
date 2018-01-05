module type ZEXPR = 
sig
  exception Error of string
  type id = string
  type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr
  type environment
  type value
    
  val emptyEnv: environment
  val eval: environment * expr -> value
  val print_value : value -> unit 
end

module Zexpr : ZEXPR = 
struct
  exception Error of string
  type id = string
  type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list
    | VAR of id
    | LET of id * expr * expr
      
  type environment = ((id * int) list)
  type value = int
    
  let rec eval: environment * expr -> int = function
    (_environment, _expr) -> 
      (match _expr with
        |NUM(a) -> a
        |PLUS(a,b) -> eval(_environment, a) + eval(_environment, b)
        |MINUS(a,b) -> eval(_environment, a) - eval(_environment, b)
        |MULT(a,b) -> eval(_environment, a) * eval(_environment, b)
        |DIVIDE(a,b) -> eval(_environment, a) / eval(_environment, b)
            (*(let divider = eval(_environment, b) in
           if divider == 0 then raise (Error "divide by zero") else eval(_environment, a) / divider
               )*)
        |MAX(expr_list) -> 
          (match expr_list with
            |[] -> 0
            |head::[] -> eval(_environment, head)
            |head::tail -> 
              (let max a b = if a > b then a else b in
              	max (eval(_environment, head)) (eval(_environment, MAX(tail)))
              )
          )
        |VAR(var_id) -> (try (List.assoc var_id _environment) with Not_found -> raise (Error "FreeVariable"))
        |LET(_id, _expr_1, _expr_2) -> 
          (let x = eval(_environment, _expr_1) in
           eval((_id, x)::_environment, _expr_2)
          )
      )
    
    
  let emptyEnv = []
  let print_value n = print_endline(string_of_int(n))
end