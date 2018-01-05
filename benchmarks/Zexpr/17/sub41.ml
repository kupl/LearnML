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

  type environment = (string * int) list
  type value = int

  let emptyEnv = []
  let rec eval (envi, expre) =
    match expre with
    | NUM (a) -> a
    | PLUS (x, y) -> (eval (envi, x)) + (eval (envi, y))
    | MINUS (x, y) -> (eval (envi, x)) - (eval (envi, y))
    | MULT (x, y) -> (eval (envi, x)) * (eval (envi, y))
    | DIVIDE (x, y) -> (eval (envi, x)) / (eval (envi, y))
    | MAX (l) ->
      let get_max exprList =
        match exprList with
        | [] -> 0
        | [x] -> eval (envi, x)
        | a::b -> if (eval (envi, a) > eval (envi, MAX (b))) then eval (envi, a)
                                                             else eval (envi, MAX (b))
      in
      get_max (l)
    | VAR i ->
      ( try snd (List.find (fun (a, _) -> a = i) envi)
        with Not_found -> raise (Error "FreeVariable")
      )
    | LET (i, x, y) ->
      eval ((i, (eval (envi, x)))::envi, y)
  let print_value = print_int
end 

