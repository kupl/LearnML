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
  let rec eval (env, e) =
    match e with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
    | MAX lst ->
      (match lst with
      | [] -> 0
      | _ -> (List.hd (List.sort (fun a b ->
                              if a > b then -1
                              else if a = b then 0
                              else 1) 
                                 (List.map (fun x -> (eval (env, x))) lst)
                      )
             )
      )
    | VAR id -> (try snd (List.find (fun a -> fst a = id) env) with 
                 | Not_found -> raise (Error "FreeVariable"))
    | LET (name, value, e) -> eval (((name, (eval (env, value)))::(List.filter (fun a -> (fst a) != name) env)), e)
   
  let print_value v = print_int v; print_string "\n"
end
