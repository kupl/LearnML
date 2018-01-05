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

    module Dic = Map.Make(String)
    type environment = int Dic.t
    type value = int

    let emptyEnv : environment = Dic.empty

    let rec eval : environment * expr -> value = fun (env, exp) ->
        match exp with
        | NUM(a) -> a 
        | PLUS(a, b) -> (eval (env, a)) + (eval (env, b))
        | MINUS(a, b) -> (eval (env, a)) - (eval (env, b)) 
        | MULT(a, b) -> (eval (env, a)) * (eval (env, b))
        | DIVIDE(a, b) -> (eval (env, a)) / (eval (env, b))
        | MAX([]) -> 0
        | MAX l -> begin
            let l_in = List.map (fun exp -> eval (env, exp)) l in
            List.fold_left (fun a b -> if a > b then a else b) min_int l_in
        end
        | VAR id -> begin
            try Dic.find id env with
            | Not_found -> raise (Error "FreeVariable")
        end
        | LET (name, value, rest) -> begin
            let value_in = eval (env, value) in
            let new_env = Dic.add name value_in env in
            eval(new_env, rest)
        end
    let print_value : value -> unit = print_int

end 
