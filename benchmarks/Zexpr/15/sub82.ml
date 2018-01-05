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

module Zexpr: ZEXPR = 
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
    type value = VAL of int
    type environment = (id * value) list

    let emptyEnv = [];;
    let int_of_value v =
        match v with
        | VAL n -> n;;
    let rec eval (env, expression) =
        match expression with
        | NUM n -> (VAL n)
        | PLUS (n, m) -> (VAL ( (int_of_value(eval (env, n))) + (int_of_value(eval (env, m))) ))
        | MINUS (n, m) -> (VAL ( (int_of_value(eval (env, n))) - (int_of_value(eval (env, m))) ))
        | MULT (n, m) -> (VAL ( (int_of_value(eval (env, n))) * (int_of_value(eval (env, m))) ))
        | DIVIDE (n, m) -> (VAL ( (int_of_value(eval (env, n))) / (int_of_value(eval (env, m))) ))
        | MAX l -> (match l with
                    | [] -> (VAL 0)
                    | t ->
                            let rec findMax l =
                                match l with
                                | [] -> Pervasives.min_int
                                | elem::t -> if (int_of_value(eval (env, elem))) > (findMax t) then (int_of_value(eval (env, elem))) else (findMax t)
                            in (VAL(findMax t)))
        | VAR v -> if List.mem_assoc v env then (List.assoc v env) else raise (Error "FreeVariable")
        | LET (v, e1, e2) -> if List.mem_assoc v env
            then (eval (((v, (eval (env, e1)))::(List.remove_assoc v env)), e2))
            else (eval (((v, (eval (env, e1)))::env), e2))

    let print_value v =
        match v with
        | VAL(x) -> print_endline (string_of_int x)
end 
