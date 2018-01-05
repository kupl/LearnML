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
  
module ZEXPR : ZEXPR = struct
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
  let eval (env,e) =
    let rec evalrec (env,e) =
    match e with
      | NUM a -> a
      | PLUS (a,b) -> (evalrec (env,a)) + (evalrec (env,b))
      | MINUS (a,b) -> (evalrec (env,a)) - (evalrec (env,b))
      | MULT (a,b) -> (evalrec (env,a)) * (evalrec (env,b))
      | DIVIDE (a,b) -> (evalrec (env,a)) / (evalrec (env,b))
      | MAX l -> (match l with
                    | [] -> 0
                    | hd::[] -> (evalrec (env,hd))
                    | hd::tl -> max (evalrec (env,hd)) (evalrec (env,(MAX tl))))
      | VAR id -> (match env with
                     | [] -> raise (Error "FreeVariable")
                     | hd::tl -> if (fst hd) = id then (snd hd) else evalrec (tl,e))
      | LET (id,a,b) -> evalrec ((id,(evalrec (env,a)))::env,b) in
      evalrec (env,e)
  let int_of_value v =
      v
end