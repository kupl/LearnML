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

  type value = int
  type environment = (id * value) list
(*
  let findmax (i, (lst:expr list)):expr =
    if(i> List.hd(lst)) then i
    else List.hd(lst)
*)
  let emptyEnv : environment = []

  let checkEmpty (e1:environment) =
    if(e1 = emptyEnv) then true
  else false
  
let rec var_env ((a1:id), (a2:environment)):value =
    match a2 with
    | f::h ->  if fst f = a1 then  snd f
               else var_env(a1, h) 
    | [] -> raise (Error "FreeVariable") 

  

  (*e1 - environment e2 - expr*)
  let rec eval(e1, e2) =
  (*for max = if int = 0 -> first max *)
    match e2 with
    | NUM e -> e
    | PLUS(a1, a2) -> eval(e1, a1) + eval(e1, a2)
    | MINUS(a1, a2) -> eval(e1, a1) - eval(e1, a2)
    | MULT(a1, a2) -> eval(e1, a1) * eval(e1, a2)
    | DIVIDE(a1, a2) -> eval(e1, a1) / eval(e1, a2)
    | MAX [] -> 0     
    (*https://stackoverflow.com/questions/14363050/list-fold-left-in-ocaml*)
    | MAX(a1::a2) -> (*if((a1::a2)=[]) then 0
                      else*) List.fold_left
      (fun m a -> max m (eval (e1, a))) (eval (e1, a1))  a2
    | VAR a -> (*if(checkEmpty e1) then raise (Error "FreeVariable")
                else *)var_env(a, e1)
    (*a1-id a2-expr a3-expr keep id in expr- environment put the present environment to a2
      eval(e1, a2) -> var (a1, (eval(e1, a2)) -> (id*value)-> list*)
   (*) | LET((a1:id), (a2:expr), (a3:expr)) -> eval((a1, (eval(e1, a2))::e1), a3)
    *)| LET (b1, b2, b3) -> eval (((b1, (eval (e1, b2)))::e1), b3)
(*id * expr * expr*)

 let print_value v = print_endline (string_of_int(v))
 
end
