(*real code start*)
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

module VARMAP = Map.Make(String)

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
  type environment = int VARMAP.t
  type value = int
  let emptyEnv : environment = VARMAP.empty

  let rec eval : environment * expr -> value = fun (env, exp) ->
   match exp with
   |NUM(x) -> x
   |PLUS(x,y) -> eval(env, x) + eval(env, y)
   |MINUS(x,y) -> eval(env, x) - eval(env, y)
   |MULT(x,y) -> eval(env, x) * eval(env, y)
   |DIVIDE(x,y) -> eval(env, x) / eval(env, y)
   |MAX(xl) ->  let rec find_max((env: environment), (el : expr list)) : int = 
    (match el with
     | [] -> 0
     | hd ::[] -> eval(env,hd)
     | hd :: tl -> let evhd = eval(env,hd) in
     let evtl = find_max(env, tl) in
     if (evhd > evtl) then evhd else evtl
    )
    in
    find_max(env, xl)
    (*
    let maxval = 0 in
    let temp_list = xl in
    while (List.length(temp_list)!=0) do
	let temp = eval(env, List.hd(temp_list)) in
	temp_list = List.tl(temp_list);
	if(temp > maxval) then maxval = temp
    done
    maxval
    *)
   |VAR(id) -> if((VARMAP.mem id env) == false) then raise (Error "FreeVariable")
    else VARMAP.find id env
   |LET(id, e1, e2) -> 
    let temp = eval(env, e1) in
    eval((VARMAP.add id temp env), e2)

  let print_value : value -> unit = fun x -> print_int(x); print_newline()
end

(*end*)
