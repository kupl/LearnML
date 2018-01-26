(*Problem 5*)
type exp = X
  |INT of int
  |ADD of exp * exp
  |SUB of exp * exp
  |MUL of exp * exp
  |DIV of exp * exp
  |SIGMA of exp * exp * exp

let empty_env = []

let rec extend_env a b env =
  match a<=b with
  |false -> env
  |true -> a::(extend_env (a+1) b env)

let get_hd env = 
  match env with
  |[] -> raise (Failure "there is no value in env")
  |hd::tl -> hd

let rec fold f x l = 
  match l with
  |[] -> 0
  |hd::tl -> (f x [hd]) + (fold f x tl)

let rec eval exp env =
  match exp with
  |X -> get_hd env
  |INT n -> n 
  |ADD (e1, e2) -> 
    let n1 = eval e1 env in
    let n2 = eval e2 env in
      n1 + n2
  |SUB (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
      n1 - n2
  |MUL (e1, e2) ->
    let n1 = eval e1 env in                                                  
    let n2 = eval e2 env in
      n1 * n2
  |DIV (e1, e2) ->
    let n1 = eval e1 env in                                                    
    let n2 = eval e2 env in
      n1 / n2
  |SIGMA (e1, e2, e3) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    fold (fun x env -> eval x env) e3 (extend_env n1 n2 env)

let calculator exp = eval exp empty_env
