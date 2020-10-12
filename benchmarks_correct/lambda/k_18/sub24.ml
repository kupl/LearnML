type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup : var list -> var -> bool 
  = fun l v ->
    match l with 
    | [] -> false
    | hd::tl -> if hd=v then true else (lookup tl v);;

let ex_env: var list -> var -> var list
  = fun l v ->
    v::l;;

let check : lambda -> bool
  = fun lam -> (* TODO *)
    let env= [] in

    let rec ch = fun lam env ->
      match lam with
      |V(v)-> (lookup env v)
      |P(v, l)-> (ch l (ex_env env v))
      |C(l1, l2)-> ((ch l1 env) && (ch l2 env))
    in
    (ch lam env);;
