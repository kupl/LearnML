type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

type env = string list

let rec find_env = fun env value->
  match env with
    |v::tl -> if value = v then true else find_env tl value
    |[]->false

let add_env = fun env value -> env@[value]

let rec subcheck = fun lam env ->
  match lam with
    |V(v) -> find_env env v
    |P(v,y)-> 
      let env' = add_env env v in
      subcheck y env'
    |C(l1,l2)-> if subcheck l1 env then subcheck l2 env else false

let check : lambda -> bool
= fun lam -> subcheck lam [];;

      
      
      
      
      
      
      