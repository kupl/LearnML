  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  module type Iter = sig
    type t
    val empty : t
    val have : var -> t -> bool
    val extend : var -> t -> t
  end

  module Iter : Iter = struct
    type t = var list
    let empty = []
    let rec have v env = 
      match env with
      | [] -> false
      | hd::tl -> if hd = v then true else have v tl
    let extend x env = x::env
  end
  
  let rec eval : exp -> Iter.t -> bool
  = fun e env -> match e with
    | V a -> Iter.have a env
    | P (a, b) -> eval b (Iter.extend a env)
    | C (a, b) -> if (eval a env) && (eval b env) then true else false
  
  let check : exp -> bool
  =fun e -> eval e Iter.empty
