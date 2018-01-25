type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
module type Iter = sig
  type t
  exception Not_found
  val empty : t
  val have : t -> exp
  val extend : exp -> t -> t
end

module Iter : Iter = struct
  type t = exp list
  exception Not_found
  let empty = []
  let have e = 
    match e with
    | [] -> raise Not_found
    | hd::tl -> hd
  let extend x e = x::e
end

let rec eval : exp -> Iter.t -> exp
=fun ev t -> match ev with
  | X -> Iter.have t
  | INT a -> INT a
  | ADD (a,b) -> 
    let evala = eval a t in
    let evalb = eval b t in
    (match (evala,evalb) with
    | (INT p, INT q) -> INT (p+q)
    | _ -> raise (Failure "Type error1"))
  | SUB (a,b) -> 
    let evala = eval a t in
    let evalb = eval b t in
    (match (evala,evalb) with
    | (INT p, INT q) -> INT (p-q)
    | _ -> raise (Failure "Type error2"))
  | MUL (a,b) ->     
    let evala = eval a t in
    let evalb = eval b t in
    (match (evala,evalb) with
    | (INT p, INT q) -> INT (p*q)
    | _ -> raise (Failure "Type error3"))
  | DIV (a,b) -> 
    let evala = eval a t in
    let evalb = eval b t in
    (match (evala,evalb) with
    | (INT p, INT q) -> if q = 0
      then raise (Failure "Divided by zero")
      else INT (p/q)
    | _ -> raise (Failure "Type error4"))
  | SIGMA (i, f, form) -> 
    let evali = eval i t in
    let evalf = eval f t in
    if evali > evalf
      then INT 0
      else eval (
        ADD(
	(eval (SIGMA (ADD (i,INT 1), f, form)) t),
	(eval form (Iter.extend evali t))
	  )
	) t

let calculator : exp -> int
=fun e -> match eval e Iter.empty with
  | INT a -> a
  | _ -> raise (Failure "error")
