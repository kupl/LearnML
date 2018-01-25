(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
  | [] -> []
  | hd::tl -> if pred hd
    then hd::(filter pred tl)
    else (filter pred tl)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match (a,b) with
  | ([],b) -> b
  | (a,[]) -> a
  | (hda::tla, hdb::tlb) -> hda::hdb::zipper(tla, tlb)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  if n < 0 then raise (Failure "Negative number")
  else if n = 0 then fun x -> x
  else if n = 1 then f
  else fun x -> iter(n-1,f) x + f x
    
(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with
  | Const a -> Const 0
  | Var alpha -> if alpha = x then Const 1 else aexp
  | Power (alpha, a) -> if alpha = x then Times [Const a; Power (alpha, a-1)] else aexp
  | Sum lst -> (match lst with
    | [] -> Const 0
    | hd::tl -> (match tl with
      | [] -> diff(hd,x)
      | _ -> Sum (diff(hd,x)::diff(Sum tl,x)::[]) ))
  | Times lst -> (match lst with
    | [] -> Const 1
    | hd::tl -> (match tl with
      | [] -> diff(hd,x)
      | _ -> Sum (
        (Times (diff(hd,x)::(Times tl)::[]))
        ::(Times (hd::diff(Times tl,x)::[]))
        ::[])
      )
    )

(*************************)
(* Problem 5: Calculator *)
(*************************)
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
