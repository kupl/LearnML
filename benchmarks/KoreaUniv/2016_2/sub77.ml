(*********************)
(*     Problem 1     *)
(*******************)
let rec fold f l a = match l with
| [] -> a
| hd::tl -> f hd (fold f tl a)


let rec max : int list -> int
   = fun lst -> fold (fun x y -> if x>y then x else y) lst min_int
 
   let rec min : int list -> int
   = fun lst -> fold (fun x y -> if x<y then x else y) lst max_int
   
 (*********************)
  (*     Problem 2     *)
  (*********************)
  let rec filter pred lst = match lst with
  |[] -> []
  |hd::tl -> if (pred hd) = true then [hd]@ (filter pred tl) 
  else filter pred tl
  
 (*********************)
  (*     Problem 3     *)
  (*********************)
  let rec double f a = f (f a)
(*     Problem 4     *)
(*********************)
type btree =
    | Empty
    | Node of int * btree * btree
  
  let rec mem : int -> btree -> bool
  = fun n tree -> match tree with
 |Empty -> false
 |Node(root,left,right) -> if root = n then true
 else (mem n left)||(mem n right) 
(*********************)
(*     Problem 5     *)
(*********************)
 type nat =
    | ZERO
    | SUCC of nat
  
  let rec natadd : nat -> nat -> nat
    = fun n1 n2 -> match n2 with
  |ZERO -> n1
  |SUCC n -> natadd (SUCC n1) n
  
  let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> if n1 = ZERO || n2 = ZERO then ZERO else match n2 with 
  |ZERO -> ZERO 
  |SUCC(ZERO) -> n1
  |SUCC(n) -> natadd n1 (natmul n1 n)
(*********************)
(*     Problem 6     *)
(*********************)
 type formula =
    | True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
  
  and exp =
    | Num of int
    | Plus of exp * exp
    | Minus of exp * exp
  
  let rec eval : formula -> bool
  = fun f -> 
    let rec evalExp : exp -> int = fun e
    -> match e with
    |Num(n) -> n
    |Plus(n1,n2) -> evalExp(n1)+evalExp(n2)
    |Minus(n1,n2) -> evalExp(n1)-evalExp(n2)
    in match f with
 |True -> true
 |False -> false
 |Not f1 -> if (eval f1) = true then false else true
 |AndAlso(f1,f2)  -> if (eval f1)=true && (eval f2)=true then true else false
 |OrElse(f1,f2) -> if (eval f1)=true || (eval f2)=true then true else false
 |Imply(f1,f2) -> if (eval f1)=true then eval f2 else true
 |Equal(e1,e2) -> if (evalExp e1) = (evalExp e2) then true else false 
