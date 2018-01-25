type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
=fun t -> 
  match t with 
  | Empty -> Empty
  | Node (a, left, right) -> Node (a, mirror right, mirror left) ;;

type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
   match n2 with
   | ZERO -> n1
   | SUCC n3 -> natadd (SUCC n1) n3;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC ZERO -> n1
  | SUCC n3 -> natadd n1 (natmul n1 n3);;

let rec natexp : nat -> nat -> nat
=fun n1 n2 -> 
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC ZERO -> n1
  | SUCC n3 -> natmul n1 (natexp n1 n3);;


type formula = 
  True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
  
(*let rec findVal : formula -> string list
= fun f -> 
  match f with 
  | Var (x) -> [x]
  | Neg (x) -> findVal x
  | And (p, q) -> (findVal p) @ (findVal q)
  | Or (p,q) -> (findVal p) @ (findVal q)
  | Imply (p,q) -> (findVal p) @ (findVal q)
  | Iff (p,q) -> (findVal p) @ (findVal q) 

let rec compressVal : string list -> string list
= fun sl ->
  match sl with 
  | [] -> []
  | h :: t -> 
  let rec removeVal : string list -> string -> string list -> string list
   = fun fac s l -> 
    match l with 
    | [] -> fac
    | h :: t -> if (h = s) then (removeVal fac s t) else (removeVal (fac @ [h]) h t)
  in removeVal [h] h t
*)
let rec sat : formula -> bool
= fun f -> true;;
  (*match f with 
  | True -> true
  | False -> false
  | Var (v) -> true
  | Neg (n) -> 
  (match n with
    | True -> false
    | False -> true
    | Var(v) -> true
    | Neg(nn) -> if(sat nn = false) then true
      else 
      (match nn with
      | True -> true
      | False -> false
      | Var(v) -> true
      | Neg(nnn) -> (sat nnn)
      | And(p, q) -> 
        (match (p, q) with
        | (Var(x), Var(y)) -> if(x = y) then true else false
        | (Var(x), Neg(y)) -> 
          (match y with
          | Var (z) -> if(x = z) then false else true
          | _ -> sat (And(p, q)))
        | (Neg(x),Var(y)) ->
          (match x with
           | Var (z) -> if(y = z) then false else true
           | _ -> sat (And(p,q)))
        | (Neg(x), Var(y)) -> not (sat (Or(p,q))))
      | Or(p, q) -> ((sat p) || (sat q))
      | Imply(p, q) -> 
        (
        match (p, q) with
        | (Var(x), Var(y)) -> true
        | (Var(x), Neg(y)) -> 
          (
          match y with
          | Var (z) -> if(x = z) then false else true
          | _ -> sat (Imply(p,y))
          )
        | (Neg(x),Var(y))-> true
        | (Neg(x),Neg(y))-> true
        )
      | Iff(p, q) -> 
        (match (p, q) with
        | (Var(x), Var(y)) -> true
        | (Var(x) ,Neg(y)) -> 
           (
            match y with
            | Var (z) -> if (x = z) then false else true
            | _ -> sat (Iff(p, y))
           )
        | (Neg(x), Var(y)) -> 
           (
            match x with
            | Var (z) -> if(y=z) then false else true
            | _ -> sat (Iff(x, q))
           )
        | (Neg(x),Neg(y)) -> true
        )
      )
    | And(t2,g2)-> 
      (match (t2, g2) with
      | (Var(x) ,Var(y)) -> if (x = y) then false else true
      | _ -> true
      )
    | Or(t2,g2) -> 
      (match (t2, g2) with
      | (Neg(x), Neg(y)) -> 
        ( match (x,y) with 
          | (Var z, Var k) -> if (z = k ) then false else true
          | _ -> true
        )
      | _ -> true
      )
    | Imply(t2,g2) -> 
      (match (t2, g2) with
       | (Var(x), Neg(y)) -> 
       (
        match y with
        | Var z -> if (z = x) then true else false
        | _ -> false
       )
       | _ -> if(((sat t2) = true) && (sat g2) = false) then true else false
      )
    | Iff (t2,g2) ->
      (
        match (t2,g2) with
        | (Var(x),Neg(y)) ->
          (match y with 
           | Var z -> if(x = z) then true else false
           | _ -> not (sat(t2) <> sat(g2))
          )
        | (Neg (y), Var (x)) -> 
          (match y with
           |Var z -> if(x= z) then true else false
           | _ -> not (sat(t2) <> sat (g2))
          )
        | _ -> not (sat(t2) <> sat (g2))
      )
  )
  | And (t, g) -> true
    (match t with
     | True -> if g = True then true else false
     | False-> false
     | _ -> true)
  | Or (t, g) -> true
  | Imply (t, g) -> 
    (match (t,g) with
    | (Var (x), Neg (y)) -> 
      (match y with
       | Var z -> if(z = x) then false else true
       | _ -> sat y
      )
    | _ -> true
    )
  | Iff (t, g) ->
    (match (t,g) with
     | (Var x, Neg(y)) ->
        (match y with
         | Var z -> if(z = y) then false else true
         | _ ->true
        )
     | _ -> true
    );;*)


type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
  match e with 
  | Const(t) -> Const (0)
  | Var(t) -> if t = x then Const (1) else Var(t)
  | Power(t, p) -> if t = x then Times([Const(p)]@[Power(t, p-1)]) else Power(t,p)
  | Times(l) -> 
    (match l with
     | [] -> Const (1)
     | h :: t -> 
       (match h with 
       | Const(c) ->Times([Const(c)]@[diff(Times(t),x)])
       | _ -> Times([diff(h,x)]@[diff(Times(t),x)])))
  | Sum(l) ->
    (match l with
     | [] -> Const (0);
     | h :: t -> Sum([diff(h,x)]@[diff(Sum(t),x)]));;
type exp = X 
  | INT  of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculater : exp -> int
= fun e ->
  let rec incalculate : int -> exp -> int
  = fun xval e1 ->
    match e1 with
    | X -> xval
    | INT (i) -> i
    | ADD (x, p) -> (incalculate xval x) + (incalculate xval p)
    | SUB (x, p) -> (incalculate xval x) - (incalculate xval p)
    | MUL (x, p) -> (incalculate xval x) * (incalculate xval p)
    | DIV (x, p) -> (incalculate xval x) / (incalculate xval p)
    | SIGMA (st, des, expression) -> if (calculater st) > (calculater des) then 0 
      else (incalculate (calculater st) expression) + (incalculate (calculater st) (SIGMA (INT((calculater st)+1), des, expression)))
  in
  match e with
  | X -> 1
  | INT (i) -> i
  | ADD (x, p) -> calculater x + calculater p
  | SUB (x, p) -> calculater x - calculater p
  | MUL (x, p) -> calculater x * calculater p
  | DIV (x, p) -> calculater x / calculater p
  | SIGMA (st, des, expression) -> if (calculater st) >(calculater des) then 0
    else incalculate (calculater st) (SIGMA(INT ((calculater st) + 1), des, expression));;

type mobile = branch * branch
and branch = SimpleBranch of length * weight
          | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
  let rec bwei : branch -> int 
  = fun b -> 
    match b with 
    | SimpleBranch(_ , wei) -> wei
    | CompoundBranch( len, (b1,b2)) -> (bwei b1) + (bwei b2)
  in
  let rec bTorq : branch -> int
  = fun b ->
    match b with
    | SimpleBranch(len, wei) -> len * wei
    | CompoundBranch(len , _) -> len * (bwei b)
  in
  match m with 
  | (lb,rb) -> if((bTorq lb) <> (bTorq rb)) then false
    else
    let rec balancedB : branch -> bool
    = fun br -> 
      match br with
      | SimpleBranch(_,_) -> true
      | CompoundBranch(len, mob) -> balanced mob
    in (balancedB lb) && (balancedB rb);;

type digit = ZERO | ONE
type bin = digit list

let digadd : digit -> digit -> digit -> digit * digit
= fun b1 b2 rise ->
  match (b1, b2, rise) with
  | (ZERO, ZERO, ZERO) -> (ZERO, ZERO)
  | (ZERO, ZERO, ONE) -> (ONE, ZERO)
  | (ZERO, ONE, ZERO) -> (ONE, ZERO)
  | (ONE, ZERO, ZERO) -> (ONE, ZERO)
  | (ONE, ONE, ONE) -> (ONE, ONE)
  | _ -> (ZERO, ONE)

let badd : bin -> bin -> bin
= fun b1 b2 ->
  let rec reversebin : bin -> bin
  = fun b ->
    match b with
    | [] -> []
    | h :: t -> (reversebin t) @ [h]
  in
    let rec adding : bin -> bin -> digit -> bin
    = fun bi1 bi2 rise ->
      match (reversebin bi1, reversebin bi2) with
      | ([], []) -> if rise = ZERO then [] else [ONE]
      | (h :: t, []) -> 
        (match (digadd h ZERO rise) with
        | (con, ris) -> (adding (reversebin t) [] ris) @ [con])
      | ([],h :: t) -> 
        (match (digadd ZERO h rise) with
        | (con, ris) -> (adding [] (reversebin t) ris) @ [con])
      | (h1::t1, h2::t2) -> 
        (match (digadd h1 h2 rise) with
        | (con, ris) -> (adding (reversebin t1) (reversebin t2) ris) @ [con])
    in
      adding b1 b2 ZERO;;

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec reversebin : bin -> bin
  = fun b ->
    match b with 
    | [] -> []
    | h :: t ->  (reversebin t) @ [h]
  in
  match (reversebin b1) with
  | [] -> [ZERO]
  | [ZERO] -> [ZERO]
  | h1 :: t1 -> 
    match (reversebin b2) with
    | [] -> [ZERO]
    | [ZERO] -> [ZERO]
    | h2 :: t2 -> 
      match h2 with
      | ONE -> badd b1 ((bmul b1 (reversebin t2)) @ [ZERO]) 
      | ZERO -> (bmul b1 (reversebin t2)) @ [ZERO];;
