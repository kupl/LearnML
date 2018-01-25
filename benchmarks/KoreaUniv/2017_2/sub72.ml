(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Empty -> Empty
  | Node (n, left, right) -> Node (n, mirror right, mirror left) 


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC n3 -> SUCC (natadd n3 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  if n1 = ZERO then ZERO
  else match n2 with
  | ZERO -> ZERO
  | SUCC n3 -> natadd (n1) (natmul n1 n3)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  if n2 = ZERO then SUCC ZERO
  else match n2 with
  | SUCC n3 -> natmul (n1) (natexp n1 n3)


(* problem 3*)
type formula =
True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let sat : formula -> bool 
= fun f ->
let rec varfinder1 : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list 
= fun var l1 l2 -> 
  match l1 with
  | [] -> [(var, true)]@l2
  | (a, b)::tail -> 
    if (a = var) then l2
    else varfinder1 var tail l2 in

    let rec varfinder2 : formula -> (formula * bool) list -> bool 
    = fun var l -> 
      match l with
      | [] -> true
      | (a, b)::tail -> 
        if var = a then b
        else varfinder2 var tail in


      let rec appendlist : formula -> (formula * bool) list -> (formula * bool) list 
      = fun f l -> 
        match f with
        | True -> []
        | False -> []
        | Var a -> varfinder1 (Var a) [] []
        | Neg a -> appendlist a l
        | And (a, b) -> (appendlist a l)@(appendlist b l)
        | Or (a, b) -> (appendlist a l)@(appendlist b l)
        | Imply (a, b) -> (appendlist a l)@(appendlist b l)
        | Iff (a, b) -> (appendlist a l)@(appendlist b l) in

let setter = appendlist f [] in

let tfswap : (formula * bool) list -> (formula * bool) list = fun l
-> match l with
| [] -> []
| (a, b)::tail -> if b = true then ((a, false)::tail) else tail in

let rec showstate : formula -> (formula * bool) list -> bool = fun f table
-> match f with
| True -> true
| False -> false
| Var a -> varfinder2 (Var a) table
| Neg a -> not(showstate a table)
| And (a, b) -> (showstate a table) && (showstate b table)
| Or (a, b) -> (showstate a table) || (showstate b table)
| Imply (a, b) -> if ((showstate a table) = true) && ((showstate b table) = false) then false else true
| Iff (a, b) -> if (showstate a table) = (showstate b table) then true else false in

let rec test : (formula * bool) list -> bool = fun l
-> match l with
| [] -> false
| (a,b)::tail -> b || (test tail) in

let rec result : formula -> (formula * bool) list -> bool = fun f l
-> if (test l) = false then showstate f l
else (showstate f l) || (result f (tfswap l)) in
result f setter

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
  match e with
  | Const n -> Const 0
  | Var n -> if n = x then Const 1 else Const 0
  | Power (y , n) -> if y = x then Times [Const n ; Power (y, n-1)] else Const 0
  | Times lis1 -> begin
      match lis1 with
      |[] -> Const 0
      | hd::tl -> Sum[ Times ([diff (hd, x)]@tl) ; Times [ hd ; diff (Times tl, x)]]
    end
  | Sum lis2 ->
      match lis2 with
      | [] -> Const 0
      | hd::tl -> Sum [ diff (hd, x) ; diff (Sum tl, x)]

(* problem 5*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
  match e with
  | INT n -> n
  | ADD (a, b) -> (calculator a) + (calculator b)
  | SUB (a, b) -> (calculator a) - (calculator b)
  | MUL (a, b) -> (calculator a) * (calculator b)
  | DIV (a, b) -> (calculator a) / (calculator b)
  | SIGMA (a, b, c) -> if (calculator a)>(calculator b) then 0 else eval(c,a) + calculator (SIGMA ( ADD( a, INT 1),  b, c))
and eval (f, x) =
  match f with
  | X -> calculator x
  | INT n -> n
  | ADD(a,b) -> eval(a, x) + eval (b,x)
  | SUB(a,b) -> eval(a, x) - eval (b,x)
  | MUL(a,b) -> eval(a, x) * eval (b,x)
  | DIV(a,b) -> eval(a, x) / eval (b,x)
    (* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
  let rec totalweight : mobile -> int
  = fun m ->
  match m with 
  | (SimpleBranch (a1, b1), SimpleBranch (a2, b2)) -> b1+b2
  | (CompoundBranch (a1, b1), SimpleBranch (a2, b2)) -> (totalweight b1)+ b2
  | (SimpleBranch (a1, b1), CompoundBranch(a2, b2)) -> b1+(totalweight b2)
  | (CompoundBranch (a1, b1), CompoundBranch(a2,b2)) -> (totalweight b1) + (totalweight b2)
  in let rec balance : mobile->bool->bool
  = fun m b -> 
  if b=false then false else 
    match m with
    | (SimpleBranch (a1, b1), SimpleBranch (a2, b2)) -> if (a1*b1) = (a2*b2) then true else false
    | (CompoundBranch (a1, b1), SimpleBranch (a2, b2)) -> if (balance b1 true) = false then false 
                                                          else if ((totalweight b1)*a1) = (a2*b2) then true
                                                          else false 
    | (SimpleBranch (a1, b1), CompoundBranch(a2, b2)) -> if (balance b2 true) = false then false 
                                                          else if ((totalweight b2)*a2) = (a1*b1) then true
                                                          else false 
    | (CompoundBranch (a1, b1), CompoundBranch(a2,b2)) -> if (balance b2 true)&&(balance b1 true) = false then false 
                                                          else if ((totalweight b2)*a2) = ((totalweight b1)*a1) then true
                                                          else false 
  in balance m true

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
  let rec lis : bin -> 'a list 
  = fun a ->
    match a with
    | []->[]
    | hd::tl -> 
      if (hd = ONE) then (lis tl)@[1]
      else (lis tl)@[0] in 
      let rec fastexpt : int -> int -> int
      = fun b n -> 
        if n = 0 then 1
        else if (n mod 2 = 0) then (fastexpt b (n/2)) * (fastexpt b (n/2))
        else b * (fastexpt b (n-1)) in 
        let rec dconvert : int -> 'a list -> int
        = fun c d ->
          match d with
          |[]->0
          | hd::tl -> (hd * (fastexpt 2 c))+(dconvert (c+1) tl) in 
          let rec bconvert : int -> bin -> bin
          = fun e f ->
            if e<1 then f
            else if (e mod 2) = 0 then bconvert (e/2) ([ZERO]@ f)
            else bconvert (e/2) ([ONE] @ f) in 
            let g = dconvert 0 (lis b1) in
            let h = dconvert 0 (lis b2) in
            bconvert (g*h) []









