(* problem 1 *)

type btree = Empty | Node of int * btree * btree

let rec  mirror : btree -> btree
= fun t -> match t with
  | Empty -> Empty
  | Node(x, y, z) -> Node(x, mirror z, mirror y)

(* problem 2 *)
type nat = ZERO | SUCC of nat
let rec make_num t1 = match t1 with
  | ZERO -> 0
  | SUCC(x) -> if (x = ZERO) then 1 else (1 + make_num x)
let rec add = fun num -> match num with
  | 0 -> ZERO 
  | 1 -> SUCC(ZERO)
  | _ -> SUCC(add (num-1))
let natadd : nat -> nat -> nat
= fun n1 n2 -> add((make_num n1) + (make_num n2))
let natmul : nat -> nat-> nat
= fun n1 n2 -> add((make_num n1)*(make_num n2))
let rec exp = fun b n -> match n with
  | 0 -> 1
  | _ -> if (n mod 2 = 0) then (exp b (n/2))*(exp b (n/2)) else b*(exp b (n-1))
let natexp : nat -> nat -> nat
= fun n1 n2 -> add(exp (make_num n1) (make_num n2))

(* problem 3 *)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
let rec find_x
= fun f -> match f with
  | True -> True
  | False -> False
  | Var(x) -> Var(x)
  | Neg(x) -> find_x x
  | And(x, y) -> find_x x
  | Or(x, y) -> find_x x
  | Imply(x, y) -> find_x x
  | Iff(x, y) -> find_x x
let rec find_y
= fun f -> match f with
  | True -> True
  | False -> False
  | Var(y) -> Var(y)
  | Neg(y) -> find_y y
  | And(x, y) -> find_y y
  | Or(x, y) -> find_y y
  | Imply(x, y) -> find_y y
  | Iff(x, y) -> find_y y
let rec same
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | Neg(x) -> if ((same x) = 1) then 0 else 1
  | And(x, y) -> (same x)*(same y)
  | Or(x, y) -> if ((same x) + (same y)) = 0 then 0 else 1(*((same x) + (same y))/2*)
  | Imply(x, y) -> if ((same x) = 1)&&((same y) = 0) then 0 else 1
  | Iff(x, y) -> if((same x) = (same y)) then 1 else 0
let rec same_f
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 0
  | Neg(x) -> if ((same_f x) = 1) then 0 else 1
  | And(x, y) -> (same_f x)*(same_f y)
  | Or(x, y) -> if ((same_f x) + (same_f y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((same_f x) = 1)&&((same_f y) = 0) then 0 else 1
  | Iff(x, y) -> if ((same_f x) = (same_f y)) then 1 else 0
let rec dif
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | Neg(x) -> if ((dif x) = 1) then 0 else 1
  | And(x, y) -> (dif x)*(same_f y)
  | Or(x, y) -> if ((dif x) + (same_f y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((dif x) = 1)&&((same_f y) = 0) then 0 else 1
  | Iff(x, y) -> if ((dif x) = (same_f y)) then 1 else 0
let rec dif_f
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 0
  | Neg(x) -> if ((dif_f x) = 1) then 0 else 1
  | And(x, y) -> (dif_f x)*(same y)
  | Or(x, y) -> if ((dif_f x) + (same y)) = 0 then 0 else 1
  | Imply(x, y) -> if ((dif_f x) = 1) && ((same y) = 0) then 0 else 1
  | Iff(x, y) -> if ((dif_f x) = (same y)) then 1 else 0
let check
= fun f -> match f with
  | True -> 1
  | False -> 0
  | Var(x) -> 1
  | _ -> if ((find_x f) = (find_y f)) then (same f) + (same_f f)
         else (same f) + (same_f f) + (dif f) + (dif_f f)
let sat : formula -> bool
= fun f -> match f with
  | _ -> if ((check f) = 0) then false else true

(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
let extract 
= fun l -> match l with
  | [] -> []
  | hd::tl -> hd
let rec diff : aexp * string -> aexp
= fun (e, x) -> match e with
  | Const(a) -> Const 0
  | Var(s) -> if (s = x) then Const 1 else Const 0
  | Power(s, i) -> if (s = x) then Times [Const (i); Power(s, (i-1))] else Const 0
  | Times(l) -> begin match l with
          | [] -> Const 0
          | hd::tl -> Sum [Times(diff(hd, x)::tl); Times[hd; diff(Times(tl), x)]]
          end
  | Sum(l) -> begin match l with
          | [] -> Const 0
          | hd::tl -> Sum [(diff (hd, x)); (diff (Times(tl), x))]
          end
          
(* problem 5 *)
type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
let rec form 
= fun exp n -> match exp with 
  | X -> n
  | INT(x) -> x
  | ADD(x, y) -> (form x n) + (form y n)
  | SUB(x, y) -> (form x n) - (form y n)
  | MUL(x, y) -> (form x n) * (form y n)
  | DIV(x, y) -> (form x n) / (form y n)
let rec result
= fun exp n1 n2 -> match n1 with 
  | _ -> if (n1 = n2) then (form exp n1) else ((form exp n1) + (result exp (n1+1) n2))
let rec scope_x
= fun exp -> match exp with
  | INT(x) -> x
  | ADD(x, y) -> (scope_x x) + (scope_x y)
  | SUB(x, y) -> (scope_x x) - (scope_x y)
  | MUL(x, y) -> (scope_x x) * (scope_x y)
  | DIV(x, y) -> (scope_x x) / (scope_x y)
  | SIGMA(x, y, z) -> result z (scope_x x) (scope_x y) 
let rec scope_y
= fun exp -> match exp with
  | INT(y) -> y
  | ADD(x, y) -> (scope_y x) + (scope_y y)
  | SUB(x, y) -> (scope_y x) - (scope_y y)
  | MUL(x, y) -> (scope_y x) * (scope_y y)
  | DIV(x, y) -> (scope_y x) / (scope_y y)
  | SIGMA(x, y, z) -> result z (scope_y x) (scope_y y) 
let rec expression
= fun exp -> match exp with
  | SIGMA(_, _, z) -> z
let rec check
= fun exp -> match exp with
  | SIGMA(_, _, _) -> 1
  | _ -> 0
let rec calculator : exp -> int
= fun e -> match e with
  | INT(x) -> x
  | ADD(x, y) -> (calculator x) + (calculator y)
  | SUB(x, y) -> (calculator x) - (calculator y)
  | MUL(x, y) -> (calculator x) * (calculator y)
  | DIV(x, y) -> (calculator x) / (calculator y)
  | SIGMA(x, y, z) -> if ((check (expression e)) = 0) then result (expression e) (scope_x x) (scope_y y) else ((scope_y y) - (scope_x x) + 1)*(calculator z)

(* problem 6 *)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
let rec sum_w
= fun m -> match m with
  | (SimpleBranch (x1, y1), SimpleBranch (x2, y2)) -> y1 + y2
  | (SimpleBranch (x1, y1), CompoundBranch (x2, y2)) -> y1 + (sum_w y2)
  | (CompoundBranch (x1, y1), SimpleBranch (x2, y2)) -> (sum_w y1) + y2
  | (CompoundBranch (x1, y1), CompoundBranch (x2, y2)) -> (sum_w y1) + (sum_w y2)
let rec sum 
= fun b -> match b with
  | SimpleBranch (x1, y1) -> x1
  | CompoundBranch (x1, y1) -> sum_w y1
let rec total 
= fun b -> match b with
  | SimpleBranch (x1, y1) -> x1*y1
  | CompoundBranch (x1, y1) -> x1 * (sum b)
let rec balanced : mobile -> bool
= fun m -> match m with
  | (SimpleBranch (x1, y1), SimpleBranch (x2, y2)) -> if (x1*y1) = (x2*y2) then true else false
  | (SimpleBranch (x1, y1), CompoundBranch (x2, y2)) -> if (total (SimpleBranch (x1, y1)) = (total (CompoundBranch (x2, y2)))) then true&&(balanced y2) else false
  | (CompoundBranch (x1, y1), SimpleBranch (x2, y2)) -> if (total (CompoundBranch (x1, y1)) = (total (SimpleBranch (x2, y2)))) then true&&(balanced y1) else false
  | (CompoundBranch (x1, y1), CompoundBranch (x2, y2)) -> if (total (CompoundBranch (x1, y1)) = (total (CompoundBranch (x2, y2)))) then true&&(balanced y1)&&(balanced y2) else false

(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list
let rec length l = match l with 
  | [] -> 0
  | hd::tl -> 1 + (length tl)
let rec fastexpt
= fun n -> match n with
  | 1 -> 1
  | _ -> 2*(fastexpt (n-1))
let rec btod
= fun b n-> match b with
  | [] -> 0
  | hd::tl -> begin match hd with
          | ZERO -> 0 + (btod tl (n-1))
          | ONE -> (fastexpt n) + (btod tl (n-1))
          end
let rec dtob
= fun n -> match n with
  | 0 -> [ZERO]
  | 1 -> [ONE]
  | _ -> if (n mod 2 = 1) then (dtob (n/2))@[ONE] else (dtob (n/2))@[ZERO]
let bmul : bin -> bin -> bin 
= fun b1 b2 -> (dtob ((btod b1 (length b1))*(btod b2 (length b2))))
