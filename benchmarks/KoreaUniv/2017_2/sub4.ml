(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree =
  fun t -> match t with
  Empty -> Empty
  | Node(n, t1, t2) -> Node(n, mirror t2, mirror t1);;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> n2
  |_ ->
    (match n2 with
    ZERO -> n1
    | SUCC(tl) -> let v1 = SUCC(n1) in natadd v1 tl);;

let rec natmul : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> ZERO
  |_ ->
    (match n2 with
    ZERO -> ZERO
    | SUCC(tl) -> let sum = n1 in natadd sum (natmul n1 tl));;

let rec natexp : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> ZERO
  |_ ->
    (match n2 with
    ZERO -> SUCC ZERO
    | SUCC(tl) -> let pro = n1 in natmul pro (natexp n1 tl));;

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
  
  let sat : formula -> bool = fun f
  ->
  let rec getVar : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list = fun var l1 l2
  ->match l1 with
  [] -> [(var, true)]@l2
  | (a, b)::tail -> if (a = var) then l2
          else getVar var tail l2 in
  
  let rec getTable : formula -> (formula * bool) list -> bool = fun var l
  -> match l with
  [] -> true
  | (a, b)::tail -> if var = a then b
          else getTable var tail in
  
  
  let rec makeList : formula -> (formula * bool) list -> (formula * bool) list = fun f l
  -> match f with
  True -> []
  | False -> []
  | Var a -> getVar (Var a) [] []
  | Neg a -> makeList a l
  | And (a, b) -> (makeList a l)@(makeList b l)
  | Or (a, b) -> (makeList a l)@(makeList b l)
  | Imply (a, b) -> (makeList a l)@(makeList b l)
  | Iff (a, b) -> (makeList a l)@(makeList b l) in
  
  let makedTable = makeList f [] in
  
  let changeBool : (formula * bool) list -> (formula * bool) list = fun l
  -> match l with
  [] -> []
  | (a, b)::tail -> if b = true then ((a, false)::tail) else tail in
  
  let rec formulatbl : formula -> (formula * bool) list -> bool = fun f table
  -> match f with
  True -> true
  | False -> false
  | Var a -> getTable (Var a) table
  | Neg a -> not(formulatbl a table)
  | And (a, b) -> (formulatbl a table) && (formulatbl b table)
  | Or (a, b) -> (formulatbl a table) || (formulatbl b table)
  | Imply (a, b) -> if ((formulatbl a table) = true) && ((formulatbl b table) = false) then false else true
  | Iff (a, b) -> if (formulatbl a table) = (formulatbl b table) then true else false in
  
  let rec eval : (formula * bool) list -> bool = fun l
  -> match l with
  [] -> false
  | (a,b)::tail -> b || (eval tail) in
  
  let rec evaltbl : formula -> (formula * bool) list -> bool = fun f l
  -> if (eval l) = false then formulatbl f l
  else (formulatbl f l) || (evaltbl f (changeBool l)) in
  evaltbl f makedTable;;
  
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
  fun (e,x) -> match e with
  | Const n -> Const 0
  | Var n -> if n = x then Const 1 else Const 0
  | Power (e1, e2) ->
    if e1 = x then Times[Const e2; Power(e1, e2 - 1)] else Const 0
  | Times lst1 ->
    (match lst1 with
    [] -> Const 0
    | hd :: tl ->
      Sum([Times([diff(hd, x)]@tl); Times([hd;diff(Times tl, x)])]))
  | Sum lst2 ->
    (match lst2 with
    [] -> Const 0
    | hd :: tl -> Sum[diff(hd, x); diff((Sum tl), x)]);;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int =
  fun e -> match e with
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) -> if (calculator e2) >= (calculator e1) then
    (eval(e3, e1) + (calculator (SIGMA(ADD(e1, INT 1), e2, e3)))) else 0
  and eval(f, x) =
    (match f with
    X -> calculator x
    | INT n -> n
    | ADD (e1, e2) -> eval(e1, x) + eval(e2, x)
    | SUB (e1, e2) -> eval(e1, x) - eval(e2, x)
    | MUL (e1, e2) -> eval(e1, x) * eval(e2, x)
    | DIV (e1, e2) -> eval(e1, x) / eval(e2, x));;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool =
  fun m ->
    let rec sum_weight tree =
      match tree with
      (SimpleBranch(e1, e2), SimpleBranch(e3, e4)) -> e2 + e4
      | (SimpleBranch(e1, e2), CompoundBranch(e3, e4)) -> e2 + (sum_weight e4)
      | (CompoundBranch(e1, e2), SimpleBranch(e3, e4)) -> (sum_weight e2) + e4
      | (CompoundBranch(e1, e2), CompoundBranch(e3, e4)) -> (sum_weight e2) + (sum_weight e4)
    in
      (match m with
      (SimpleBranch(e1, e2), SimpleBranch(e3, e4)) ->
        let v1 = (e1 * e2) in let v2 = (e3 * e4) in 
          if v1 = v2 then true else false
      | (SimpleBranch(e1, e2), CompoundBranch(e3, e4)) ->
        if (balanced e4) then let v1 = e1 * e2 in
          let v2 = e3 * (sum_weight e4) in if v1 = v2 then true else false
        else false
      | (CompoundBranch(e1, e2), SimpleBranch(e3, e4)) ->
        if (balanced e2) then let v1 = e1 * (sum_weight e2) in
          let v2 = e3 * e4 in if v1 = v2 then true else false
        else false
      | (CompoundBranch(e1, e2), CompoundBranch(e3, e4)) ->
        if (balanced e2) && (balanced e4) then let v1 = e1 * (sum_weight e2) in
          let v2 = e3 * (sum_weight e4) in if v1 = v2 then true else false
        else false);;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin =
  fun b1 b2 ->
    let rec reverse lst =
      match lst with
      [] -> []
      | hd :: tl -> (reverse tl) @ [hd]
    in
      let rec btd binary pro = 
        match binary with
        [] -> 0
        | [ZERO] -> 0
        | [ONE] -> 1 * pro
        | hd :: tl -> ((btd [hd] 1) * pro) + (btd tl (pro * 2))
      in
        let rec dtb decimal sum =
          if decimal > 0 then
            if decimal mod 2 = 0 then (dtb (decimal / 2) (sum @ [ZERO]))
            else (dtb (decimal / 2) (sum @ [ONE]))
          else sum
        in
          let v1 = (reverse b1) in let v2 = (reverse b2) in
            let decimal_mul = (btd v1 1) * (btd v2 1) in
              (reverse (dtb decimal_mul []));;