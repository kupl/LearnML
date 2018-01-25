(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Node(a, b, c) -> Node(a, (mirror c), (mirror b))
  | _ -> Empty
;;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC(r) -> natadd r (SUCC(n2))
;;

let natmul : nat -> nat -> nat 
= fun n1 n2 ->
  let rec iter
  = fun c res ->
    match c with
    | ZERO -> res
    | SUCC(r) -> iter r (natadd n2 res)
  in
  iter n1 ZERO
;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
  let rec iter
  = fun c res ->
    match c with
    | ZERO -> res
    | SUCC(r) -> iter r (natmul n1 res)
  in
  iter n2 (SUCC(ZERO))
;;

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
= fun f -> true

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
  | Const(a) -> Const(0)
  | Var(v) -> Const(1)
  | Power(v, a) -> if v = x then Times [Const a; Power(v, (a-1))] else Const(0)
  | Times(l) ->
      let rec iter : aexp list -> aexp list -> aexp list -> aexp list
      = fun l1 l2 sum ->
        match l1 with
        | [] -> sum
        | hd::tl -> iter tl (hd::l2) ((Times((diff(hd,x))::tl@l2))::sum)
      in
      Sum(iter l [] [])
  | Sum(l) ->
      let rec iter
      = fun l res ->
        match l with
        | [] -> res
        | hd::tl -> iter tl ((diff (hd,x))::res)
      in
      Sum(iter l [])
;;

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
  let rec placement : exp -> exp -> exp
  = fun e a ->
    match e with
    | X -> a
    | INT(x) -> INT(x)
    | ADD(x, y) -> ADD(placement x a, placement y a)
    | SUB(x, y) -> SUB(placement x a, placement y a)
    | MUL(x, y) -> MUL(placement x a, placement y a)
    | DIV(x, y) -> DIV(placement x a, placement y a)
    | SIGMA(a, b, c) -> INT(calculator(SIGMA(a, b, c)))
  in
  match e with
  | X -> 0
  | INT(a) -> a
  | ADD(a, b) -> calculator(a) + calculator(b)
  | SUB(a, b) -> calculator(a) - calculator(b)
  | MUL(a, b) -> calculator(a) * calculator(b)
  | DIV(a, b) -> calculator(a) / calculator(b)
  | SIGMA(a, b, c) ->
      let rec sigma
      = fun a b c ->
        if a = b then calculator (placement c a)
        else
          (calculator(placement c a)) + (sigma (INT(calculator(ADD(a, INT(1))))) b c)
      in
      sigma a b c
;;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec subm
  = fun m ->
    let bw
    = fun b ->
      match b with
      | SimpleBranch(l,w) -> l*w
      | CompoundBranch(l,m) -> l*(subm m)
    in
    let rec w
    = fun b ->
      match b with
      | SimpleBranch(l,w) -> w
      | CompoundBranch(l,m) ->
        match m with
        | (l, r) -> (w l) + (w r)
    in
    match m with
    | (a, b) ->
      if (bw a) != (bw b) then 0
      else if (w a) = 0 then 0
      else if (w b) = 0 then 0
      else (w a)+(w b)
  in
  ((subm m) != 0)
;;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let bintoint : bin -> int
  = fun binary ->
    let rec bi : bin -> int -> int
    = fun b sum ->
      match b with
      | hd::tl -> bi tl ((sum*2)+(if hd=ONE then 1 else 0))
      | [] -> sum
    in
    bi binary 0
  in
  let inttobin : int -> bin
  = fun integer ->
    let rec ib : int -> bin -> bin
    = fun i b ->
      if i > 0 then (ib (i/2) ((if i mod 2 = 0 then ZERO else ONE)::b))
      else b
    in
    ib integer []
  in
  inttobin ((bintoint b1)*(bintoint b2))
;;