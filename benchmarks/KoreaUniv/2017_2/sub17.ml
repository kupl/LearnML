(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree = fun t ->
  match t with
  |Empty -> Empty
  |Node(n, t1, t2) -> Node(n, mirror t2, mirror t1)

(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 ->
  match n1 with 
  | ZERO -> n2
  | SUCC n -> SUCC (natadd n n2)

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n -> natadd n2 (natmul n n2)

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC n -> natmul n1 (natexp n1 n)

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

let sat : formula -> bool = fun f ->
  let rec isinlist : string list -> string -> bool = fun strlist v ->
    (match strlist with
     | [] -> false
     | hd::tl -> if hd = v then true else isinlist tl v
    )
  in
  let rec isinlist2 : (string * bool) list -> string -> bool = fun table v ->
    (match table with
     | [] -> false
     | (str, b)::tl -> if str = v then b else isinlist2 tl v
    )
  in
  let rec find_vals : formula -> string list = fun form ->
    (match form with
    | True| False -> []
    | Var v -> [v]
    | Neg v -> find_vals v
    | And (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Or (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Imply (v1, v2) -> (find_vals v1)@(find_vals v2)
    | Iff (v1, v2) -> (find_vals v1)@(find_vals v2)
    )
  in
  let rec edditlist : string list -> string list = fun strlist ->
    (match strlist with
     | [] -> []
     | hd::tl -> if isinlist tl hd then tl else hd::(edditlist tl)
    )
  in
  let rec calform : formula -> (string * bool) list -> bool = fun form table ->
    (match form with
     | True -> true
     | False -> false
     | Var v -> isinlist2 table v
     | Neg v -> not (calform v table)
     | And (v1, v2) -> (calform v1 table) && (calform v2 table)
     | Or (v1, v2) -> (calform v1 table) || (calform v2 table)
     | Imply (v1, v2) -> (not (calform v1 table)) || (calform v2 table)
     | Iff (v1, v2) -> let b1 = calform v1 table in let b2 = calform v2 table in (b1 && b2) || ((not b1) && (not b2))
    )
  in
  let rec calall : formula -> string list -> (string * bool) list -> bool = fun form strlist table ->
    (match strlist with
    | [] -> calform form table
    | hd::tl -> 
      if calall form tl ((hd, true)::table) then true
      else if calall form tl ((hd, false)::table) then true else false
    )
  in
  let vallist = edditlist (find_vals f) in
  calall f vallist []

(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e, x) ->
  match e with
  | Const _ -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (b, n) -> if b <> x then Const 0 else
    (match n with
     | 0 -> Const 0
     | c -> Times [Const c; Power (b, c - 1)]
    )
  | Times fx -> 
    (match fx with
      | [] -> Const 0
      | hd::[] -> diff (hd, x)
      | hd::tl ->  Sum [Times [diff (hd, x); Times tl]; Times [hd; diff (Times tl, x)]]
    )
  | Sum el ->
    (match el with
     | [] -> Const 0
     | hd::[] -> diff (hd, x)
     | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)]

    )

(* problem 5 *)
type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Foo of string

let rec calculator : exp -> int = fun e ->
  let rec exptofunc : exp -> (int -> int) = fun f -> 
    (match f with
     | X -> (fun x -> x)
     | INT n -> (fun x -> n)
     | ADD (e1, e2) ->
        (fun x -> (exptofunc e1 x) + (exptofunc e2 x))
     | SUB (e1, e2) ->
        (fun x -> (exptofunc e1 x) - (exptofunc e2 x))
     | MUL (e1, e2) ->
        (fun x -> (exptofunc e1 x) * (exptofunc e2 x))
     | DIV (e1, e2) ->
        (fun x -> (exptofunc e1 x) / (exptofunc e2 x))
     | _ -> raise (Foo "Too Much Sigma")
    )
  in
  let rec calsigma : int -> int -> (int -> int) -> int = fun n0 n f ->
    if n0 > n then 0 else
      (f n0) + (calsigma (n0 + 1) n f)
  in
  match e with
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (n1, n2, f) -> calsigma (calculator n1) (calculator n2) (exptofunc f)
  | _ -> raise (Foo "No Info of X")

(* problem 6 *)
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
            | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec balanced : mobile -> bool = fun m -> 
  let rec getmobileweight : mobile -> weight = fun mob -> 
    (match mob with
      | (m1, m2) ->
        (match m1 with 
          | SimpleBranch (_, w1) -> w1
          | CompoundBranch (_, subm) -> getmobileweight subm
        ) + 
        (match m2 with
          | SimpleBranch (_, w2) -> w2
          | CompoundBranch (_, subm) -> getmobileweight subm
        )
    )
  in
  match m with
  | (lb, rb) ->
    let balanceleft = 
      match lb with
      | SimpleBranch (len, wgt) -> (len*wgt, true)
      | CompoundBranch (len, subm) -> (len*(getmobileweight subm), balanced subm)
    in
    let balanceright =
      match rb with
      | SimpleBranch (len, wgt) -> (len*wgt, true)
      | CompoundBranch (len, subm) -> (len*(getmobileweight subm), balanced subm)
    in
    match (balanceleft, balanceright) with
      |((lt, lbool), (rt, rbool)) -> if lbool && rbool then lt=rt else false

(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin = fun b1 b2 ->
  let rec blen : bin -> int = fun b ->
    (match b with
      | [] -> 0
      | _::tl -> 1 + blen tl
    )
  in
  let rec zadd : bin -> int -> bin = fun b n ->
    (match n with
      | 0 -> b
      | t -> zadd (ZERO::b) (t-1)
    )
  in
  let rec zrmv : bin -> bin = fun b -> 
    (match b with
      | [] -> []
      | hd::tl -> 
        (match hd with
          | ZERO -> zrmv tl
          | _ -> b
        )
    )
  in
  let rec bflip : bin -> bin = fun b ->
    (match b with
     | [] -> []
     | hd::tl -> (bflip tl) @ [hd]
    )
  in
  let rec fadd : bin -> bin -> bin -> bin = fun b1 b2 carry ->
    (match (b1, b2, carry) with
    |([], [], c) -> c
    |(hd1::tl1, hd2::tl2, c::_) ->
      (match (hd1, hd2, c) with
       | (ZERO, ZERO, ZERO) 
          -> (fadd tl1 tl2 [ZERO]) @ [ZERO]
       | (ZERO, ZERO, ONE) | (ZERO, ONE, ZERO) | (ONE, ZERO, ZERO) 
          -> (fadd tl1 tl2 [ZERO]) @ [ONE]
       | (ONE, ONE, ZERO) | (ONE, ZERO, ONE) | (ZERO, ONE, ONE)  
          -> (fadd tl1 tl2 [ONE]) @ [ZERO]
       | (ONE, ONE, ONE) 
          -> (fadd tl1 tl2 [ONE]) @ [ONE]
      )
    )
  in
  let badd : bin -> bin -> bin = fun b1 b2 ->
    let l1 = blen b1 in let l2 = blen b2 in
    (match if l1 > l2 then (b1, zadd b2 (l1 - l2)) else (zadd b1 (l2 - l1), b2) with
    | (bin1, bin2) -> fadd (bflip bin1) (bflip bin2) [ZERO]
    )
  in
  let result = zrmv (match (b1, b2) with
  | ([ZERO], _) | (_, [ZERO]) -> [ZERO]
  | ([ONE], b) | (b, [ONE] ) -> b
  | (hd::tl, b) -> badd ((bmul [hd] b) @ (zadd [] (blen tl))) (bmul tl b)
  )
  in
  match result with
  | [] -> [ZERO]
  | _ -> result
  
