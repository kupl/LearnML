(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
| Empty -> Empty
| Node (p, l, r) -> Node (p, mirror r, mirror l)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(nat) -> SUCC(natadd nat n2) 

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
| SUCC ZERO -> n1
| SUCC(nat) -> let inside = natmul n1 nat in natadd n1 inside
| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| _ -> match n2 with
      | SUCC ZERO -> n1
      | SUCC(nat) -> let inside = natexp n1 nat in natmul n1 inside
      | ZERO -> SUCC ZERO


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

let rec sat : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Var x -> if x = "P" then true else false (*preset P is true as the question has described*)
| Neg x -> if sat x = true then false else true
| And (e1, e2) -> sat e1 && sat e2
| Or (e1, e2) -> sat e1 || sat e2
| Imply (e1, e2)-> sat (Neg(e1)) || sat e2
| Iff (e1, e2) -> (sat (Neg(e1)) || sat e2) && (sat (Neg(e2)) || sat e1)


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
| Const c -> Const 0
| Var v -> if v = x then Const 1 else Var v
| Power (str, po) -> begin match str with
      | x -> Times [Const po ; Power(str, po -1)]
     (* | _ -> Power (str, po) *)
  end
| Times l -> begin match l with
      | [] -> raise (Failure "wrong format")
      | hd::tl -> let inner_hd = match tl with
                  | hd2 :: tl2 -> hd2
                  | [] -> Const 0

      in Sum[Times[diff (hd, x); inner_hd]; Times[hd ; diff(inner_hd, x)]]
  end
| Sum l -> match l with
      | [] -> raise (Failure "wrong format")
      | hd::tl -> let inner_hd = match tl with
        | hd2 :: tl2 -> hd2
        | [] -> Const 0 in Sum[diff(hd, x); diff(inner_hd, x)]



(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
  | X -> raise (Failure "not applicable")
  | SIGMA(e1, e2, e3) ->
    let rec evaluateExpInSig : exp -> int -> int
        = fun expWithX xValue ->
        match expWithX with
        | X -> xValue
        | INT (i) -> i
        | ADD (e1, e2) -> (evaluateExpInSig e1 xValue) + (evaluateExpInSig e2 xValue)
        | SUB (e1, e2) -> (evaluateExpInSig e1 xValue) - (evaluateExpInSig e2 xValue)
        | MUL (e1, e2) -> (evaluateExpInSig e1 xValue) * (evaluateExpInSig e2 xValue)
        | DIV (e1, e2) -> (evaluateExpInSig e1 xValue) / (evaluateExpInSig e2 xValue)
        | SIGMA (e'1, e'2, e'3) -> raise (Failure "not applicable with this function ability")
        
      in let rec summation : int -> int -> exp -> int
            = fun init bound expWithX ->
              if init > bound then 0
              else (evaluateExpInSig expWithX init) + (summation (init +1) bound expWithX)
          in (summation (evaluateExpInSig e1 0) (evaluateExpInSig e2 0) e3)

  | INT (i) -> i
  | ADD (e1, e2) -> calculator(e1) + calculator(e2)
  | SUB (e1, e2) -> calculator(e1) - calculator(e2)
  | MUL (e1, e2) -> calculator(e1) * calculator(e2)
  | DIV (e1, e2) -> calculator(e1) / calculator(e2)

   
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> let sum =0 in
  let rec accumulate mo = match mo with
  | SimpleBranch (x1,x2), SimpleBranch (y1, y2)-> 
  sum + x2 + y2
  | SimpleBranch (x1,x2), CompoundBranch (y1, mob) -> 
  sum + x2 + accumulate mob
  | CompoundBranch (x1, mob), SimpleBranch (y1, y2) -> 
  sum + accumulate mob + y2
  | CompoundBranch (x1, mob1), CompoundBranch (y1, mob2) -> 
  sum + accumulate mob1 + accumulate mob2

  in let rec measure mob = match mob with
  |SimpleBranch(a, b), SimpleBranch(c, d) ->
  let ltor = a * b in let rtor = c * d in if ltor = rtor then true else false
  |SimpleBranch(a, b), CompoundBranch(c, d) ->
  let ltor = a * b in let sum =0 in let rtor = c * accumulate d in if ltor = rtor then true else false
  |CompoundBranch (a, b), SimpleBranch(c, d) ->
  let sum =0 in let ltor = a * accumulate b in let sum =0 in let rtor = c * d in if ltor = rtor then true else false 
  |CompoundBranch (a, b), CompoundBranch (c,d) ->
  let sum =0 in let ltor = a* accumulate b in let sum =0 in let rtor = accumulate d in if ltor = rtor then true else false
    in match m with
    | SimpleBranch(a1, a2), SimpleBranch(b1, b2) -> if a1 *a2 = b1 * b2 then true else false
    | SimpleBranch(a1, a2), CompoundBranch(b1, m2) -> if measure m = true 
                                                      then balanced m2 
                                                      else false
    | CompoundBranch (a1, m1), SimpleBranch (b1, b2) -> if measure m = true
                                                        then balanced m1
                                                        else false
    | CompoundBranch (a1, m1), CompoundBranch (b1, m2) -> if measure m = true
                                                        then if balanced m1 = true then balanced m2
                                                              else false
                                                      else false





(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec reverse l = match l with
    |[] -> []
    |hd::tl -> (reverse tl) @ hd ::[] in 
    let rec decimalize : bin -> int 
    = fun l -> match l with
     | [] -> 0
     | h::t -> let num = if h = ZERO then 0 else 1
      in num + 2 * (decimalize t) in
       let decimul : bin -> bin -> int 
       = fun b1 b2 -> let d1 = decimalize (reverse b1) in 
                      let d2 = decimalize (reverse b2) in
              d1 * d2
        in let rec binarize : int -> bin 
        = fun decimul -> match decimul with
        | 0 -> []
        | i -> let digi = (decimul mod 2) in
              if digi = 0 then binarize (decimul/2) @ [ZERO]
              else binarize (decimul/2) @ [ONE]
               in binarize (decimul b1 b2)