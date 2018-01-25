(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> (* TODO *)
  match t with
    | Empty -> Empty
    | Node(key, lb, rb) -> Node(key, mirror rb, mirror lb)




(* problem 2*)
type nat = ZERO | SUCC of nat

let rec print : int -> nat
= fun num ->
  if (num = 0) then ZERO
  else SUCC (print (num-1))

let rec expt b n =
  if (n = 0) then 1
  else b * (expt b (n-1))

let rec count : nat -> int -> int
= fun n1 num ->
  match n1 with
    | ZERO -> num
    | SUCC nat -> count nat num+1

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if(((count n1 0) = 0) && ((count n2 0) = 0)) then ZERO
  else print((count n1 0) + (count n2 0))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if(((count n1 0) = 0) || ((count n2 0) = 0)) then ZERO
  else print((count n1 0) * (count n2 0))

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if((count n1 0) = 0) then ZERO
  else if((count n2 0) = 0) then SUCC ZERO
  else print((expt (count n1 0) (count n2 0)))




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

let rec logic : formula -> bool
= fun f ->
  match f with
    | True -> true
    | False -> false
    | Var str -> true
    | Neg form -> 
      if (form = True) then false
      else if (form = False) then true
      else if (logic form) then true
      else false
    | And (form1, form2) ->
      if((form1 = Neg form2)||(form2 = Neg form1)) then false
      else if ((logic form1)&&(logic form2)) then true
      else false
    | Or (form1, form2) ->
      if ((logic form1)||(logic form2)) then true
      else false
    | Imply (form1, form2) ->
      if ((logic form1)&&(logic form2)) then true
      else if (((logic form1)=true)&&((logic form2)=false)) then false
      else if (((logic form1)=false)&&((logic form2)=true)) then true
      else true
    | Iff (form1, form2) ->
      if ((logic form1)&&(logic form2)) then true
      else if(((logic form1) = false) && ((logic form2) = false)) then true
      else false 

let sat : formula -> bool
= fun f -> (* TODO *)
  match f with
    | True -> true 
    | False -> false 
    | Var str -> true
    | Neg form -> logic f
    | And (form1, form2) -> logic f
    | Or (form1, form2) -> logic f
    | Imply (form1, form2) -> logic f
    | Iff (form1, form2) -> logic f




(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
  match e with
    | Const integer -> Const 0
    | Var str -> 
      if(str = x) then Const 1
      else Const 0
    | Power (str, integer) -> 
      if(integer < 0) then Const 0
      else if(integer = 1) then Const 1
      else Times [Const integer; Power (str, (integer - 1))]
    | Times al1 ->
      (match al1 with
        | [] -> Const 0
        | [aexp1] -> diff (aexp1, x)
        | h::t -> Sum [Times [diff (h, x); Times t]; Times [h; diff (Times t, x)]])
    | Sum al2 ->
      (match al2 with
        | [] -> Const 0
        | [aexp1] -> diff (aexp1, x)
        | h::t -> Sum [diff (h, x); diff (Sum t, x)])




(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> (* TODO *)
  match e with
    | X -> 0
    | INT integer -> integer
    | ADD (exp1, exp2) -> (calculator exp1 + calculator exp2)
    | SUB (exp1, exp2) -> (calculator exp1 - calculator exp2)
    | MUL (exp1, exp2) -> (calculator exp1 * calculator exp2)
    | DIV (exp1, exp2) -> 
      if (calculator exp2 = 0) then 0
      else (calculator exp1 / calculator exp2)
    | SIGMA (exp1, exp2, exp3) ->
        let rec cal : int * exp -> int
        = fun (index, form) -> 
          match form with
          | X -> index
          | INT integer -> integer
          | ADD (exp1, exp2) -> (cal (index, exp1) + cal (index, exp2))
          | SUB (exp1, exp2) -> (cal (index, exp1) - cal (index, exp2))
          | MUL (exp1, exp2) -> (cal (index, exp1) * cal (index, exp2))
          | DIV (exp1, exp2) -> (cal (index, exp1) / cal (index, exp2))
          | SIGMA (exp1, exp2, exp3) -> calculator (SIGMA (exp1, exp2, exp3)) in
      if (calculator exp1 > calculator exp2) then 0 
      else (cal (calculator exp1, exp3) + calculator(SIGMA (ADD (exp1, INT 1), exp2, exp3)))




(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sumOfWeight : branch -> int
= fun bra ->
  match bra with
    | SimpleBranch (len, wei) -> wei
    | CompoundBranch (len, mob) -> 
      match mob with 
        | (branch1, branch2) -> (sumOfWeight branch1 + sumOfWeight branch2)
  
let rec calAll : branch -> int
= fun bra ->
  match bra with
    | SimpleBranch (len, wei) -> (len * wei)
    | CompoundBranch (len, mob) -> 
      match mob with
        | (branch1, branch2) -> (len * (sumOfWeight branch1 + sumOfWeight branch2))

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
  match m with
    | (branch1, branch2) ->
      match branch1 with
        | SimpleBranch (len1, wei1) ->
          (match branch2 with
            | SimpleBranch (len2, wei2) -> 
              if (calAll branch1 = calAll branch2) then true
              else false
            | CompoundBranch (len2, mob) ->
              if (balanced mob) then
                (if (calAll branch1 = calAll branch2) then true
                else false)
              else false)
        | CompoundBranch (len1, mob1) ->
          if (balanced mob1) then
            (match branch2 with
              | SimpleBranch (len2, wei2) ->
                if (calAll branch1 = calAll branch2) then true
                else false
              | CompoundBranch (len2, mob2) ->
                if (balanced mob2) then 
                  (if (calAll branch1 = calAll branch2) then true
                  else false)
                else false)
          else false




(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length l =
  match l with
    | [] -> 0
    | h::t -> length t + 1

let rec btod : bin * int -> int
= fun (b, len)->
  match b with
    | [] -> 0
    | h::t -> 
      if (h = ZERO) then btod(t, length t)
      else (expt 2 (len-1) + btod(t, length t))
  
let rec dtob : int -> bin
= fun integer -> 
  if (integer = 0) then [ZERO]
  else if (integer = 1) then [ONE]
  else if (integer mod 2 = 0) then dtob(integer/2)@[ZERO]
  else dtob(integer/2)@[ONE]

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  dtob (btod (b1, length b1) * btod (b2, length b2))