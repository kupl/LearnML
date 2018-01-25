(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
  Empty -> Empty
| Node (a,s1,s2) -> Node (a,mirror s2, mirror s1)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> (match n2 with
    ZERO -> ZERO
  | SUCC m2 -> SUCC (natadd n1 m2) )
| SUCC m1 -> (match n2 with
    ZERO -> SUCC (natadd m1 n2)
  | SUCC m2 -> SUCC (SUCC (natadd m1 m2) ) )

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> ZERO 
| _ -> (match n2 with
    ZERO -> ZERO
  | SUCC m2 -> (natadd n1 (natmul n1 m2)))

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> (SUCC ZERO)
| _ -> (match n2 with
    ZERO -> (SUCC ZERO)
    | SUCC m2 -> (natmul n1 (natexp n1 m2)))


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

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

let rec add_var
= fun v env -> match env with 
  [] -> [(v,False)]
| hd::tl -> 
    let str = fst hd in
      if str = v then env
      else hd::(add_var v tl)

let rec lookup_var
= fun f env -> match f with
  True -> env
| False -> env
| Var v -> add_var v env
| Neg f1 -> lookup_var f1 env
| And (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Or (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Imply (f1,f2) -> lookup_var f2 (lookup_var f1 env)
| Iff (f1,f2) -> lookup_var f2 (lookup_var f1 env)

let rec find_val
= fun v env -> match env with 
  [] -> False
| hd::tl -> 
    let str = fst hd in
      if str = v then snd hd
      else find_val v tl

let rec cal_formula
= fun f env -> match f with
  True -> true
| False -> false
| Var v -> cal_formula (find_val v env) env
| Neg f1 -> not(cal_formula f1 env)
| And (f1,f2) -> (cal_formula f1 env) && (cal_formula f2 env)
| Or (f1,f2) -> (cal_formula f1 env) || (cal_formula f2 env)
| Imply (f1,f2) -> (not(cal_formula f1 env)) || (cal_formula f2 env)
| Iff (f1,f2) -> cal_formula ( Or( And(f1,f2), Neg( Or(f1,f2)))) env

let rec gen_states
= fun f envs enve -> match envs with
  [] -> cal_formula f enve
| hd::tl -> 
    let cf = gen_states f tl (((fst hd),(False))::enve) in
    let ct = gen_states f tl (((fst hd),(True))::enve) in
      if (cf = true) || (ct = true) then true
      else false

let rec sat : formula -> bool
= fun f -> 
  let env = lookup_var f [] in
  gen_states f env []
  


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
  Const k -> (Const 0)
| Var v -> 
    if x = v then (Const 1)
    else (Const 0)
| Power (v,n) ->
    if x = v then (Times[(Const n); (Power (v, n-1))])
    else (Const 0)
| Times te ->( match te with
      [] -> (Const 0)
    | hd::tl -> (Sum[ Times ([diff (hd,x)] @ tl); Times (hd :: [diff ((Times tl),x)]) ])
    )
| Sum se ->( match se with
      [] -> (Const 0)
    | hd::tl -> (Sum[diff (hd,x); diff ((Sum tl),x)])
    )



(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal_sigma 
= fun e iter eon ->
if iter > eon then 0
else match e with
  X -> iter
| INT k -> k
| ADD (e1,e2) -> (cal_sigma e1 iter eon) + (cal_sigma e2 iter eon)
| SUB (e1,e2) -> (cal_sigma e1 iter eon) - (cal_sigma e2 iter eon)
| MUL (e1,e2) -> (cal_sigma e1 iter eon) * (cal_sigma e2 iter eon)
| DIV (e1,e2) -> (cal_sigma e1 iter eon) / (cal_sigma e2 iter eon)
| SIGMA (est,eed,form) ->
  let st = (cal_sigma est 0 0) in
  let ed = (cal_sigma eed 0 0) in
  if st > ed then 0
  else if st = ed then (cal_sigma form st ed)
  else (cal_sigma form st ed) + (cal_sigma (SIGMA(INT (st+1),INT ed,form)) 0 0)

let calculator : exp -> int
= fun e -> cal_sigma e 0 0

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

let rec balandw
= fun m ->
    let br1 = fst m in
    let br2 = snd m in
    match br1 with
      SimpleBranch (l1,w1) ->(
        match br2 with
          SimpleBranch (l2,w2) ->
            if (l1*w1) = (l2*w2) then (true,(w1+w2))
            else (false, 0)
        | CompoundBranch (l2,sub2) -> 
            let res2  = balandw sub2 in
            if (fst res2) && ( (l1*w1) = (l2*(snd res2)) ) then (true,(w1+(snd res2)))
            else (false, 0)
      )
    | CompoundBranch (l1,sub1) ->(
        match br2 with
          SimpleBranch (l2,w2) ->
            let res1 = balandw sub1 in
            if (fst res1) && ( (l2*w2) = (l1*(snd res1)) ) then (true,(w2+(snd res1)))
            else (false, 0)

        | CompoundBranch (l2,sub2) ->
            let res1 = balandw sub1 in
            let res2 = balandw sub2 in
            if (fst res1) && (fst res2) && ( (l1*(snd res1)) = (l2*(snd res2)) ) then (true,((snd res1)+(snd res2)))
            else (false, 0)
      )

let balanced : mobile -> bool
= fun m ->
    fst (balandw m)




(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

exception Problem
let hd l = match l with [] -> raise Problem | hd::tl -> hd
let tl l = match l with [] -> raise Problem | hd::tl -> tl

let full_adder 
= fun m ->
  if m = (ZERO,ZERO,ZERO) then (ZERO,ZERO)
  else if m = (ZERO,ZERO,ONE) then (ONE,ZERO)
  else if m = (ZERO,ONE,ZERO) then (ONE,ZERO)
  else if m = (ZERO,ONE,ONE) then (ZERO,ONE)
  else if m = (ONE,ZERO,ZERO) then (ONE,ZERO)
  else if m = (ONE,ZERO,ONE) then (ZERO,ONE)
  else if m = (ONE,ONE,ZERO) then (ZERO,ONE)
  else if m = (ONE,ONE,ONE) then (ONE,ONE)
  else (ZERO,ZERO)

let rec sum
= fun b1 b2 c -> 
  if b1 = [] then 
    if b2 = [] then
      if c = ZERO then []
      else [ONE]
    else 
      let h2 = hd b2 in
      let t2 = tl b2 in
      let res = full_adder(ZERO,h2,c) in
      (fst res)::(sum b1 t2 (snd res))
  else
    let h1 = hd b1 in
    let t1 = tl b1 in
    if b2 = [] then
      let res = full_adder(h1,ZERO,c) in
      (fst res)::(sum t1 b2 (snd res))
    else
      let h2 = hd b2 in
      let t2 = tl b2 in
      let res = full_adder(h1,h2,c) in
      (fst res)::(sum t1 t2 (snd res))

let rec reverse
= fun l -> match l with
  [] -> []
| hd::tl -> (reverse tl)@[hd]

let rec process
= fun a b c -> match b with
  [] -> c
| hd::tl -> ( match hd with
    ZERO -> process a tl c
  | ONE -> process a tl (ZERO::(sum a c ZERO))
  )

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let sol = reverse (process (reverse b1) b2 []) in
  if sol = [] then [ZERO]
  else sol