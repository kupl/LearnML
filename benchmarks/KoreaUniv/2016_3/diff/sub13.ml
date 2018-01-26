(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec map f l =
match l with
| [] -> []
| hd::tl -> (f hd)::(map f tl)

let rec tidy_up e =
match e with
| Const n -> Const n
| Var s -> Power (s, 1)
| Power (s, n) -> Power (s, n)
| Sum lst -> Sum (map tidy_up lst)
| Times lst -> Times (map tidy_up lst)

let rec m_a_p f s l =
match l with 
| [] -> []
| hd::tl -> f hd s :: m_a_p f s tl 

let rec isvar s e = match e with 
| Var s0 -> if (s <> s0) then false else true
| Power (s0, n) -> if (s<>s0) then false else if (n<1) then false else true
| _ -> false

let rec isconst s e = match e with
| Const n -> true
| Power (s0, n) -> if (s0 <> s) then true else if (n <> 0) then false else true
| _ -> false

let rec diff e s = 
let e0 = tidy_up e in
match e0 with
| Const n -> Const 0
| Var s0 -> diff (tidy_up (Var s0)) s
| Power (s0, n) -> if (s0 <> s) then (Const 0) else if (n > 0) then Times [Const n; Power (s, n-1)] else Const 0
| Sum lst -> Sum (m_a_p diff s lst)
(**)
| Times lst -> 
 let l0 = (List.find_all (isvar s) lst) in
 let l1 = (List.find_all (isconst s) lst) in
 begin match l0 with
 | [] -> Times []
 | hd::tl -> Times ( ((diff hd s) :: tl) @ l1 )
 end

end
(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec getmeasure branc =
match branc with
| SimpleBranch (l,w) -> [w]
| CompoundBranch(l,m) ->
 begin match m with
 | (branch0, branch1) -> (getmeasure branch0) @ (getmeasure branch1)
 end

let sum l = List.fold_right (fun x y -> x+y) l 0

let rec torque branc =
match branc with
| SimpleBranch (l,w) -> l* sum(getmeasure branc)
| CompoundBranch (l,m) -> l* sum(getmeasure branc)

let rec balanced m = 
match m with
| (branch0, branch1) -> 
 begin match branch0 with
 | SimpleBranch (l0,w0) -> 
  begin match branch1 with
  | SimpleBranch (l1,w1) -> if (torque branch0) <> (torque branch1) then false else true
  | CompoundBranch (l1, m1) -> if (balanced m1) && (torque branch0) == (torque branch1) then true else false
  end
 |CompoundBranch (l0, m0) ->
  begin match branch1 with
  | SimpleBranch (l1,w1) -> if (balanced m0) && (torque branch0) == (torque branch1) then true else false
  | CompoundBranch (l1, m1) -> if (balanced m0) && (balanced m1) && (torque branch0) == (torque branch1) then true else false
  end
 end

end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec eval e n =
match e with
| INT n0 -> n0
| X -> n
| ADD (e0, e1) -> (eval e0 n) + (eval e1 n)
| SUB (e0, e1) -> (eval e0 n) - (eval e1 n)
| MUL (e0, e1) -> (eval e0 n) * (eval e1 n)
| DIV (e0, e1) -> (eval e0 n) / (eval e1 n)
| SIGMA (e0,e1,e2) -> 0

let calculator e =
match e with
| SIGMA (e0,e1,e2) -> 
 begin
 let asdfg = ref 0 in
 for i = (eval e0 0) to (eval e1 0) do 
 asdfg := !asdfg + (eval e2 i)
 done;
 !asdfg
 end
| _ -> eval e 0

end
(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let isinthelist l a = 
let temp = List.find_all ((fun x y -> if x <> y then false else true) a) l in
match temp with
| [] -> false
| hd::tl -> true

let isnotinthelist l a = 
let temp = List.find_all ((fun x y -> if x <> y then false else true) a) l in
match temp with
| [] -> true
| hd::tl -> false


let rec pvar e =
match e with
| P (s0, e0) -> s0::(pvar e0)
| C (e0, e1) -> (pvar e0) @ (pvar e1)
| V s0 -> []

let rec vvar e =
match e with
| P (s0, e0) -> vvar e0
| C (e0, e1) -> (vvar e0) @ (vvar e1)
| V s0 -> s0::[]

let rec check e =
let l0 = pvar e in
let l1 = vvar e in
match List.filter (isnotinthelist l0) l1 with
| [] -> true
| hd::tl -> false

end