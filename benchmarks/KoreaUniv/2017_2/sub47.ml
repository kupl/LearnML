(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
        match t with
        | Empty -> Empty
        | Node(b, l, r) -> Node(b, mirror r, mirror l)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
        match n2 with
        | ZERO -> n1
        | SUCC s -> natadd (SUCC n1) s

let rec natmul_rec n1 n2 r =
        match n2 with
        | ZERO -> r
        | SUCC s -> natmul_rec n1 s (natadd n1 r)
let natmul : nat -> nat -> nat 
= fun n1 n2 -> natmul_rec n1 n2 ZERO

let rec natexp_rec n1 n2 r =
        match n2 with
        | ZERO -> r
        | SUCC s -> natexp_rec n1 s (natmul n1 r)
let natexp : nat -> nat -> nat 
= fun n1 n2 -> natexp_rec n1 n2 (SUCC ZERO)


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

type sat_env = (string * bool) list

let rec sat_append_var x l =
        match l with
        | [] -> [x]
        | hd::tl -> if hd = x then l else hd::(sat_append_var x tl)

let rec sat_find_var f l =
        match f with
        | True -> l
        | False -> l
        | Var x -> sat_append_var x l
        | Neg (e) -> sat_find_var e l
        | And (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Or (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Imply (e1, e2) ->
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1
        | Iff (e1, e2) -> 
                let l1 = sat_find_var e1 l
                in sat_find_var e2 l1

let rec sat_apply_env x env =
        match env with
        | [] -> raise (Failure "Error")
        | (y, v)::tl -> if x = y then v else sat_apply_env x tl

let rec sat_make_env_all_f : string list -> sat_env -> sat_env
= fun var r ->
        match var with
        | [] -> r
        | hd::tl -> sat_make_env_all_f tl ((hd, false)::r)

let rec sat_next_case env c =
        match c with
        | true ->
                (match env with
                | [] -> raise (Failure "No Next Case") 
                | (x, true)::tl -> (x, false)::(sat_next_case tl true)
                | (x, false)::tl -> (x, true)::tl)
        | false -> env

let rec sat_has_next_case env =
        match env with
        | [] -> false
        | (_, b)::tl -> if b then sat_has_next_case tl else true

let rec sat_eval f env =
        match f with
        | True -> true
        | False -> false
        | Var x -> sat_apply_env x env
        | Neg (e) ->
                (match sat_eval e env with
                | true -> false
                | false -> true)
        | And (e1, e2) ->
                (match sat_eval e1 env with
                | true -> sat_eval e2 env
                | false -> false)
        | Or (e1, e2) ->
                (match sat_eval e1 env with
                | true -> true
                | false -> sat_eval e2 env)
        | Imply (e1, e2) -> sat_eval (Or ((Neg e1), e2)) env
        | Iff (e1, e2) -> sat_eval (Or (And (e1, e2), Neg(Or (e1, e2)))) env

let rec sat_rec f env =
        if sat_eval f env then true
        else if sat_has_next_case env then sat_rec f (sat_next_case env true)
        else false
let sat : formula -> bool
= fun f -> sat_rec f (sat_make_env_all_f (sat_find_var f []) [])


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
        | Const _ -> Const 0
        | Var y -> if x = y then Const 1 else Const 0
        | Power (y, n) -> if x = y then Times [Const n; Power(y, n-1)] else Const 0
        | Times l ->
                (match l with
                | [] -> Const 0
                | hd::tl -> Sum [Times ((diff (hd, x))::tl); Times [hd; (diff (Times tl, x))]])
        | Sum l ->
                (match l with
                | [] -> Const 0
                | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)])


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_eval e n =
        match e with
        | X -> n
        | INT a -> a
        | ADD (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 + v2
        | SUB (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 - v2
        | MUL (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 * v2
        | DIV (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 / v2
        | SIGMA (e1, e2, e3) ->
                let f = calc_eval e1 n in
                let t = calc_eval e2 n in
                if f > t then 0 else (calc_eval e3 f) + (calc_eval (SIGMA (INT (f+1), INT (t), e3)) n)
let rec calculator : exp -> int
= fun e ->
        match e with
        | X -> raise (Failure "Incomputable Formula")
        | INT n -> n
        | ADD (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 + v2
        | SUB (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 - v2
        | MUL (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 * v2
        | DIV (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e1 in
                v1 / v2
        | SIGMA (e1, e2, e3) ->
                let f = calculator e1 in
                let t = calculator e2 in
                if f > t then 0 else (calc_eval e3 f) + calculator (SIGMA (INT (f+1), INT (t), e3))


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight m =
        match m with
        | (SimpleBranch (_, lw), SimpleBranch (_, rw)) -> lw + rw
        | (SimpleBranch (_, lw), CompoundBranch (_, rm)) -> lw + (mobile_weight rm)
        | (CompoundBranch (_, lm), SimpleBranch (_, rw)) -> (mobile_weight lm) + rw
        | (CompoundBranch (_, lm), CompoundBranch (_, rm)) -> (mobile_weight lm) + (mobile_weight rm)

let rec balanced : mobile -> bool
= fun m ->
        match m with
        | (SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
                if ll * lw = rl * rw then true else false
        | (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
                if balanced rm then
                        if ll * lw = rl * (mobile_weight rm) then true else false
                else false
        | (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
                if balanced lm then
                        if ll * (mobile_weight lm) = rl * rw then true else false
                else false
        | (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
                match (balanced lm, balanced rm) with
                | (true, true) -> if ll * (mobile_weight lm) = rl * (mobile_weight rm) then true else false
                | _ -> false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec b_reverse : bin -> bin
= fun b ->
        match b with
        | [] -> []
        | hd::tl -> (b_reverse tl)@[hd]

let badd b1 b2 =
        let b1_r = b_reverse b1 in
        let b2_r = b_reverse b2 in
        let rec badd_r br1 br2 c =
                match c with
                | ZERO ->
                       (match br1 with
                        | [] -> br2
                        | hd1::tl1 ->
                                match br2 with
                                | [] -> br1
                                | hd2::tl2 ->
                                        match (hd1, hd2) with
                                        | (ONE, ONE) -> ZERO::badd_r tl1 tl2 ONE
                                        | (ONE, ZERO) -> ONE::badd_r tl1 tl2 ZERO
                                        | (ZERO, ONE) -> ONE::badd_r tl1 tl2 ZERO
                                        | (ZERO, ZERO) -> ZERO::badd_r tl1 tl2 ZERO)
                | ONE -> badd_r (badd_r br1 [ONE] ZERO) br2 ZERO
        in
        let result_r = badd_r b1_r b2_r ZERO in
        b_reverse result_r

let rec remove_front_zero b =
        match b with
        | [] -> [ZERO]
        | ZERO::tl -> remove_front_zero tl
        | ONE::tl -> b

let rec bmul_rec b1 b2 r =
        match b2 with
        | [] -> r
        | ZERO::tl -> bmul_rec b1 tl (r@[ZERO])
        | ONE::tl -> bmul_rec b1 tl (badd (r@[ZERO]) b1)
let bmul : bin -> bin -> bin
= fun b1 b2 -> 
        let result = bmul_rec b1 b2 [ZERO]
        in remove_front_zero result
