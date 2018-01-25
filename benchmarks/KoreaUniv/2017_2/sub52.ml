(*# exception Problem;;*)

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Empty -> Empty
  | Node(n, Empty, Empty) -> Node(n, Empty, Empty)
  | Node(n , btree1, btree2) -> Node(n, mirror btree2, mirror btree1)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC (remains) -> natadd remains (SUCC (n2))


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match (n1, n2) with
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | ((SUCC ZERO), n) -> n
  | (n, (SUCC ZERO)) -> n
  | ((SUCC (remains)), n) -> natadd n (natmul remains n)


let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
  match (n1, n2) with
  | (_, ZERO) -> (SUCC ZERO)
  | (ZERO, _) -> ZERO
  | ((SUCC ZERO), n) -> (SUCC ZERO)
  | (n, (SUCC ZERO)) -> n
  | (n, (SUCC (remains))) -> natmul n (natexp n remains)

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

type logical_env = (string * bool) list
let extend_logical_env (x, b) e = (x, b) :: e
let rec find_logical_env x e =
  match e with
  | [] -> raise (Failure "Runtime error!")
  | (y, b) :: tl -> if x = y then b else find_logical_env x tl

let rec extract_var : formula -> string list -> string list
= fun f v_list ->
  let rec append_dup l1 l2 =
    (match l1 with
     | [] -> l2
     | hd::tl -> if (List.mem hd l2) then append_dup tl l2
                 else append_dup tl (hd::l2))
  in
  match f with
  | True -> v_list
  | False -> v_list
  | Var (x) -> append_dup [x] v_list
  | Neg (f1) -> extract_var f1 v_list
  | And (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Or (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Imply (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)
  | Iff (f1, f2) -> append_dup (extract_var f1 v_list) (extract_var f2 v_list)

let rec sat_check_branch : string list -> (formula -> logical_env-> bool) -> formula -> logical_env -> bool
= fun vlist sat_fun f e->
  match vlist with
  | [] -> (sat_fun f e)
  | hd::tl -> (sat_check_branch tl sat_fun f (extend_logical_env (hd, true) e)) || (sat_check_branch tl sat_fun f (extend_logical_env (hd, false) e))


let rec sat_helper : formula -> logical_env -> bool
= fun f e ->
  match f with
  | True -> true
  | False -> false
  | Var (x) -> find_logical_env x e
  | Neg (f1) -> let b = sat_helper f1 e in (not b)
  | And (f1, f2) -> let b1 = sat_helper f1 e in
                    let b2 = sat_helper f2 e in
                    b1 && b2
  | Or (f1, f2) -> let b1 = sat_helper f1 e in
                   let b2 = sat_helper f2 e in
                   b1 || b2
  | Imply (f1, f2) -> sat_helper (Or (Neg (f1), f2)) e
  | Iff (f1, f2) -> sat_helper (And (Imply (f1, f2), Imply (f2, f1))) e

let sat : formula -> bool
= fun f ->
  let vlist = extract_var f [] in
  sat_check_branch vlist sat_helper f []


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
  | Const i -> Const 0
  | Var y  -> if y = x then (Const 1) else (Const 0)
  | Power (y, i) -> if y = x then Times [(Const i); (Power (y, (i-1)))]
                    else (Const 0)
  | Times l -> (match l with
                | [] -> (Const 0)
                | [single_e] -> (diff (single_e, x))
                | hd::tl -> Sum [Times ((diff (hd, x))::tl); Times ((hd)::[(diff ((Times tl), x))])]
               )
  | Sum l -> let derived_list = List.fold_left (fun acc e_prime -> acc @ [diff (e_prime, x)]) [] l in
             Sum derived_list


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_helper : exp -> int list -> int
= fun e l ->
  match e with
  | X -> (match l with
          | [] -> raise (Failure "Value error: Can't find x value in the env")
          | [n] -> n
          | _ -> raise (Failure "Runtime error: unexpected env"))
  | INT n -> n
  | ADD (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 + v2
  | SUB (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 - v2
  | MUL (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 * v2
  | DIV (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 / v2

  | SIGMA (e1, e2, exp) -> let n1 = calc_helper e1 l in
                           let n2 = calc_helper e2 l in
                           if n1 > n2 then 0
                           else (calc_helper exp [n1]) + (calc_helper (SIGMA ((ADD (e1, INT 1)), e2, exp)) l)

let calculator : exp -> int
= fun e ->
  calc_helper e []


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight : mobile -> int
= fun m ->
  match m with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let w_m2 = mobile_weight m2 in
                                                        w1 + w_m2
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let w_m1 = mobile_weight m1 in
                                                        w_m1 + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let w_m1 = mobile_weight m1 in
                                                          let w_m2 = mobile_weight m2 in
                                                          w_m1 + w_m2


let rec balance_helper : mobile -> bool
= fun m ->
  match m with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if (l1 * w1) = (l2 * w2) then true else false
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let m2_balance = balance_helper m2 in
                                                        let w_m2 = mobile_weight m2 in
                                                        if m2_balance = false then false
                                                        else balance_helper ((SimpleBranch (l1, w1), SimpleBranch (l2, w_m2)))
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let m1_balance = balance_helper m1 in
                                                        let w_m1 = mobile_weight m1 in
                                                        if m1_balance = false then false
                                                        else balance_helper ((SimpleBranch (l1, w_m1), SimpleBranch (l2, w2)))
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let m1_balance = balance_helper m1 in
                                                          let m2_balance = balance_helper m2 in
                                                          let w_m1 = mobile_weight m1 in
                                                          let w_m2 = mobile_weight m2 in
                                                          if (m1_balance && m2_balance) = true then balance_helper ((SimpleBranch (l1, w_m1), SimpleBranch (l2, w_m2)))
                                                          else false


let balanced : mobile -> bool
= fun m ->
  balance_helper m


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec badd : bin -> bin -> digit -> bin
= fun b1 b2 carry ->
  match (b1, b2, carry) with
  | ([], [], ZERO) -> [ZERO]
  | ([], [], ONE) -> [ONE]
  | (ZERO::tl, [], ZERO) -> ZERO :: (badd tl [] ZERO)
  | (ZERO::tl, [], ONE) -> ONE :: (badd tl [] ZERO)
  | (ONE::tl, [], ZERO) -> ONE :: (badd tl [] ZERO)
  | (ONE::tl, [], ONE) -> ZERO :: (badd tl [] ONE)
  | ([], ZERO::tl, ZERO) -> ZERO :: (badd [] tl ZERO)
  | ([], ZERO::tl, ONE) -> ONE :: (badd [] tl ZERO)
  | ([], ONE::tl, ZERO) -> ONE :: (badd [] tl ZERO)
  | ([], ONE::tl, ONE) -> ZERO :: (badd [] tl ONE)
  | (ZERO::tl1, ZERO::tl2, ZERO) ->  ZERO :: (badd tl1 tl2 ZERO)
  | (ZERO::tl1, ZERO::tl2, ONE) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ONE::tl1, ZERO::tl2, ZERO) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ONE::tl1, ZERO::tl2, ONE) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ZERO::tl1, ONE::tl2, ZERO) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ZERO::tl1, ONE::tl2, ONE) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ONE::tl1, ONE::tl2, ZERO) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ONE::tl1, ONE::tl2, ONE) ->  ONE :: (badd tl1 tl2 ONE)

let rec bmul_helper : bin -> bin -> bin -> bin
= fun b1 b2 offset ->
  match b1 with
  | [] -> [ZERO]
  | hd::tl -> let imt = (if hd = ONE then offset @ b2 else [ZERO]) in
              badd imt (bmul_helper tl b2 (ZERO::offset)) ZERO

let rec trim_zeros : bin -> bin
= fun b ->
  match b with
  | [] -> []
  | [ZERO] -> [ZERO]
  | [ONE] -> [ONE]
  | hd::tl -> if hd = ONE then b else (trim_zeros tl)

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rev_b1 = List.rev b1 in
  let rev_b2 = List.rev b2 in
  let rev_mult = bmul_helper rev_b1 rev_b2 [] in
  let mult_zeros = List.rev rev_mult in
  let mult = trim_zeros mult_zeros in
  mult
