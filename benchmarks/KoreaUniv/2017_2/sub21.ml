(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
    match t with
    | Empty -> Empty
    | Node(a, b, c) -> Node(a, (mirror c), (mirror b));;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
    match n1 with
    | ZERO -> n2
    | SUCC a -> natadd a (SUCC n2);;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
    let rec sub_natmul: nat -> nat -> nat -> nat
    = fun n1 n2 n ->
        match n1 with
        | ZERO -> ZERO
        | SUCC ZERO -> n2
        | SUCC a -> sub_natmul a (natadd n2 n) n
    in sub_natmul n1 n2 n2;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
    let rec sub_natexp: nat -> nat -> nat -> nat
    = fun n n1 n2 ->
        match n2 with
        | ZERO -> if n1 = ZERO then raise(Failure "Error") else SUCC ZERO
        | SUCC ZERO -> n1
        | SUCC a -> sub_natexp n (natmul n n1) a
    in sub_natexp n1 n1 n2;;


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
= fun f -> 
    match f with
    | True -> true
    | False -> false
    | Var str -> true
    | Neg form ->
            (match (sat form) with
            | true -> false
            | false -> true)
    | And (form1, form2) -> (sat form1) && (sat form2)
    | Or (form1, form2) -> (sat form1) || (sat form2)
    | Imply (form1, form2) -> 
            (match (sat form1) with
            | true -> 
                    (match (sat form2) with
                    | true -> true
                    | false -> false)
            | false -> true)
    | Iff (form1, form2) -> if (sat form1) = (sat form2) then true else false;;


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
    let rec diff_list: aexp * string -> aexp list
    = fun (e, x) -> 
        match e with
        | Sum [Const a] -> [Const 0]
        | Sum (Var x :: tl) -> [Const 1]
        | Sum ((Times [Const a; Var x]) :: tl) -> [Const a]
        | Sum ((Power (x, 1)) :: tl) -> [Const 1]
        | Sum ((Times [Const a; Power (x, 1)]) :: tl) -> [Const a]
        | Sum (Power (x, 2) :: tl) ->
                (Times [Const 2; Var x]) :: (diff_list (Sum tl, x))
        | Sum ((Times [Const a; Power (x, 2)]) :: tl) -> 
                (Times [Const (a*2); Var x]) :: (diff_list (Sum tl, x))
        | Sum (Power (x, n) :: tl) ->
                (Times [Const n; Power (x, (n-1))]) :: (diff_list (Sum tl, x))
        | Sum ((Times [Const a; Power (x, n)]) :: tl) ->
                (Times [Const (a*n); Power (x, (n-1))]) :: (diff_list (Sum tl, x))
        | _ -> raise(Failure "Error")
    in Sum (diff_list(e, x));;


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> 
    let rec sub_cal: exp -> int -> int -> int
    = fun e n sum ->
        match e with
        | X -> n
        | INT x -> x
        | ADD (e1, e2) -> (sub_cal e1 n sum) + (sub_cal e2 n sum)
        | SUB (e1, e2) -> (sub_cal e1 n sum) - (sub_cal e2 n sum)
        | MUL (e1, e2) -> (sub_cal e1 n sum) * (sub_cal e2 n sum)
        | DIV (e1, e2) -> (sub_cal e1 n sum) / (sub_cal e2 n sum)
        | SIGMA (e1, e2, e3) -> 
                if e1 <= e2 then sub_cal 
                 (SIGMA (INT (sub_cal (ADD (e1, INT 1)) n sum), e2, e3)) 
                 (n+1) (sum+(sub_cal e3 n sum))
                else sum 
    in match e with 
    | SIGMA (e1, e2, e3) -> (sub_cal e (sub_cal e1 1 0) 0)
    | _ -> (sub_cal e 1 0);;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->     
    let rec w_bal: branch -> int -> int
    = fun b weight ->
        match b with
        | SimpleBranch (l1, w1) -> weight+w1
        | CompoundBranch (l1, (b1, b2)) -> (w_bal b1 weight) + (w_bal b2 weight)
    in let l_bal: branch -> int
       = fun b ->
           match b with
           | SimpleBranch (l1, w1) -> l1
           | CompoundBranch (l1, m1) -> l1   
        in let rec sub_bal: mobile -> bool
        = fun sub_mob ->
            match sub_mob with
            | (sub_b1, sub_b2) ->
                    (match (sub_b1, sub_b2) with
                    | ((SimpleBranch (sub_l1, sub_w1)), (SimpleBranch (sub_l2, sub_w2))) -> 
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then true
                            else false
                    | ((SimpleBranch (sub_l1, sub_w1)), (CompoundBranch (sub_l2, sub_m1))) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1)
                            else false
                    | ((CompoundBranch (sub_l1, sub_m1)), SimpleBranch (sub_l2, sub_w1)) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1)
                            else false
                    | (CompoundBranch (sub_l1, sub_m1), CompoundBranch (sub_l2, sub_m2)) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1) && (sub_bal sub_m2)
                            else false)
            in sub_bal m;;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
    let rec sub_bmul: bin -> bin -> bin -> bin
    = fun sb1 sb2 sum ->
        match sb1 with
        | [] -> sum
        | ZERO :: tl -> sub_bmul tl sb2 (sum @ [ZERO]) 
        | ONE :: tl -> 
                let rec badd: bin -> bin -> bin
                = fun bb1 bb2 ->
                    let rec brev: bin -> bin
                    = fun b ->
                        match b with
                        | [] -> []
                        | hd :: tl -> (brev tl) @ [hd]
                    in if bb1 = [] then bb2 
                       else if bb2 = [] then bb1
                            else match (brev bb1, brev bb2) with
                                 | (ZERO :: t1, ZERO :: t2) -> ((badd (brev t1) (brev t2)) @ [ZERO])
                                 | (ZERO :: t1, ONE :: t2) -> ((badd (brev t1) (brev t2)) @ [ONE])
                                 | (ONE :: t1, ZERO :: t2) -> ((badd (brev t1) (brev t2)) @ [ONE])
                                 | (ONE :: t1, ONE :: t2) -> ((badd (badd (brev t1) (brev t2)) [ONE]) @ [ZERO])
                                 | _ -> raise(Failure "Error")
                in (sub_bmul tl sb2 (badd (sum @ [ZERO]) sb2))
    in if b2 = [ZERO] then [ZERO]
       else match b1 with
            | [] -> []
            | [ZERO] -> [ZERO]
            | [ONE] -> b2
            | hd :: tl -> sub_bmul tl b2 b2;;
