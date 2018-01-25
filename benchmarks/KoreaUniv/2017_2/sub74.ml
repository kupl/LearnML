(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
        | Empty -> Empty
        | Node (i, a, b) -> let v1 = mirror a in
                            let v2 = mirror b in
                            Node(i, v2, v1)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> n1
               | SUCC n3 -> natadd (SUCC (n1)) n3

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> ZERO
               | SUCC n3 -> natadd (natmul n1 n3) n1

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> (SUCC ZERO)
               | SUCC n3 -> match n3 with
                            | ZERO -> n1
                            | SUCC n4 -> natmul (natexp n1 n3) n1


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

type env = (string * formula) list

let rec find_env x env = 
  match env with
  | [] -> Var x
  | (y,v)::t -> if x = y then v
        else find_env x t

let update_env (x,v) e = (x,v)::e

let rec sat_env : formula -> env -> bool
= fun f env -> (match f with
           | True -> true
           | False -> false
           | Var str -> (match (find_env str env) with
                        | True -> true
                        | False -> false
                        | _ -> raise (Failure "Error"))
           | Neg form1 -> (match form1 with
                          | _ -> not (sat_env form1 env))
           | And (form1, form2) -> (sat_env form1 env)&&(sat_env form2 env)
           | Or (form1, form2) -> (sat_env form1 env)||(sat_env form2 env)
           | Imply (form1, form2) -> (match (sat_env form1 env) with
                                      | false -> true
                                      | true -> (sat_env form2 env))
           | Iff (form1, form2) -> if ((sat_env (Imply (form1, form2)) env)&&(sat_env (Imply (form2, form1)) env)) then true else false
           )


let rec var_check : formula -> env -> env
= fun f env -> (match f with
                | True | False -> env
                | Var str -> (match (find_env str env) with
                              | Var x -> (update_env (str, True) env)
                              | _ -> env)
                | Neg form1 -> var_check form1 env
                | And (form1, form2) | Or (form1, form2) | Imply (form1, form2) | Iff (form1, form2) -> (var_check form2 (var_check form1 env))
)

let bool_flop : (string * formula) -> (string * formula)
= fun env -> (match env with
            | (x, y) -> if (y=True) then (x, False) else (x, True))

let rec var_set : formula -> env -> env -> bool
= fun f first_env setting_env -> (match first_env with
                                  | [] -> (sat_env f setting_env)
                                  | (hd::tl) -> ((var_set f tl (setting_env@[hd]))||(var_set f tl (setting_env@[(bool_flop hd)])))
                                  )

let sat : formula -> bool
= fun f -> var_set f (var_check f []) []

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff_check = fun lst count -> match lst with
                                      | [] -> count
                                      | (hd::tl) -> (match hd with
                                                    | Const num -> diff_check tl count
                                                    | _ -> diff_check tl (count+1))

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with 
               | Const num -> Const 0
               | Var str -> (if (str = x) then (Const 1) else Var str)
               | Power (str, num) -> (if (str = x) then (if num <= 1 then Const num else Times [Const num; Power (str, num-1)]) 
                                      else Power (str, num))
               | Times lst -> (match lst with
                               | [] -> (Const 0)
                               | (hd::tl) -> let cnt = (diff_check lst 0) in (
                                                if cnt = 0 then Const 0
                                                else (
                                                  (match hd with 
                                                  | Const num -> (Times ((Const num)::(diff ((Times (tl)), x))::[]))
                                                  | _ -> (match tl with
                                                          | [] -> (Times ((diff (hd, x))::[]))
                                                          | _ -> (Times ((diff (hd, x))::(diff ((Times tl), x))::[]))))
                                                )
                                             )
                               )
               | Sum lst -> (match lst with
                             | [] -> (Const 0)
                             | (hd::tl) -> (match hd with
                                            | _ -> (Sum ((diff (hd, x))::(diff ((Sum tl), x))::[]))))


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_env : exp -> int -> int
= fun e num -> (match e with
           | X -> num
           | INT num1 -> num1
           | ADD (exp1, exp2) -> ((calc_env exp1 num)+(calc_env exp2 num))
           | SUB (exp1, exp2) -> ((calc_env exp1 num)-(calc_env exp2 num))
           | MUL (exp1, exp2) -> ((calc_env exp1 num)*(calc_env exp2 num))
           | DIV (exp1, exp2) -> ((calc_env exp1 num)/(calc_env exp2 num))
           | SIGMA (exp1, exp2, exp3) -> let v1 = (calc_env exp1 num) in
                                         let v2 = (calc_env exp2 num) in
                                         if (v1=v2) then (calc_env exp3 v1)
                                         else ((calc_env exp3 v1)+(calc_env (SIGMA ((INT (v1+1)), exp2, exp3)) v1))
            )

let calculator : exp -> int
= fun e -> (calc_env e 0)


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let left : mobile -> branch
= fun m -> match m with (x,_) -> x;;

let rigth : mobile -> branch
= fun m -> match m with (_,x) -> x;;


let rec calc_weight : branch -> int
= fun b -> (match b with
           | CompoundBranch (length, mobile) -> ((calc_weight (left mobile))+(calc_weight (rigth mobile)))
           | SimpleBranch (length, weight) -> weight
           )


let calc_torgue : branch -> int
= fun b -> match b with
           | SimpleBranch (length, weight) -> (length*weight)
           | CompoundBranch (length, mobile) -> (length*((calc_weight (left mobile))+(calc_weight (rigth mobile))))


let rec balanced : mobile -> bool
= fun m -> (match m with
           | (x,y) -> let v1 = (calc_torgue x) in 
                      let v2 = (calc_torgue y) in
                      (if (v1!=v2) then false else 
                        (match x with 
                         | CompoundBranch (x_length, x_mobile) -> ((balanced x_mobile)&&(match y with 
                                 | CompoundBranch (y_length, y_mobile) -> (balanced y_mobile)
                                 | _ -> true))
                         | _ -> (match y with 
                                 | CompoundBranch (y_length, y_mobile) -> (balanced y_mobile)
                                 | _ -> true)
                        )
                      )
                      
           )

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec reverse l = 
  match l with
  | [] -> []
  | hd::tl -> ((reverse tl)@[hd])


let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl


let rec append l cnt = if (cnt>0) then (append (ZERO::l) (cnt-1)) else l

let rec zero_check = fun b -> match b with
                              | [] -> false
                              | (hd::tl) -> if hd=ONE then true else (zero_check tl)

let bitsum : digit -> digit -> digit -> digit
= fun b1 b2 carry -> if (b1=ZERO&&b2=ZERO&&carry=ONE) then ONE else
                     if (b1=ZERO&&b2=ONE&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ZERO&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ONE) then ONE else ZERO


let bitcarry : digit -> digit -> digit -> digit
= fun b1 b2 carry -> if (b1=ZERO&&b2=ONE&&carry=ONE) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ZERO&&carry=ONE) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ONE) then ONE else ZERO


let rec bsum : bin -> bin -> digit -> bin
= fun b1 b2 carry -> (match b1 with
                      | [] -> (match carry with
                               | ZERO -> []
                               | ONE -> [ONE]
                               )
                      | (hd1::tl1) -> (match b2 with
                                       | [] -> b1
                                       | (hd2::tl2) -> (bitsum hd1 hd2 carry)::(bsum tl1 tl2 (bitcarry hd1 hd2 carry))
                                      )
                     )

let calc : bin -> bin -> bin
= fun b1 b2 -> let v1 = reverse (append b1 ((length b2)-(length b1))) in
               let v2 = reverse (append b2 ((length b1)-(length b2))) in
               (reverse (bsum v1 v2 ZERO))

let rec calc_mul = fun b1 b2 -> (match b2 with
                                | [] -> []
                                | (hd::tl) -> if hd=ZERO then (calc_mul (b1@[ZERO]) tl) else (calc b1 (calc_mul (b1@[ZERO]) tl))
                                )

let bmul : bin -> bin -> bin
= fun b1 b2 -> if ((zero_check b1)&&(zero_check b2)) then (calc_mul b1 (reverse b2)) else [ZERO]

