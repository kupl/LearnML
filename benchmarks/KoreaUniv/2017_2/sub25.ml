(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
            | Empty -> t
            | Node(int,x,y) -> Node(int,(mirror y),(mirror x))

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec how_many : nat -> int
= fun n -> match n with
            | ZERO -> 0
            | SUCC(x) -> 1 + how_many(x)

let rec make_answer : int -> nat
= fun n -> if n = 0 then ZERO
           else SUCC(make_answer (n-1))

let natadd : nat -> nat -> nat
= fun n1 n2 -> make_answer ((how_many n1) + (how_many n2))

let natmul : nat -> nat -> nat
= fun n1 n2 -> make_answer ((how_many n1) * (how_many n2))

let natexp : nat -> nat -> nat
= fun n1 n2 -> make_answer (int_of_float (float_of_int(how_many n1) ** float_of_int(how_many n2)))

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

type env2 = (string * bool) list
let empty_env2 = []
let empty_env3 = []
let extend_env2 (x,v) e = (x,v)::e
let rec apply_env2 x env = match env with
                      | [] -> apply_env2 x (extend_env2 (x,true) env)
                      | (y,v)::tl -> if x = y then v 
                                     else apply_env2 x tl

let rec help_sat : formula -> env2 -> bool
= fun f env -> match f with
                | True -> true
                | False -> false
                | Var(x) -> apply_env2 x env
                | Neg(x) -> if (help_sat x env) = true then false
                            else true
                | And(x,y) -> (help_sat x env) && (help_sat y env)
                | Or(x,y) -> (help_sat x env) || (help_sat y env)
                | Imply(x,y) -> if (help_sat x env) = false then true
                                else if (help_sat y env) = true then true
                                else false 
                | Iff(x,y) -> if (help_sat x env) = true && (help_sat y env) = true then true
                              else if (help_sat x env) = false && (help_sat y env) = false 
                                then true
                              else false

let rec sat : formula -> bool
= fun f -> let rec help_sat2 f env = 
             match f with
             | True -> true
             | False -> false
             | Var(x) -> false
             | Neg(x) -> if (help_sat2 x env) = true then false
                         else true
             | And(x,y) -> (help_sat2 x env) && (help_sat2 y env)
             | Or(x,y) -> (help_sat2 x env) || (help_sat2 y env)
             | Imply(x,y) -> if (help_sat2 x env) = false then true
                             else if (help_sat2 y env) = true then true
                             else false
             | Iff(x,y) -> if (help_sat2 x env) = true && (help_sat2 y env) = true then true
                           else if (help_sat2 x env) = false && (help_sat2 y env) = false then true
                           else false
            in (help_sat f empty_env2) || (help_sat2 f empty_env3)

(* problem 4*)
type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
                | Const(a) -> Const(0)
                | Var(a) -> if a = x then Const(1) else Var(a)
                | Power(a,b) -> if a = x && b = 1 then Const(1)
                                else if a = x then Times[(Const(b));(Power(a, b-1))]
                                else Power(a,b)
                | Times(a) -> (match a with
                                | [n1;n2] -> if n1 = Const(0) || n2 = Const(0) then Const(0)
                                             else if n1 = Var(x) then n2
                                             else if n2 = Var(x) then n1
                                             else Times[n1;diff(n2,x)])
                                                    
                | Sum(a) -> let rec help_diff e1 
                             = (match e1 with
                                 | [] -> []
                                 | hd::tl -> (diff(hd,x))::(help_diff tl))
                             in Sum(help_diff a)

(* problem 5*)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp

type env = exp * int list
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env x e =
  match e with
  | [] -> 0
  | (y,v)::tl -> if x = y then v else apply_env x tl
        
let rec calculator : exp -> int
= fun e -> let rec help_cal e env 
            = match e with
             | X -> apply_env X env
             | INT(x) -> x
             | ADD(x,y) -> (help_cal x env) + (help_cal y env)
             | SUB(x,y) -> (help_cal x env) - (help_cal y env)
             | MUL(x,y) -> (help_cal x env) * (help_cal y env)
             | DIV(x,y) -> (help_cal x env) / (help_cal y env)
             | SIGMA(x,y,z) -> match (help_cal x env), (help_cal y env) with 
                                 | n1, n2 -> 
                                    if n1 = n2 then (let v1 = extend_env (X,n1) env
                                                     in help_cal z v1)
                                    else ((let v2 = extend_env (X,n1) env
                                          in help_cal z v2) + (help_cal (SIGMA(INT(n1+1),y,z)) env))
            in help_cal e empty_env


(* problem 6*)
type mobile = branch * branch (*left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : branch -> int
= fun m -> match m with 
            | SimpleBranch(x,y) -> y
            | CompoundBranch(x,y) -> match y with
                                      | (n1, n2) -> (weight n1) + (weight n2)               

let help_bal : branch -> int
= fun m -> match m with 
            | CompoundBranch(x,y) -> (match y with
                                      | (n1, n2) -> x * ((weight n1) + (weight n2)))
            | SimpleBranch(x,y) -> x * y

let rec balanced : mobile -> bool
= fun m -> (match m with
            | (x) -> (match x with
                      | (y,z) -> (match y,z with 
                                  | CompoundBranch(n1,n2), CompoundBranch(n3,n4) ->
                                     if ((help_bal y = help_bal z) && balanced n2 && balanced n4)
                                         then true
                                     else false
                                  | CompoundBranch(a1,a2), SimpleBranch(a3,a4) ->
                                     if ((help_bal y = help_bal z) && balanced a2) then true
                                     else false
                                  | SimpleBranch(a5,a6), CompoundBranch(a7,a8) ->
                                     if ((help_bal y = help_bal z) && balanced a8) then true
                                     else false
                                  | SimpleBranch(n5,n6), SimpleBranch(n7,n8) ->
                                     if (help_bal y = help_bal z) then true
                                     else false)))

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec reverse : bin -> bin
= fun b1 -> match b1 with
            | [] -> []
            | hd::tl -> (reverse tl)@[hd]

let rec bin_dec : bin -> int -> int
= fun b1 n -> match b1 with
              | [] -> 0
              | hd::tl -> if hd = ONE then (n + (bin_dec tl (n*2)))
                          else (bin_dec tl n*2)

let rec dec_bin : int -> bin -> bin
= fun n b2-> if n / 2 = 0 then (ONE::b2)
             else if n mod 2 = 0 then (dec_bin (n/2) (ZERO::b2))
             else dec_bin (n/2) (ONE::b2)

let bmul : bin -> bin -> bin
= fun b1 b2 -> let empty_bin = [] 
                in dec_bin ((bin_dec (reverse b1) 1) * (bin_dec (reverse b2) 1)) empty_bin
