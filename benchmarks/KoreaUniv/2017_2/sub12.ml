(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
          | Empty -> Empty
          | Node(n,t1,t2) -> Node(n,mirror t2, mirror t1)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec find_num : nat -> int
= fun n1 -> match n1 with
            |ZERO -> 0
            |SUCC(n2) -> 1 + find_num n2

let rec addnat : int -> nat
= fun n1 -> if n1=0 then ZERO else SUCC(addnat (n1-1))

let rec exp : int -> int -> int
= fun n1 n2 -> if n2=0 then 1 else n1 * (exp n1 (n2-1))
  
let natadd : nat -> nat -> nat 
= fun n1 n2 -> addnat ((find_num n1) + (find_num n2))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> addnat ((find_num n1) * (find_num n2))

let natexp : nat -> nat -> nat 
= fun n1 n2 -> addnat (exp (find_num n1) (find_num n2))


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

let rec find_all_string : formula -> string list -> string list
= fun f l-> match f with
           | True -> l
           | False -> l
           | Var s -> s::l
           | Neg f1 -> find_all_string f1 l
           | And (f1,f2) -> find_all_string f1 (find_all_string f2 l)
           | Or (f1,f2) -> find_all_string f1 (find_all_string f2 l)
           | Imply (f1,f2) ->find_all_string f1 (find_all_string f2 l)
           | Iff (f1, f2) -> find_all_string f1(find_all_string f2 l)

let rec true_in_string : formula->string->formula
= fun f s -> match f with
            | True -> True
            | False -> False
            | Var t -> if t=s then True else Var t
            | Neg f1 -> Neg (true_in_string f1 s)
            | And (f1, f2)-> And (true_in_string f1 s, true_in_string f2 s)
            | Or (f1, f2) -> Or (true_in_string f1 s, true_in_string f2 s)
            | Imply (f1, f2) -> Imply(true_in_string f1 s, true_in_string f2 s)
            | Iff (f1, f2) -> Iff(true_in_string f1 s, true_in_string f2 s)

let rec false_in_string : formula->string->formula
= fun f s -> match f with
            | True -> True
            | False -> False
            | Var t -> if t=s then False else Var t
            | Neg f1 -> Neg (false_in_string f1 s)
            | And (f1, f2) -> And (false_in_string f1 s, false_in_string f2 s)
            | Or(f1, f2) -> Or(false_in_string f1 s, false_in_string f2 s)
            | Imply(f1, f2) -> Imply(false_in_string f1 s, false_in_string f2 s)
            | Iff(f1, f2) -> Iff(false_in_string f1 s, false_in_string f2 s)

let rec sum_list : formula list -> formula list -> formula list
=fun fl1 fl2 -> match fl1 with
                |hd::tl -> hd::(sum_list tl fl2)
                |_ -> fl2

let rec kwang : formula->string list ->formula list
= fun f sl -> match sl with
                |hd::tl -> sum_list (kwang (true_in_string f hd) tl) (kwang (false_in_string f hd) tl)
                |_ -> [f]

let rec check_sat : formula -> int
= fun f -> match f with
           |True -> 1
           |False -> 0
           |Neg f1 -> if (check_sat f1 = 1) then 0 else 1
           |And(f1,f2) -> (check_sat f1)*(check_sat f2)
           |Or(f1,f2) -> if ((check_sat f1)+(check_sat f2)=0) then 0 else 1
           |Imply(f1,f2) -> if ((check_sat f1)=1 & (check_sat f2)=0) then 0 else 1
           |Iff(f1,f2) -> if((check_sat f1)=(check_sat f2)) then 1 else 0
let rec check_list_sat : formula list -> int
=fun fl -> match fl with
           | hd::tl -> (check_sat hd)+(check_list_sat tl)
           | _ -> 0

let rec sat : formula -> bool
= fun f -> if ( check_list_sat (kwang f  (find_all_string f []))=0) then false else true


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
               | Const n -> Const 0
               | Var y -> if y=x then Const 1 else Const 0
               | Power (y,i) -> if y=x then Times [Const i; Power(y,i-1)] else Const 0
               | Times [Const n; Const m] -> Const 0
               | Times [Const n; Var y] -> if y=x then Const n else Const 0
               | Times [Var y; Const n] -> if y=x then Const n else Const 0
               | Times [Const n; Power(y,i)] -> if y=x then Times[Const n; diff(Power(y,i),x)] else Const 0
               | Times [Power(y,i); Const n] -> if y=x then Times[Const n; diff(Power(y,i),x)] else Const 0
               | Times [Const n; Times l] -> Times [Const n; diff(Times l, x)]
               | Times [Times l; Const n] -> Times [Const n; diff(Times l, x)]
               | Times [Const n; Sum l] -> Times [Const n; diff(Sum l, x)]
               | Times [Sum l; Const n] -> Times [Const n; diff(Sum l, x)]
               | Sum (hd::tl) -> Sum [diff(hd,x); diff(Sum (tl),x)]
               | _ -> Const 0


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec xcalculator: int -> exp -> int
= fun n e -> match e with
           | X -> n
           | INT t -> t
           | ADD (X, X) -> n+n
           | ADD (X, INT t) -> n+t
           | ADD (INT t, X) -> n+t
           | ADD (INT t, INT u) -> t+u
           | ADD (e1, e2) -> (xcalculator n e1)+(xcalculator n e2)
           | SUB (X, X) -> 0
           | SUB (X, INT t) -> n-t
           | SUB (INT t, X) -> t-n
           | SUB (INT t, INT u) -> t-u
           | SUB (e1, e2) -> (xcalculator n e1)-(xcalculator n e2)
           | MUL (X, X) -> n*n
           | MUL (X, INT t) -> n*t
           | MUL (INT t, X) -> n*t
           | MUL (INT t, INT u) -> t*u
           | MUL (e1, e2) -> (xcalculator n e1)*(xcalculator n e2)
           | DIV (X, X) -> 1
           | DIV (X, INT t) -> n/t
           | DIV (INT t, X) -> t/n
           | DIV (INT t, INT u) -> t/u
           | DIV (e1, e2) -> (xcalculator n e1)/(xcalculator n e2)
           

let rec calculator : exp -> int
= fun e -> match e with
           | INT n -> n
           | ADD (e1,e2) -> (calculator e1)+(calculator e2)
           | SUB (e1,e2) -> (calculator e1)-(calculator e2)
           | MUL (e1,e2) -> (calculator e1)*(calculator e2)
           | DIV (e1,e2) -> (calculator e1)/(calculator e2)
           | SIGMA (INT e1,INT e2,e3) -> if e2<e1 then 0 else (xcalculator e1 e3) + calculator (SIGMA(INT (e1+1), INT e2, e3))
           | SIGMA (e1,e2,e3) -> calculator (SIGMA(INT (calculator e1), INT (calculator e2), e3))
           




(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sum_weight : mobile -> int
= fun m -> match m with
           | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
           | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> w1+sum_weight(m1)
           | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> w1+sum_weight(m1)
           | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> sum_weight(m1)+sum_weight(m2)

let find_part_balance : mobile ->int
= fun m -> match m with
           | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1=l2*w2 then 1 else 0
           | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> if l1*w1=l2*(sum_weight m1) then 1 else 0
           | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> if l2*w1=l1*(sum_weight m1) then 1 else 0
           | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if l1*(sum_weight m1)=l2*(sum_weight m2) then 1 else 0
           
let rec find_total_balance : mobile ->int
=fun m -> match m with
          | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> find_part_balance m
          | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> (find_part_balance m)*(find_total_balance m1)
          | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> (find_part_balance m)*(find_total_balance m1)
          | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (find_part_balance m)*(find_total_balance m1)*(find_total_balance m2)

let balanced : mobile -> bool
= fun m -> if (find_total_balance m)=1 then true else false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec binary_to_int : bin -> int -> int
= fun b n -> match b with
           | hd::tl -> if hd=ONE then binary_to_int tl (2*n+2) else if hd=ZERO then binary_to_int tl (2*n) else n/2
           | _ -> n/2

let rec int_to_binary : int -> bin
= fun n -> match n with
           | 0 -> []
           | _ -> if n mod 2=1 then ONE::(int_to_binary (n/2)) else if n mod 2=0 then ZERO::(int_to_binary (n/2)) else []
let rec reverse_binary : bin -> bin -> bin
= fun b1 b2 -> match b1 with
           | hd::tl -> reverse_binary tl (hd::b2)
           | _ -> b2
let bmul : bin -> bin -> bin
= fun b1 b2 -> if b1=[ZERO] then [ZERO] else if b2=[ZERO] then [ZERO]   else reverse_binary (int_to_binary((binary_to_int b1 0)*(binary_to_int b2 0))) []
