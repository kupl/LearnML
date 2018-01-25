(*problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t ->
let rec mirro t =
match t with
| Empty -> Empty
| Node (x, y, z) -> Node (x, mirro z, mirro y) in mirro t;;
(*problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 ->
let rec naadd n1 n2 = 
match n1 with
| ZERO -> n2 | SUCC n1 -> SUCC (naadd n1 n2) in naadd n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 ->
let rec namul n1 n2 =
match n2 with
| ZERO -> ZERO
| SUCC a -> natadd n1 (namul n1 a) in namul n1 n2;;

let natexp : nat -> nat -> nat
= fun n1 n2 ->
let rec naexp n1 n2 =
match n2 with 
| ZERO -> (SUCC ZERO)
| SUCC a -> natmul n1 (naexp n1 a) in naexp n1 n2;;
(*problem 3*)
(*problem 4*)

(*problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
let rec cal e num = 
match e with
| X -> num
| INT a -> a
| ADD (x, y) -> (cal x num) + (cal y num)
| SUB (x, y) -> (cal x num) - (cal y num)  
| MUL (fst, snd) -> (cal fst num) * (cal snd num)
| DIV (fst, snd) -> (cal fst num) / (cal snd num)
| SIGMA(s, t, c) -> 
let k = (cal s num) 
in let m = (cal t num)
in let rec sum k =
if k > m then 0
else (cal c k) + sum(k + 1) in sum k in cal e 0;;
(*problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
let rec w t =
match t with
| ((SimpleBranch(a,b)), (SimpleBranch(c,d))) ->  b + d
| ((CompoundBranch(c,mo)), (SimpleBranch(a,b))) -> (w mo) + b
| ((SimpleBranch(a,b)), (CompoundBranch(c,mo))) -> b + (w mo)
| ((CompoundBranch(len,mo)), (CompoundBranch(len2,mo2))) -> (w mo)+(w mo2)  in
let rec bal m =
match m with
| ((SimpleBranch(a,b)), (SimpleBranch(c,d))) -> (a*b) = (c*d)
| ((CompoundBranch(c,sum)), (SimpleBranch(a,b))) -> (bal sum) && (c * (w sum)) = (a*b)
| ((SimpleBranch(a,b)), (CompoundBranch(c,sum))) -> (bal sum) && (a*b) = (c * (w sum))
| ((CompoundBranch(c,sum)), (CompoundBranch(d,sum2))) -> (bal sum) && (bal sum2) && (c * (w sum)) = (d * (w sum)) in bal m;;
(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
let rec d2b b =
if b = 0 then [] 
else  
if (b mod 2 = 0)
then ZERO :: d2b(b/2)
else ONE :: d2b(b/2)
in let rec b2i b
= match b with
| [] -> 0
| hd::tl -> 
match hd with
| ONE -> (1 lsl List.length tl) + b2i tl 
| ZERO -> b2i tl in List.rev (d2b ((b2i b1) * (b2i b2)));;
