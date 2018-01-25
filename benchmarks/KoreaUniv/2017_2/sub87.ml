exception NOANSWER
(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with 
|Empty -> Empty
|Node(int, Empty, Empty) -> Node(int, Empty, Empty)
|Node(int, left, right) -> Node(int, mirror right, mirror left)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
|ZERO -> n2
|SUCC nat -> SUCC(natadd nat n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
|ZERO -> ZERO
|SUCC ZERO -> n2
|SUCC nat -> (natadd n2 (natmul nat n2))

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
|ZERO -> SUCC ZERO
|SUCC ZERO -> n1
|SUCC nat2 -> (natmul n1 (natexp n1 nat2))
 



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


let rec tf boollist sen
= match sen with
|True -> true
|False -> false
|Var x-> List.assoc x boollist
|Neg x -> not(tf boollist x)
|And (a1, a2) -> (tf boollist a1) && (tf boollist a2)
|Or (a1, a2) -> (tf boollist a1) || (tf boollist a2)
|Imply (a1, a2)-> (not(tf boollist a1)) || (tf boollist a2)
|Iff (a1, a2) -> ((not(tf boollist a1)) || (tf boollist a2)) && ((not (tf boollist a2)) || (tf boollist a1))

let rec getlist f l 
= match f with
|True-> l
|False -> l 
|Var x-> (match l with
        |[]->[x]
        |hd::tl->x::(hd::tl))
|Neg x-> getlist x l 
|And (a1, a2)-> getlist a1 l  @ getlist a2 l 
|Or (a1, a2) -> getlist a1 l @ getlist a2 l 
|Imply(a1, a2) -> getlist a1 l  @ getlist a2 l 
|Iff(a1, a2) -> getlist a1 l  @ getlist a2 l 


let rec findsame l x =
match x with
|[]->l
|hd::tl-> if List.mem hd l then findsame l tl else findsame (hd :: l) tl

let rec gettruth boollist f =
match boollist with
|[]->[]
|hd::tl-> tf hd f :: gettruth tl f

let rec find boollist f =
match boollist with
|[]-> false
|hd::tl-> if hd == true then true else find tl f


let rec makelist boollist x =
match x with
|[]-> [boollist]
|v::tl-> makelist ((v, true) :: boollist) tl @ makelist((v,false)::boollist) tl


let sat : formula -> bool
= fun f->
  let flist = getlist f [] in
    let slist = findsame [] flist in
      let mlist = makelist [] slist in
        let k =  gettruth mlist f in
          find k f




(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


let rec diff : aexp * string -> aexp

= fun (e,x) ->(*x is parameter*) 
match e with
|Const c -> Const 0
|Var f -> if f = x then (Const 1) else (Const 0)
|Power (a,b) -> if a = x then Times[Const b; Power(a,(b-1))] else (Const 0)
|Times l -> (match l with
            |[]-> raise (NOANSWER)
            |[a]-> Times [diff(a,x)]
            |h::t -> Sum [Times (diff(h,x)::t);Times (h::[diff(Times t,x)])])
                                            
|Sum l -> (match l with
              |[]-> Const 0
              |h::t -> Sum(diff(Sum t, x)::[diff(h,x)]))


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec another e l =
match e with
|X-> (match l with
      |[n]->n
      |_-> raise (NOANSWER))
|INT n -> n
|ADD(x,y) -> another x l + another y l
|SUB(x,y) -> another x l - another y l
|MUL(x,y) -> another x l * another y l
|DIV(x,y) -> another x l / another y l

|SIGMA(x,y,exp) -> let v1 = another x l in
                    let v2 = another y l in
                      let rec keep v1 v2 exp l =
                        if v1 == v2 then another exp [v2]
                        else
                        another exp [v1] + keep((v1)+1) v2 exp l
                        in keep v1 v2 exp l

let calculator :exp ->int
=fun e->
another e []


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int



let rec sum d =
match d with
|(CompoundBranch(a,b),CompoundBranch(x,y))->(sum b) + (sum y)
|(SimpleBranch(a,b),SimpleBranch(x,y))-> b + y
|(SimpleBranch(a,b),CompoundBranch(x,y))-> b + (sum y)
|(CompoundBranch(a,b),SimpleBranch(x,y))-> (sum b) + y

let rec balance tree
=match tree with
|(left,right)-> match (left, right) with 
                |(SimpleBranch (a,b), SimpleBranch(c,d)) -> a*b == c*d
                |(CompoundBranch(a,b), CompoundBranch(c,d))-> balance(SimpleBranch(a,(sum b)), SimpleBranch(c,sum d))
                |(SimpleBranch(a,b), CompoundBranch(c,d))->balance(SimpleBranch(a,b), SimpleBranch(c,sum d))
                |(CompoundBranch(a,b),SimpleBranch(c,d))-> balance(SimpleBranch(a ,(sum b)),SimpleBranch(c,d))



let balanced : mobile -> bool
= fun m -> balance m

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list


let rec exponent b =
if (b == 0) then 1
else 2 * exponent (b-1)

let rec decimal b1 = 
match b1 with
|[]->0
|h::t-> if h == ONE then (exponent (List.length t)) + decimal t else 0 + decimal t

let rec element x =
match x with
|1-> ONE 
|0-> ZERO
|_->raise (NOANSWER)

let rec binary n l =
match n with
|0->l
|_-> binary (n/2) ((element(n mod 2))::l)

let sum b1 b2 = binary (decimal b1 * decimal b2) []


let bmul: bin -> bin -> bin
= fun b1 b2 -> sum b1 b2

