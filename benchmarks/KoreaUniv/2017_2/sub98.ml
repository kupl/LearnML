(*problem 1 *)
type btree = Empty | Node of int *btree *btree
  let rec mirror  
    = fun t
      -> match t with
        |Empty -> Empty
          |Node(x,lt,rt)->Node(x,mirror rt, mirror lt);;

(*problem 2*)
type nat = ZERO | SUCC of nat
let two = SUCC (SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

    let rec natadd
    =fun n1 n2 -> match n2 with
    |ZERO -> let rec subnat n = match n with
    |ZERO -> ZERO
    |SUCC x ->
    SUCC(subnat x)
  in
  subnat n1
    |SUCC y -> SUCC (natadd n1 y);;

    let rec natmul
    = fun n1 n2 -> match n2 with
    |ZERO -> ZERO
    |SUCC n2 -> natadd n1 (natmul n1 n2);;


(*problem 3*)

type formula =
True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let rec sat
  = fun f-> match f with
    |True->true
      |False->false
  |Var x -> true
  |Neg(x) -> false
     |And (x,y)-> (sat x) &&(sat y)
    |Or (x,y) -> (sat x) ||(sat  y)
    |Imply(x,y) -> not(sat x) ||(sat y)
    |Iff(x,y) -> (sat x) = (sat y)



(*problem 4*)


  type aexp = 

  | Const of int

  | Var of string

  | Power of string * int

  | Times of aexp list

  | Sum of aexp list

 

  let rec diff : aexp * string -> aexp

  = fun (e, x) -> 

   match e with

   | Const a -> Const 0;

   | Var v -> if x = v then Const 1 else Const 0

   | Power (b, n) -> if x = b then Times [Const n; Power(b, (n-1))]
                                    else Const 0

   | Times l -> (match l with 
                  | [] -> Const 0
                  | hd::tl -> Sum[(Times ((diff (hd, x))::tl)); Times [hd;(diff ((Times tl),x))]])

   | Sum m-> match m with 
                | [] -> Const 0
                | hd::tl -> Sum [(diff (hd, x)); (diff ((Sum tl), x))]


(*problem 5 *)

type exp =
    X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec sub (f,x) = 

    match f with

    | X -> x
    | INT z -> z
    | ADD (a1,b2) -> sub (a1,x) + sub (b2,x)
    | SUB (a1,b2) -> sub (a1,x) - sub (b2,x)
    | MUL (a1,b2) -> sub (a1,x) * sub (b2,x)
    | DIV (a1,b2) -> sub (a1,x) / sub (b2,x)
    | SIGMA (a,b,f1) -> sub (SIGMA (ADD(a,INT(1)),b, f1),x)


let rec calculator e : exp->int
= fun e-> sub(e,1)
 

(*problem 6*)


type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile

and length = int
and weight = int

let node (l,w) = w



let rec scale bm = match bm with
|SimpleBranch(l,w)-> node(l,w)
|CompoundBranch(l,m)-> match m with
|(left,right) -> scale left + scale right


let rec balanced : mobile -> bool
= fun m -> match m with 
|(l,r)-> match l, r with
|(CompoundBranch(ln,mob),SimpleBranch(len,we))->if (ln* scale l)-(len *we) >0 ||(len *we) - (ln * scale l) >0 then false else true
|(CompoundBranch(ln,mob),CompoundBranch(lng,mo))->if (ln* scale l)-(lng*scale r) >0 ||(lng*scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),SimpleBranch(len,w))->if (le*we)-(len*w) >0 ||(len*w) - (le*we) >0 then false else true
|(SimpleBranch(le,we),CompoundBranch(lng,mo))->if (le*we)-(lng*scale r) >0 ||(lng*scale r) - (le*we) >0 then false else true



(*problem 7 *)

type digit = ZERO | ONE
type bin = digit list


let rec btoi b = match b with
|[]->0
|hd::tl -> hd+(2*btoi tl)

let rec ctob c = match c with
|[]->[]
|hd::tl -> if hd = ONE then ctob tl@[1] 
else ctob tl@[0]

let rec btoc b = match b with
|[]->[]
|hd::tl -> if hd = 1 then btoc tl@[ONE]
else btoc tl@[ZERO]

let rec itob i = match i with
|0 -> []
|a -> [a mod 2 ] @itob (a/2)

let rec bmul : bin -> bin -> bin
= fun b1 b2->
let a = btoi(ctob b1)in
let b = btoi(ctob b2)in
let c = (itob (a*b)) in btoc c
 