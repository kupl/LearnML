(* problem 1*)
type btree =
Empty
|Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> (* TODO *)
match t with
|Empty ->t
|Node(m,t1,t2)->Node(m,mirror t2,mirror t1)

  (* problem 2*)
  type nat =
   ZERO
  | SUCC of nat

  let rec natadd : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
  match n2 with
  |ZERO->n1
  |SUCC k2-> (SUCC (natadd n1 k2))
  let rec natmul : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
      match n2 with
      |ZERO->n1
      |SUCC k -> if k=ZERO then n1 else natadd n1 (natmul n1 k) 
  
  let rec natexp : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
   match n2 with
   |ZERO->SUCC ZERO
   |SUCC k -> if k=ZERO then SUCC ZERO else natmul (natmul n1 k) n1

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
  let sat : formula -> bool
  = fun f -> (* TODO *)
  match f with
  |True->true
  |False->false
  |Var f->bool_of_string f
  |Neg f->if f =True then false else true
  |And(f1,f2)->if f1=True&&f2=True then true else false
  |Or(f1,f2)->if f1=True||f2=True then true else false
  |Imply(f1,f2)->if f1=False||f2=True then true else false
  |Iff(f1,f2)->if f1=f2 then true else false

(* problem 4*)
  type aexp =
    | Const of int
     | Var of string
     | Power of string * int
     | Times of aexp list
     | Sum of aexp list

     let diff : aexp * string -> aexp
     = fun (e,x) -> (* TODO *)
     match e with
     |const n->const 0
     |

  (* problem 5*)
  type exp = X
           | INT of int
           | ADD of exp * exp           
           | SUB of exp * exp
           | MUL of exp * exp
           | DIV of exp * exp
           | SIGMA of exp * exp * exp

     let rec calculator : exp -> int
       = fun e -> (* TODO *)
        let rec etoi (exp,int)=
          match exp with
      | X->etoi(int,int)
      | INT n->n
      | ADD(f1,f2)->etoi(f1,int)+etoi(f2,int)
      | SUB(f1,f2)->etoi(f1,int)-etoi(f2,int)
      | MUL(f1,f2)->etoi(f1,int)*etoi(f2,int)
      | DIV(f1,f2) ->if etoi(f2,int)=0 then raise(Faliure "0 division") else etoi(f1,int)/etoi(f2,int)
      in
      match e with
      | INT i->i
      | ADD(f1,f2)->etoi(ADD(f1,f2),INT 0)
      | SUB(f1,f2)->etoi(SUB(f1,f2),INT 0)
      | MUL(f1,f2)->etoi(MUL(f1,f2),INT 0)
      | DIV(f1,f2)-> etoi(DIV(f1,f2),INT 0)
      | SIGMA (i1, i2, f1)->if etoi(SUB(i1,i2),INT 0)=0 then etoi(f1,i2) else     etoi(f1,i1)+calculator(SIGMA(ADD(INT 1,i1),i2,f1))

  (* problem 6*)
  type mobile = branch * branch     (* left and rigth branches *)
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
             and length = int
             and weight = int

   let rec balanced : mobile -> bool
   = fun m -> (* TODO*)
   let rec branchweight branch = 
   match branch with
   |SimpleBranch (a,b) -> b
   |CompoundBranch (a,(b1,b2))->(branchweight b1)+(branchweight b2)
   in
   match m with
   |SimpleBranch(a1,b1),SimpleBranch(a2,b2)->if a1*branchweight(SimpleBranch(a1,b1))=a2*branchweight(SimpleBranch(a2,b2)) then true else false
   |SimpleBranch(a1,b1),CompoundBranch(a2,(b2,b3))->if a1*branchweight(SimpleBranch(a1,b1))=a2*branchweight(CompoundBranch(a2,(b2,b3))) then balance(b2,b3) else false
   |CompoundBranch(a1,(b1,b2)),SimpleBranch(a2,b3)->if a1*branchweight(CompoundBranch(a1,(b1,b2)))=a2*branchweight(SimpleBranch(a2,b3)) then balance(b1,b2) else false
   |CompoundBranch(a1,(b1,b2)),CompoundBranch(a2,(b3,b4))->if a1*branchweight(CompoundBranch(a1,(b1,b2)))=a2*branchweight(CompoundBranch(a2,(b3,b4))) then balance(b1,b2)&&balance(b3,b4) else false

   
  (* problem 7*)
  type digit = ZERO|ONE
  type bin = digit list

  let ten : bin -> int
  = fun t1 ->
  let rec ten1 : bin -> int
  = fun t1 ->
  match t1 with
  |[]->0
  |hd::tl->if hd==ONE then 1+2*(ten1 tl)
           else 2*(ten1 tl) in
           match List.rev t1 with
           |[]->0
           |hd::tl->if hd==ONE then 1+2*(ten1 tl)
           else 2*(ten1 tl)

  let rec bmul : bin->bin->bin
  =fun b1 b2 -> (* TODO *)
  let rec bin t1 =
  match t1 with
  |0->[ZERO]
  |1->[ONE]
  |_->if t1 mod 2=0 then (bin (t1/2))@[ZERO]
      else (bin ((t1-1)/2))@[ONE] in
      if b1=[]||b2=[] then []
       else match (ten b1)*(ten b2)  with
       |0-> [ZERO]
       |1-> [ONE]
       |_->if (ten b1)*(ten b2) mod 2 = 0 then  (bin ((ten b1)*(ten b2)/2))@[ZERO] 
           else (bin (((ten b1)*(ten b2)-1)/2))@[ONE]

