(*1*)
type btree = Empty | Node of int * btree * btree;;
let rec  mirror : btree -> btree =
fun t ->
  match t with
 Empty-> Empty
| Node (n,Empty, Empty) -> Node( n,Empty,Empty)
| Node (n, left, Empty) -> Node(n, mirror Empty,mirror left)
| Node (n, Empty, right) ->Node(n, mirror right, mirror Empty)
| Node (n,left,right) ->Node (n,mirror right,mirror left);;


(*2*)
type nat = ZERO | SUCC of nat;;
let rec natadd : nat ->nat ->nat = fun n1 n2 ->
  match n1 with
  ZERO -> n2
| SUCC (n3) -> natadd n3 (SUCC(n2)) ;;

let rec  natmul : nat->nat->nat = fun n1 n2 ->
  let n4 = n1 in  
  let rec multemp : nat->nat->nat->nat = fun n1 n2 n4 ->
  match n2 with
  ZERO -> ZERO
| SUCC ZERO -> n1
| SUCC (n5) -> multemp (natadd n4 n1) n5 n4 in multemp n1 n2 n4 ;;

let natexp : nat ->nat -> nat = fun n1 n2 ->
  let n4 = n1 in
  let rec exptemp : nat->nat->nat->nat = fun n1 n2 n4 ->
  match n2 with
  ZERO -> SUCC ZERO
| SUCC ZERO -> n1
| SUCC (n5) -> exptemp (natmul n4 n1) n5 n4 in exptemp n1 n2 n4;;

(*3*)

type formula =
  True
 |False
 |Var of string
 |Neg of formula
 |And of formula*formula
 |Or of formula*formula
 |Imply of formula*formula
 |Iff of formula*formula ;;

 let rec sat : formula ->bool = fun f ->
  match f with
  True -> true
| False -> false
| Var(a) -> raise(Failure("Error")) 
| Neg(Var(a)) -> if (Var(a)=True) then sat(False) else sat(True)
| Neg(formula) -> if (formula=False) then sat(True) else sat(False)
(*| And(formula1,formula2) -> if (formula1=True && formula2=True) then sat(True) else sat(False)
*)
| And (True, True) -> sat(False)
| And (False, _) -> sat(False)
| And (True, formula1) -> if (formula1=True) then sat(True) else sat(False)
| And (formula1, formula2) -> if (formula1=True && formula2=True) then sat(True) else sat(False)
| Or (True, _) -> sat(True)
| Or (False, formula1) -> if (formula1 = True) then sat(True) else sat(False)
| Or(formula1, formula2)-> if (formula1=False && formula2=False) then sat(False) else sat(True)
| Imply(False, _) -> sat(True)
| Imply(True, formula1) -> if (formula1=True) then sat(True) else sat(False)
| Imply(formula1, formula2) -> if (formula1 = True && formula2=False) then sat(False) else sat(True)
| Iff(formula1,formula2) -> if (Imply(formula1,formula2)=True && Imply(formula2,formula1)=True) then sat(True) else sat(False)
 ;;


(*4*)
  
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x) ->
  match e with
  Const n -> Const 0
 |Var x-> Const 1
 |Power(x,1) -> Times[Var "x";]
 |Power(x,e) ->Times[Const e ; Power("x",(e-1))]
 |Times[Const n; Power("x",1)]-> Times[Const(n)]
 |Times[Const n; Power("x",e)]-> Times[Const (n*e); Power("x",(e-1))] 
 |Times[Const n; Var("x")]->Times[Const(n)]
 |Times _ -> raise(Failure("Error"))

 |Sum [aexp] -> Sum [diff (aexp,"x")]
 |Sum l -> match l with
                |[] -> raise(Failure("Error"))
                |hd::tl-> Sum [diff(hd,"x") ; diff (Sum tl,"x")];;

(*5*)

  type exp =
     X
    | INT of int
    | ADD of exp*exp
    | SUB of exp*exp
    | MUL of exp*exp
    | DIV of exp*exp
    | SIGMA of exp*exp*exp  
(*
  type env = (var*value) list;;
  let empty_env=[];;
  let extend_env (x,v) e = (x,v) ::e;;
  let rec apply_env x e =
    match e with
    | []-> raise (Failure("variable not fount"))
    | (y,v) :: tl -> if x=y then v else apply_env x tl;;
*)
  let rec calculatortemp : exp*int ->exp =fun (e, t) ->
  match e with
  | X->INT t
  | INT n-> INT n
  | ADD(e1,e2) -> ADD(calculatortemp(e1,t),calculatortemp(e2,t))
  | SUB(e1,e2) -> SUB(calculatortemp(e1,t),calculatortemp(e2,t))
  | MUL(e1,e2) -> MUL(calculatortemp(e1,t),calculatortemp(e2,t))
  | DIV(e1,e2) -> DIV(calculatortemp(e1,t),calculatortemp(e2,t))
  | SIGMA(e1,e2,e3)->calculatortemp(e3,t);;

  let rec calculator : exp -> int = fun e->
  (* let rec eval env = function
  match env with
  |Var x -> List.assoc x env
  *)
   match e with
  |INT k -> k
  |ADD (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2  in
      v1+v2
  |SUB (e1,e2) ->
    let v1 = calculator e1 in
    let v2 = calculator e2  in
      v1-v2
  |MUL (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2 in
      v1*v2
  |DIV (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2 in
      v1/v2
  |SIGMA (e1,e2,e3) ->
    let v1 = calculator e1 in
    let v2 = calculator e2 in
    let v3 = calculator(calculatortemp(SIGMA(e1,e2,e3),calculator (e1))) in
    if v1>v2 then 0 else v3 + calculator(SIGMA(ADD(e1, INT 1),e2,e3));;

(*6*)
type mobile = branch*branch (*left and right branches *)
and branch = SimpleBranch of length * weight
            |CompoundBranch of length * mobile
            and length = int
            and weight = int

let rec lengwei : branch->int = fun lw->
  match lw with
 |SimpleBranch(length,weight) -> length*weight
 |CompoundBranch (length,(SimpleBranch(length2,weight),SimpleBranch(length3,weight2))) -> (weight+weight2)
 |CompoundBranch (length,(branch1,branch2))-> length * (lengwei (branch1)+ lengwei(branch2));;
let balanced : mobile -> bool = fun m->
 match m with
 |(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->
  if (lengwei (SimpleBranch(l1,w1))) = (lengwei (SimpleBranch(l2,w2))) then true else false
 |(CompoundBranch(l1,(br1,br2)),SimpleBranch(l2,w2))->
  if (lengwei (CompoundBranch(l1,(br1,br2)))) = (lengwei (SimpleBranch(l2,w2))) then true else false
 |(SimpleBranch(l1,w1),CompoundBranch(l2,(br1,br2)))->
  if (lengwei (SimpleBranch(l1,w1))) = (lengwei (CompoundBranch(l2,(br1,br2)))) then true else false
 |(CompoundBranch(l1,w1),CompoundBranch(l2,(br1,br2)))->
  if (lengwei (CompoundBranch(l1,(br1,br2)))) = (lengwei (CompoundBranch(l2, (br1,br2)))) then true else false;;


(*7*)

  type digit = ZERO | ONE
  type bin = digit list
  
  let rec expt b n =
    if n = 0 then 1
    else b*(expt b(n-1));;
(*
  let rec decimal = fun a->
   let d = 0 in
   let c = List.rev a in
   let n = -1 in
   let e = 0 in
   let j = 0 in
   let rec decimaltemp = fun c d e->
   let k = expt 2 d in
     let j=e in
     if(List.hd c = "ONE")
           then decimaltemp (List.tl c) (n+1) (e+k)
     else 
  decimaltemp (List.tl c) (n+1) 0;;
  *)
(*
  let rec decimal : bin->int = fun a->
  let b = List.rev a in
  let c = 1 in
  let d = 0 in
  let n = 0 in
    let rec decimaltemp = fun b c->
  match b with
  | [] -> raise (Failure ("Error"))
  |hd::tl -> if hd=ONE 
          then (let d=d+c+(decimaltemp tl (expt 2 (n+1))) in decimaltemp b c)
          else (let d=d+0+(decimaltemp tl (expt 2 (n+1))) in decimaltemp b c);;
*)
(*
  let rec binary = fun n ->
    let l =[] in
      let rec binarytemp : digit->int->digit= fun l n->
      if (n/2 = 1) then ONE :: l
       else if(n mod 2 = 1) then ONE :: binarytemp l n/2
             else ZERO :: binarytemp l n/2;;
*)
    (*
  let rec binary = fun n->
    let l =[] in
      let rec binarytemp = fun l n->
      match l with
      | [] -> if(n mod 2= 1) then ONE::binarytemp l n/2
      |hd::tl -> if(n/2 <> 1)
                    then if(n mod 2=1) then 
*)


 (* let bmul : bin -> bin ->bin = fun b1 b2 ->
   *)
