exception Problem;;

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
Empty -> Empty
|Node(a,b,c)->Node(a,(mirror c),(mirror b));;



(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1, n2 with
|ZERO, ZERO -> ZERO
|ZERO, nat2 -> nat2
|nat1, ZERO -> nat1
|SUCC a, SUCC b ->SUCC(SUCC (natadd a b));;


let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->match n1, n2 with
|ZERO, ZERO -> ZERO
|ZERO, nat2 -> ZERO
|nat1, ZERO -> ZERO
|SUCC ZERO, nat2 ->nat2
|nat1, SUCC ZERO -> nat1
|nat1, SUCC b ->natadd (natmul (SUCC ZERO) nat1) (natmul nat1 b) ;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1, n2 with
|ZERO, nat2-> ZERO
|SUCC ZERO, nat2-> SUCC ZERO
|nat1, ZERO -> SUCC ZERO
|nat1, SUCC b-> natmul nat1 (natexp nat1 b);;


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

let rec createvar v = match v with
|True -> [True]
|False ->[False]
|Var x->[True; False]
|_->[];;


(*f 안에 있는 변수 찾기.*)
let rec findvar f = match f with
|True->[True]
|False->[False]
|Var x->[Var x]
|Neg x->findvar(x)
|And (f1, f2)->findvar(f1)@findvar(f2)
|Or (f1, f2)->findvar(f1)@findvar(f2)
|Imply (f1, f2)->findvar(f1)@findvar(f2)
|Iff (f1, f2)->findvar(f1)@findvar(f2)



let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l [];;

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l [];;





(*각 변수마다 [T;F]리스트 만들어주기
let rec f = match f with
|hd::tl->match hd with
        |True-> let hd = [True; False] in 
        |False-> let hd =[True; False]
        |Var x-> let hd=[True; False]

let a = Or(And(Var "x", Var "y"), Var "x");;
let b = findvar a;;(*[Var "x"; Var "y"; Var "x"]*)

*)
(*

[a1; a2]연산자[b1;b2]->[(a1,b1);(a1,b1);(a1,b1);(a1,b1)]

Var "x" 는 x[true; false]
Add (Var "x", Var "x") x[true; false]and x[true;false]->이름 같으면 2개
이름 다르면 


let product f1 f2 = match f1, f2 with

let rec opt f = mathc f with
|True->[true]
|False->[false]


*)

let rec sat : formula -> bool
= fun f -> match f with
|True -> true
|False->false
|Neg p->match p with
        |Var x->true
        |True->false
        |False->true
        |_->false




(*f안에 변수 찾기-> 변수 중복 제거-> 변수 개수세기 (ok) 

[Var "x", Var "y", Var "z"]

f1 = [var x->t Var "y", Var "z"]인거 ->[var x->t var y->t,var z] -> [var x->t var y->t,var z->t] 
                                                              [var x->t var y->t,var z->f] 
                                  [var x->t var y->t,var z] -> [var x->t var y->t,var z->t]
                                                              [var x->t var y->t,var z->f] 


f2 = [var x->f Var "y", Var "z"]인거 

==>총 8개를 init[]에 저장 그리고 나서 truthtable2 init -> True 갯수세기


->  power(2, 변수갯수) -> tf 변환 -> [리스트에 저장]->리스트 truthtable2돌리기 -> True 존재 체크  *)


let rec length l = match l with
|[]->0
|hd::tl->1+(length tl);;

(*f 안에 있는 변수 찾기.*)
let rec findvar f = match f with
|True ->[True]
|False->[False]
|Var x->[Var x]
|Neg x->findvar(x)
|And (f1, f2)->findvar(f1)@findvar(f2)
|Or (f1, f2)->findvar(f1)@findvar(f2)
|Imply (f1, f2)->findvar(f1)@findvar(f2)
|Iff (f1, f2)->findvar(f1)@findvar(f2);;


let rec changeexptrue f x = match f with
|True -> True
|False-> False
|Var p-> (if x=p then True else Var p)
|Neg p-> (match p with
        |Var t ->if t=x then False else Neg p
        |_->(let pp=(changeexptrue p x) in Neg p)
      )
|And(f1, f2)->(let ff1 = (changeexptrue f1 x) in
              let ff2 = (changeexptrue f2 x) in
              And(ff1, ff2)
            )
|Or(f1, f2)->(let ff1 = (changeexptrue f1 x) in
              let ff2 = (changeexptrue f2 x) in
              Or(ff1, ff2)
              )
|Imply(f1, f2)->(let ff1 = (changeexptrue f1 x) in
              let ff2 = (changeexptrue f2 x) in
              Imply(ff1, ff2)
            )
|Iff(f1, f2)->(let ff1 = (changeexptrue f1 x) in
              let ff2 = (changeexptrue f2 x) in
              Iff(ff1, ff2)
            )
|_->False;;

let rec changeexpfalse f x = match f with
|True -> True
|False-> False
|Var p-> (if x=p then False else Var p)
|Neg p-> (match p with
        |Var t ->if t=x then True else Neg p
        |_->(let pp=(changeexpfalse p x) in Neg p)
      )
|And(f1, f2)->(let ff1 = (changeexpfalse f1 x) in
              let ff2 = (changeexpfalse f2 x) in
              And(ff1, ff2)
            )
|Or(f1, f2)->(let ff1 = (changeexpfalse f1 x) in
              let ff2 = (changeexpfalse f2 x) in
              Or(ff1, ff2)
              )
|Imply(f1, f2)->(let ff1 = (changeexpfalse f1 x) in
              let ff2 = (changeexpfalse f2 x) in
              Imply(ff1, ff2)
            )
|Iff(f1, f2)->(let ff1 = (changeexpfalse f1 x) in
              let ff2 = (changeexpfalse f2 x) in
              Iff(ff1, ff2)
            )
|_->False;;


let rec sat : formula -> bool
= fun f -> 
let fvar = remove_duplicates(findvar f) in true ;;


let b = And(Var "x", Var "y");;
let ffff = remove_duplicates(findvar b);;
(*[Var "x"; Var "y"]

f1 : And(True, Var "y")
f2 : And(False, Var "y")

tl :  [Var "y"]

r1 : And(True, t)

[] And(True, True)
*)
let init = [];;


let rec result f fvar =
  let a = [] in    
   (match fvar with
   |[]->[]
   |hd::tl->(match hd with
            |Var x->(let f1 = (changeexptrue f x) in (*f1은 fomula "x"->T인 나머지는 아직 var*)
                      let f2 = (changeexpfalse f x) in (*f2은 fomula "x"->F인 나머지는 아직 var*)
                        let r1 = result f1 tl in
                          let r2 = result f2 tl in 
                          (r1@r2@[f1]@[f2]@a)
                      )
            |True-> (result f tl)@a
            |False-> (result f tl)@a
            )
 );;



(*)
let rec remove li = 
  match li with
  |[]->[]
  |hd::tl->(let tt = findvar(hd) in
            (match tt with
            |thd::thl -> (match thd with
                          |Var x->remove tl
                      )
            |_->hd::(remove tl)));;    
          

let rec tt fl =match fl with
|[]->[False]
|hd::tl->(truthtable2 hd)::(tt tl);;

*)
let rec truthtable2 f = match f with
|True ->True
|False->False
|And(f1, f2)->(let ff1 = truthtable2 (f1) in
              let ff2 = truthtable2 (f2) in
              match ff1, ff2 with
              |True, True->False
              |_,_->False
            )
|Or(f1, f2)->(let ff1 = truthtable2 (f1) in
              let ff2 = truthtable2 (f2) in
              match ff1, ff2 with
              |True, True->True
              |True, _-> True
              |_,True->True
              |_,_->False
              )
|Imply(f1, f2)->(let ff1 = truthtable2 (f1) in
              let ff2 = truthtable2 (f2) in
              match ff1, ff2 with
              |True, True->True
              |True, False->False
              |False, True->True
              |False, False->True
            )
|Iff(f1, f2)->(let ff1 = truthtable2 (f1) in
              let ff2 = truthtable2 (f2) in
              match ff1, ff2 with
              |True, True->True
              |True, False->False
              |False, True->False
              |False, False->True
            )
|_->False



let rec truthtable f = match f with
|True ->true
|False->false
|And(f1, f2)->(let ff1 = truthtable (f1) in
              let ff2 = truthtable (f2) in
              match ff1, ff2 with
              |true, true->false
              |_,_->false
            )
|Or(f1, f2)->(let ff1 = truthtable (f1) in
              let ff2 = truthtable (f2) in
              match ff1, ff2 with
              |true, true->true
              |true, _-> true
              |_,true->true
              |_,_->false
              )
|Imply(f1, f2)->(let ff1 = truthtable (f1) in
              let ff2 = truthtable (f2) in
              match ff1, ff2 with
              |true, true->true
              |true, false->false
              |false, true->true
              |false, false->true
            )
|Iff(f1, f2)->(let ff1 = truthtable (f1) in
              let ff2 = truthtable (f2) in
              match ff1, ff2 with
              |true, true->true
              |true, false->false
              |false, true->false
              |false, false->true
            )
|_->false

(*
type env = (string * formula) list

let rec expo :int->int->int = fun n p ->
    if p = 0 then 1
    else if p =1 then n
    else n*(expo n (p-1))

let update_env : (string * formula)->(string * formula) list->(string * formula) list = fun (a,b) env->
    (a,b)::env;;

let rec find_env : string->(string * formula) list->formula = fun s env->
    match env with 
    |[]->raise(Failure "error")
    |(x,en)::tl->(if s=x then en
                else find_env s tl
            )
;;

let rec overlap : string list -> string -> formula = fun lst s ->
match lst with 
    |[]->False
    |hd::tl->if hd = s then True
            else overlap tl s
;;
let rec varlst : formula -> string list ->string list= fun f lst->
match f with
    |True -> lst
    |False -> lst
    |Var x-> if (overlap lst x) = False then x::lst
            else lst
    |Neg a->varlst a lst
    |And (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
                else varlst a lst@varlst b lst)
    |Or (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
                else varlst a lst@varlst b lst)
    |Imply (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
                else varlst a lst@varlst b lst)
    |Iff (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
                else varlst a lst@varlst b lst)
;;

let rec length : 'a list -> int = fun lst->
match lst with |[]->0 |hd::tl->1+(length tl);;

let rec length_plus : string list->formula list->formula list = fun slst flst->
if length slst != length flst then ([False]@flst)
else flst
;;

let rec dec_to_formula : int->formula list = fun n->
if (n/2 != 1 && n/2 !=0 && n mod 2 = 0) then dec_to_formula(n/2)@[False]
else if (n/2 != 1 && n/2 !=0 && n mod 2 = 1) then dec_to_formula(n/2)@[True]
else if (n/2 = 1 && n mod 2 = 0) then [True;False]
else if (n/2 = 1 && n mod 2 = 1) then [True;True]
else if n = 1 then [True]
else [False]
;;

let rec mkenv : string list -> formula list ->(string*formula) list  = fun lst flst->
    match lst with 
    |hd::tl->(match flst with
                |hd2::tl2->(hd,hd2)::mkenv tl tl2
                |[]->(hd,False)::mkenv tl []
            )
    |[]->[]
;;
let rec sat2 : formula -> env-> formula = fun f env->
match f with
    |True -> True
    |False -> False
    |Var x-> find_env x env 
    |Neg a -> if (sat2 a env)=True then False
                else True
    |And (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
                    (match (c,d) with 
                        |(True,True)->True
                        |(True,False)->False
                        |(False,True)->False
                        |(False,False)->False
                    )
    |Or (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
                    (match (c,d) with 
                        |(True,True)->True
                        |(True,False)->True
                        |(False,True)->True
                        |(False,False)->False
                    )
    |Imply (a,b)-> let (c,d) = (sat2 a env,sat2 b env) in
                    (match (c,d) with 
                        |(True,True)->True
                        |(True,False)->False
                        |(False,True)->True
                        |(False,False)->True
                    )
    |Iff (a,b)->let (c,d) = (sat2 a env,sat2 b env) in
                    (match (c,d) with 
                        |(True,True)->True
                        |(True,False)->False
                        |(False,True)->False
                        |(False,False)->True
                    )
;;
let rec sat3 : string list->formula ->int ->formula list->formula list = fun slst f n flst->
let a = length slst in
    let b = (expo 2 a) in
        (if(n!=b) then
            [(sat2 f (mkenv slst (length_plus slst (dec_to_formula n))))]@
            (sat3 slst f (n+1) flst)
        else 
        []
        )
;;
let rec find_true : formula list->bool = fun flst->
match flst with 
    |[]->false
    |hd::tl->(if hd=True then true
            else find_true tl
    )
;;
let rec sat : formula -> bool = fun f->
    let a = (varlst f []) in
        let b= sat3 a f 0 [] in
        find_true b
;;

*)


(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
|Const n->Const 0
|Var a -> if a=x then Const 1 else Const 0
|Power (a, n) -> if a=x then Times[Const n ; Power (a, (n-1))] else Const 0
|Times li ->(match li with
            |[] -> Const 0
            |hd::[]->diff (hd, x)
            |hd::tl ->(match hd with
                      |Const n -> Times [hd; diff (Times tl, x)]
                      |_->Sum ([Times ([diff (hd,x)]@tl)]@[Times ([hd]@[diff ((Times tl),x)])])))
|Sum li-> (match li with
          |[]->Const 0
          |hd::[] -> diff(hd, x)
          |hd::tl-> (match hd with 
                    |Const n -> diff (Sum tl, x)
                    |_->Sum ([diff (hd, x)]@[diff((Sum tl), x)])));;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec sigma (f, a, b) = (if a=b then (f a)
                          else if a<b then (f a) + sigma (f, a+1, b)
                        else raise Problem)

let rec calculator1 : exp -> int
= fun e -> match e with
  |X->0
  |INT n ->n
  |ADD(e1, e2)->calculator1(e1)+calculator1(e2)
  |SUB(e1, e2) -> calculator1(e1)-calculator1(e2)
  |MUL(e1, e2)-> calculator1(e1)*calculator1(e2)
  |DIV(e1, e2)->calculator1(e1)/calculator1(e2)
  |_-> raise Problem

let rec makefun ex =match ex with
|X-> (fun x->x)
|INT n -> (fun x->n)
|ADD(e1, e2)->(fun x->(((makefun e1)x)+((makefun e2)x)))
|SUB(e1, e2)->(fun x->(((makefun e1)x)-((makefun e2)x)))
|MUL(e1, e2)->(fun x->(((makefun e1)x)*((makefun e2)x)))
|DIV(e1, e2)->(fun x->(((makefun e1)x)/((makefun e2)x)))
|SIGMA(e1,e2,e3)->((fun x->(makefun(INT(sigma(makefun(e3), calculator1(e1), calculator1(e2)))))x))
|_->(fun x->0)

let rec calculator : exp -> int
= fun e -> match e with
  |X->0
  |INT n ->n
  |ADD(e1, e2)->calculator(e1)+calculator(e2)
  |SUB(e1, e2) -> calculator(e1)-calculator(e2)
  |MUL(e1, e2)-> calculator(e1)*calculator(e2)
  |DIV(e1, e2)->calculator(e1)/calculator(e2)
  |SIGMA(e1, e2, e3) ->(match e3 with
                        |_->sigma(makefun(e3), calculator(e1), calculator(e2)))
  |_-> raise Problem;;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

 let rec cal_weight : branch->weight
  = fun br ->
  match br with
  |SimpleBranch(l, w) -> w
  |CompoundBranch(l, m) -> (match m with (lb,rb) -> (cal_weight lb)+(cal_weight rb));;
  
  let rec cal_torque : branch->int
  =fun br -> match br with
  |SimpleBranch (l,w) -> l*w
  |CompoundBranch(l,m) -> (match m with (lb, rb) -> l*(cal_weight lb + cal_weight rb));;


let rec balanced : mobile -> bool
= fun m -> match m with (lb, rb)->
          (match (lb, rb) with 
          |SimpleBranch(l1, w1), SimpleBranch(l2, w2) -> if l1*w1=l2*w2 then true
          else false
          |SimpleBranch(l1, w1),CompoundBranch(l2, m2) -> (match m2 with (lb,rb) ->
          (if (balanced m2) then (if l2*(cal_weight(lb)+cal_weight(rb))=l1*w1 then true else false)else false))
          |CompoundBranch(l1, m1), SimpleBranch(l2, w2) -> (match m1 with (lb, rb) ->
            (if (balanced m1) then (if l1*(cal_weight(lb)+cal_weight(rb)) = l2*w2 then true else false) else false))
          |CompoundBranch(l1, m1), CompoundBranch(l2, m2) ->(match m1, m2 with (lb1, rb1),(lb2,rb2) ->
          (if (balanced m1)&&(balanced m2) then (if l1*(cal_weight(lb1)+cal_weight(rb1))=l2*(cal_weight(lb2)+cal_weight(rb2)) then true else false) else false))
          |_ -> raise Problem
          );;




(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length b = match b with
                  |[]->0
                  |hd::tl->1+(length tl);;

let rec reverse b = match b with
                  |[]->[]
                  |hd::tl->(reverse tl)@[hd];;

let rec btod b = match b with
                |[ONE]->1
                |[ZERO]->0
                |hd::tl->(if hd=ONE then 1+2*(btod tl)
                        else 2*(btod tl));;
                

let rec dtob b = match b with
                  |0->[ZERO]
                  |1->[ONE]
                  |n ->(if (n mod 2 = 0) then (dtob (n/2))@[ZERO]
                        else (dtob (n/2))@[ONE]);;
                 
let bmul : bin -> bin -> bin
= fun b1 b2 -> let br1 = reverse b1 in
               let br2 = reverse b2 in
               dtob((btod br1)*(btod br2));;

