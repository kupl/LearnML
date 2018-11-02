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
    | x::xs -> if e = x then go xs acc else go xs (x::acc)
  in go l [];;

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l [];;


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