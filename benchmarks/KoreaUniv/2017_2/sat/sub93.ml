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

let rec sat : formula -> bool
= fun f -> match f with
|True -> true
|False->false
|Neg p->match p with
        |Var x->true
        |True->false
        |False->true
        |_->false

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
