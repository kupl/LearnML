(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula;;

  let rec leftfind:formula->formula
  =fun n->
  match n with
  |True->True
  |False->False
  |Var x->Var x
  |Neg x->leftfind x
  |And(a,b)->leftfind a
  |Or(a,b)->leftfind a
  |Imply(a,b)->leftfind a
  |Iff(a,b)->leftfind a;;


  let rec rightfind:formula->formula
   =fun n->
    match n with
  |True->True
  |False->False
  |Var x->Var x
  |Neg x->rightfind x
  |And(a,b)->rightfind b
  |Or(a,b)->rightfind b
  |Imply(a,b)->rightfind b
  |Iff(a,b)->rightfind b;;

let rec tt:formula->formula
=fun n->
match n with
True->True
|False->False
|Var n->True
|Neg n->False
|And(a,b)->if tt a=True &&tt b=True then True else False
|Or(a,b)->if tt a=True||tt b= True then True else False
|Imply(a,b)->if tt a=True&&tt b=True then True else if tt a=True&&tt b=False then False else True
|Iff(a,b)->if tt a=tt b then True else False;;

 
 let rec ff:formula->formula
 =fun n->
   match n with
|True->True
|False->False
|Var n->False
|Neg n->True
|And(a,b)->if ff a=True &&ff b=True then True else False
|Or(a,b)->if ff a=True||ff b= True then True else False 
|Imply(a,b)->if ff a=True&&ff b=True then True else if ff a=True&&ff b=False then False else
              True
|Iff(a,b)->if ff a=ff b then True else False;;

 
  let rec tf:formula->formula
 =fun n->
   match n with
|True->True
|False->False
|Var n->True
|Neg n->if tf n=True then False else True
|And(a,b)->if tt a=True &&ff b=True then True else False
|Or(a,b)->if tt a=True||ff b= True then True else False
|Imply(a,b)->if tt a=True&&ff b=True then True else if tt a=True&&ff b=False then False else
              True
|Iff(a,b)->if tt a=ff b then True else False;;


 let rec ft:formula->formula
 =fun n->
  match n with
|True->True
|False->False
|Var n->True
|Neg n->if ft n=True then False else True
|And(a,b)->if ff a=True &&tt b=True then True else False
|Or(a,b)->if ff a=True||tt b= True then True else False
|Imply(a,b)->if ff a=True&&tt b=True then True else if ff a=True&&tt b=False then False else
               True
|Iff(a,b)->if ff a=tt b then True else False;;

let sat : formula -> bool
= fun f -> (* TODO *)
if leftfind f=rightfind f then begin if tt f=True||ff f=True then true else false end
else begin if tt f=True||tf f=True||ft f=True||ff f=True then true else false end;;