type formula=
|True
|False
|Not of formula
|AndAlso of formula*formula
|OrElse of formula*formula
|Imply of formula*formula
|Equal of exp*exp
and exp=
|Num of int
|Plus of exp*exp
|Minus of exp*exp
let rec trans:exp->exp
=fun e1-> match e1 with
|Plus(Num(a),Num(b))->Num(a+b)
|Plus(Num(a),e3)->Plus(Num(a),trans(e3))
|Plus(e2,Num(b))->Plus(trans(e2),Num(b))
|Plus(e2,e3)->Plus(trans(e2),trans(e3))
|Minus(Num(a),Num(b))->Num(a-b)
|Minus(Num(a),e3)->Minus(Num(a),trans(e3))
|Minus(e2,Num(b))->Minus(trans(e2),Num(b))
|Minus(e2,e3)->Minus(trans(e2),trans(e3))
|Num a->Num a
let rec eval:formula->bool
=fun f->match f with
|True->true
|False->false
|Not f1->if f1=True then false else true
|AndAlso (f1,f2)->if (f1=True && f2=True) then true else false
|OrElse (f1,f2)->if (f1=False && f2=False) then false else true
|Imply (f1,f2)->if (f1=True && f2=False) then false else true
|Equal (e1,e2)-> if (trans(e1)=trans(e2)) then true else false;;


