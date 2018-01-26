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
