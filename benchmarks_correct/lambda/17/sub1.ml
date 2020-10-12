type lambda = V of var
            | P of var*lambda
            | C of lambda*lambda
and var = string

let rec checklambdalist alambda checklist =
  match alambda with
 | V (a) -> List.mem a checklist
 | P (a,b) -> checklambdalist b (a::checklist)
 | C(a,b)-> (checklambdalist a checklist) && (checklambdalist b checklist)
;;

let check alambda = checklambdalist alambda []
;;


