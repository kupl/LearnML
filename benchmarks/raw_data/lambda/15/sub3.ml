(*
    PL 2-3
    2008-11609 박성원
*)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check lambda =
  let rec findInList l v =
    match l with
    | [] -> false
    | h :: l -> (h = v) || (findInList l v)
  in
  let rec checkImpl lambda areaNames =
    match lambda with
    | V var -> findInList areaNames var
    | P (var, lambda) -> checkImpl lambda (var :: areaNames)
    | C (lambda1, lambda2) -> (checkImpl lambda1 areaNames) && (checkImpl lambda2 areaNames)
  in
  checkImpl lambda []
