type var = string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

let rec checkName ((n : string), (mtr : lambda)) : bool =
  match mtr with
  | V s -> if n = s then true else false
  | P (n1, mtr1) -> checkName (n1, mtr1)
  | C (m1, m2) -> (
      match (m1, m2) with
      | _, P (nm1, mt1) -> checkName (n, mt1)
      | P (nm2, mt2), _ -> checkName (n, mt2)
      | _, _ -> checkName (n, m1) || checkName (n, m2) )


let rec check (m : lambda) : bool =
  match m with P (n, mtr) -> checkName (n, mtr) | _ -> false
