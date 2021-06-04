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


let rec __s4 ((__s5 : lambda), (__s6 : string list)) : bool =
  match __s5 with
  | V __s7 -> List.exists (fun (__s8 : string) -> __s8 = __s7) __s6
  | P (__s9, __s10) -> __s4 (__s10, __s9 :: __s6)
  | C (__s11, __s12) -> __s4 (__s11, __s6) && __s4 (__s12, __s6)


let rec check (m : lambda) : bool =
  match m with __s3 -> __s4 (m, []) | _ -> false
