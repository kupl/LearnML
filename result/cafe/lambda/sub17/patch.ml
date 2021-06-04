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


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (m : lambda) : bool = List.length (__s3 m) = 0
