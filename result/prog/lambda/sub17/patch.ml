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


let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let __s6 (__s7 : lambda * string list) : bool =
  match __s7 with
  | V __s8, __s9 -> List.mem __s8 __s9
  | P (__s10, __s11), __s12 -> false
  | C (__s13, __s14), __s15 -> false


let rec check (m : lambda) : bool = List.for_all __s6 (__s3 (m, []))
