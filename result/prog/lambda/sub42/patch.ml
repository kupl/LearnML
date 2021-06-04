type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec station_list (x : lambda) : string list =
  match x with
  | V n -> [ n ]
  | P (n, m) -> station_list m
  | C (m1, m2) -> List.append (station_list m1) (station_list m2)


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


let rec check (x : lambda) : bool = List.for_all __s6 (__s3 (x, []))
