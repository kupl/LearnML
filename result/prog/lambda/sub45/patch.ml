type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec station_list (x : lambda) : string list =
  match x with
  | V n -> [ n ]
  | P (n, m) -> station_list m
  | C (m1, m2) -> List.append (station_list m1) (station_list m2)


let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | C (__s12, __s13) -> __s1 __s12 __s3 && __s1 __s13 __s3
  | P (__s14, __s15) -> __s1 __s15 (__s14 :: __s3)


let rec check (x : lambda) : bool =
  match x with
  | V n -> false
  | P (n, m) -> __s1 x [ n ]
  | C (m1, m2) -> check m1 && check m2
