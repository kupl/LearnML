type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) (__s5 : string list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | C (__s7, __s8) -> __s3 __s7 __s5 && __s3 __s8 __s5
  | P (__s9, __s10) -> __s3 __s10 (__s5 @ [ __s9 ])


let check (m : lambda) : bool =
  let rec checkRec (m : lambda) (s : var list) : bool =
    match m with
    | V n -> List.mem n s
    | P (n, m) -> checkRec m s
    | C (m1, m2) -> checkRec m1 s && checkRec m2 s
  in
  __s3 m []
