type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check2 ((m : lambda), (l : string list)) : bool =
  match m with
  | P (nam, met) -> check2 (met, nam :: l)
  | V nam ->
      let x : string = List.find (fun (elem : string) -> elem = nam) l in
      true
  | C (met1, met2) -> check2 (met1, l) && check2 (met2, l)


let rec __s1 ((__s2 : lambda), (__s3 : var list)) : bool =
  match __s2 with
  | V __s6 -> List.mem __s6 __s3
  | P (__s7, __s8) -> __s1 (__s8, __s3 @ [ __s7 ])
  | C (__s9, __s10) -> __s1 (__s9, __s3) && __s1 (__s10, __s3)


let check (m : lambda) : bool = __s1 (m, [])
