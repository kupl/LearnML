type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) ((__s5 : string list), (__s6 : string list)) : bool
    =
  match __s4 with
  | V __s7 ->
      List.for_all (fun (__s8 : string) -> List.mem __s8 __s5) (__s7 :: __s6)
  | P (__s9, __s10) -> __s3 __s10 (__s9 :: __s5, __s6)
  | C (__s11, __s12) -> __s3 __s11 (__s5, __s6) && __s3 __s12 (__s5, __s6)


let check (lambda : lambda) : bool = __s3 lambda ([], [])
