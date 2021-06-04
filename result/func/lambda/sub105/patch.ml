exception NotImplemented

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec varOfExp (lambda : lambda) : var =
  match lambda with
  | V var -> var
  | P (var, e) -> varOfExp e
  | C (e1, e2) -> varOfExp e2


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
