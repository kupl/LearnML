type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool =
  let rec remove_var (lambda : lambda) (var : string) : lambda =
    match lambda with
    | P (v, ex) -> P (v, remove_var ex var)
    | C (ex, ex') -> C (remove_var ex var, remove_var ex' var)
    | V v -> if v = var then V "X" else V v
  in
  List.length (__s3 e) = 0
