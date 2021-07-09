type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec envmt (e : lambda) : string list =
  let lst : string list = [] in

  match e with
  | V v -> lst
  | P (v, lambdar) -> (v :: envmt lambdar) @ lst
  | C (lambda1, lambda2) -> envmt lambda1 @ envmt lambda2 @ lst


let rec exist ((e : lambda), (lst : string list)) : int =
  match e with
  | V v -> (
      match lst with
      | [] -> 0
      | hd :: tl -> if hd = v then 1 + exist (e, tl) else exist (e, tl) )
  | P (v, lambda1) -> exist (lambda1, lst)
  | C (lambda1, lambda2) -> exist (lambda1, lst) + exist (lambda2, lst)


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool =
  let env : string list = envmt e in
  List.length (__s3 e) = 0
