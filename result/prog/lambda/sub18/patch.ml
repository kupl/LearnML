type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

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


let rec check (lambda : lambda) : bool =
  let rec varStation (lambda : lambda) : string list =
    match lambda with
    | V n -> [ n ]
    | P (n, m) -> varStation m
    | C (m1, m2) -> List.append (varStation m1) (varStation m2)
  in

  let rec varArea (lambda : lambda) : string list =
    match lambda with
    | V n -> []
    | P (n, m) -> n :: varArea m
    | C (m1, m2) -> List.append (varArea m1) (varArea m2)
  in

  let rec check2 (l1 : string list) (l2 : string list) : bool =
    match l1 with [] -> true | hd :: tl -> List.mem hd l2 && check2 tl l2
  in
  List.for_all __s6 (__s3 (lambda, []))
