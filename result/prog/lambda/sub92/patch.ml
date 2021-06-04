type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec bound (lambda : lambda) : var list =
  match lambda with
  | V var -> []
  | P (var, ex) -> var :: bound ex
  | C (ex1, ex2) -> bound ex1 @ bound ex2


let rec variables (lambda : lambda) : var list =
  match lambda with
  | V var -> [ var ]
  | P (var, ex) -> variables ex
  | C (ex1, ex2) -> variables ex1 @ variables ex2


let rec containHelper ((bound : string list), (a : string)) : bool =
  match (bound, a) with
  | [], a -> false
  | x :: tl, a -> if x = a then true else containHelper (tl, a)


let rec contain ((bound : string list), (variables : string list)) : bool =
  match (bound, variables) with
  | [], variables -> false
  | bound, [] -> true
  | bound, a :: tl ->
      if containHelper (bound, a) = false then false else contain (bound, tl)


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


let check (lambda : lambda) : bool = List.for_all __s6 (__s3 (lambda, []))
