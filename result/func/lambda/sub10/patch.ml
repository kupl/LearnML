type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let __s3 ((__s4 : string list), (__s5 : string)) : bool =
  if List.exists (fun (__s19 : string) -> __s19 = __s5) __s4 then true
  else false


let rec __s6 ((__s7 : string list), (__s8 : lambda)) : bool =
  match __s8 with
  | V __s14 -> __s3 (__s7, __s14)
  | P (__s15, __s16) -> __s6 (__s7 @ [ __s15 ], __s16)
  | C (__s17, __s18) -> __s6 (__s7, __s17) && __s6 (__s7, __s18)


let rec check (lambda : lambda) : bool =
  match lambda with
  | P (__s10, __s11) -> __s6 ([ __s10 ], lambda)
  | P (a, V b) -> a = b
  | P (a, C (b, c)) -> check (P (a, b)) && check (P (a, c))
  | P (a, P (b, c)) -> check (P (a, c))
  | C (__s12, __s13) -> check __s13 && check __s12
  | C (_, _) -> false
  | V _ -> false
