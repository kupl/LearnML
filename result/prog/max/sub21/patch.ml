let max_f (a : int) (b : int) : int = if a > b then a else b

let rec __s8 (__s9 : int -> int -> int) (__s10 : int list) (__s11 : int) : int =
  match __s10 with
  | [] -> __s11
  | __s12 :: __s13 -> __s9 __s12 (__s8 __s9 __s13 __s11)


let rec max (lst : int list) : int =
  match lst with [] -> -99999 | hd :: tl -> __s8 max_f tl hd
