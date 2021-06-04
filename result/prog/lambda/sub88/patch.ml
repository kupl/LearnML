type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isexist (vars : string list) (v : string) : bool =
  match vars with
  | [] -> false
  | hd :: tl -> if hd = v then true else isexist tl v


let rec chkvars (lambda : lambda) (vars : string list) : bool =
  match lambda with
  | V v -> isexist vars v
  | P (v, e) -> chkvars e (v :: vars)
  | C (V v, e) -> chkvars e (v :: vars)


let rec __s3 (__s4 : lambda) (__s5 : var list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | P (__s7, __s8) ->
      let __s9 : var list = __s7 :: __s5 in
      __s3 __s8 __s9
  | C (__s10, __s11) -> __s3 __s10 __s5 && __s3 __s11 __s5


let check (lambda : lambda) : bool = __s3 lambda []
