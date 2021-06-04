type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

type bounded = var list

let update_bounded (a : string) (stk : 'a list) : 'a list = a :: stk

let rec find_bounded a (stk : 'b list) : bool =
  match stk with
  | [] -> false
  | hd :: tl -> if hd = a then true else find_bounded a tl


let rec boundCheck (lam : lambda) (bd : string list) : bool =
  match lam with
  | V v -> find_bounded v bd
  | P (v, l) ->
      let s : string list = update_bounded v bd in
      boundCheck l s
  | C (l1, l2) -> if boundCheck l2 bd then boundCheck l2 bd else false


let rec __s3 (__s4 : lambda) (__s5 : var list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | P (__s7, __s8) ->
      let __s9 : var list = __s7 :: __s5 in
      __s3 __s8 __s9
  | C (__s10, __s11) -> __s3 __s10 __s5 && __s3 __s11 __s5


let rec check (lam : lambda) : bool = __s3 lam []
