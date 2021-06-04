type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec equi x (l : 'a list) : bool =
  match l with [] -> true | hd :: tl -> x = hd && equi x tl


let rec check_r (e : lambda) (env : string list) : bool =
  match e with
  | V var -> if env = [] then false else equi var env
  | P (v, e) -> check_r e (v :: env)
  | C (e1, e2) -> check_r e1 env && check_r e2 env


let rec __s3 (__s4 : lambda) (__s5 : var list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | P (__s7, __s8) ->
      let __s9 : var list = __s7 :: __s5 in
      __s3 __s8 __s9
  | C (__s10, __s11) -> __s3 __s10 __s5 && __s3 __s11 __s5


let check (e : lambda) : bool = __s3 e []
