let empty_env' : 'a list = []

let extend_env' (x : lambda) (e : 'b list) : 'b list = x :: e

let rec apply_env' (e : 'c list) x : bool =
  match e with
  | [] -> false
  | y :: tl -> if x = y then true else apply_env' tl x


type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s2 (__s3 : lambda) (__s4 : string list) : bool =
  match __s3 with
  | V __s7 -> List.mem __s7 __s4
  | P (__s8, __s9) -> __s2 __s9 (__s8 :: __s4)
  | C (__s10, __s11) -> __s2 __s10 __s4 && __s2 __s11 __s4


let rec check (lam : lambda) : bool =
  let rec check' (lam : lambda) (env : lambda list) : bool =
    match lam with
    | V x -> apply_env' env lam
    | P (x, f) ->
        let proc : lambda list = extend_env' (V x) env in
        check' f proc
    | C (f1, f2) -> if check' f1 env = check' f2 env then true else false
  in
  __s2 lam empty_env'
