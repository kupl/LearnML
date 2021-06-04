type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s4 (__s5 : lambda) (__s6 : var list) : bool =
  match __s5 with
  | V __s7 -> List.exists (fun (__s8 : string) -> __s8 = __s7) __s6
  | P (__s9, __s10) -> __s4 __s10 (__s9 :: __s6)
  | C (__s11, __s12) -> if __s4 __s11 __s6 then __s4 __s12 __s6 else false


let rec check (lam : lambda) : bool =
  let rec eval (lam : lambda) (ll : string list) : var list =
    match lam with
    | V v -> (
        match ll with
        | [] -> "false" :: ll
        | hd :: tl -> if hd = v then ll else hd :: eval (V v) tl )
    | P (v, l) ->
        let ll2 : string list = v :: ll in
        eval l ll2
    | C (l1, l2) -> eval l2 (eval l1 ll)
  in

  let rec compare (ll : string list) : bool =
    match ll with
    | [] -> true
    | hd :: tl -> if hd = "false" then false else compare tl
  in

  match lam with __s3 -> __s4 lam []
