type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check2 ((m : lambda), (l : string list)) : bool =
  match m with
  | P (nam, met) -> check2 (met, nam :: l)
  | V nam ->
      let x : string = List.find (fun (elem : string) -> elem = nam) l in
      true
  | C (met1, met2) -> check2 (met1, l) && check2 (met2, l)


let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match __s4 with
  | V __s8 -> (
      match __s5 with
      | [] -> false
      | __s9 :: __s10 -> if __s8 = __s9 then true else __s3 (V __s8, __s10) )
  | P (__s11, __s12) -> __s3 (__s12, __s11 :: __s5)
  | C (__s13, __s14) ->
      if __s3 (__s13, __s5) = true && __s3 (__s14, __s5) = true then true
      else false


let check (m : lambda) : bool = __s3 (m, [])
