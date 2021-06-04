type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec varfind (arr : 'a list) x : bool =
  match arr with
  | [] -> false
  | hd :: tl -> if hd = x then true else varfind tl x


let rec find (arr : string list) (lam : lambda) : bool * string list =
  match lam with
  | V x -> (varfind arr x, arr)
  | P (x, l) ->
      let arr' : string list = x :: arr in
      find arr' l
  | C (l1, l2) ->
      let (b, arr') : bool * string list = find arr l1 in

      let (b2, arr'') : bool * string list = find arr' l2 in
      (b && b2, arr'')


let rec __s4 (__s5 : string list) (__s6 : lambda) : bool =
  match __s6 with
  | V __s7 -> List.mem __s7 __s5
  | P (__s8, __s9) -> __s4 (__s8 :: __s5) __s9
  | C (__s10, __s11) -> __s4 __s5 __s10 && __s4 __s5 __s11


let rec check (lam : lambda) : bool =
  let (b, a) : bool * string list = find [] lam in

  match lam with __s3 -> __s4 [] lam
