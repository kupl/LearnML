type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checklist ((l : string list), (s : string)) : bool =
  match l with [] -> false | h :: t -> if h = s then true else checklist (t, s)


let rec extend ((x : lambda), (env : string list)) : string list =
  match x with
  | V var -> if checklist (env, var) then env else var :: env
  | P (v, e) -> extend (e, env)
  | C (e1, e2) -> extend (e1, extend (e2, env))


let rec __s1 (__s2 : lambda) : string list =
  match __s2 with
  | V __s8 -> [ __s8 ]
  | P (__s9, __s10) ->
      List.filter (fun (__s11 : string) -> __s11 != __s9) (__s1 __s10)
  | C (__s12, __s13) ->
      let __s14 : string list = __s1 __s12 in

      let __s15 : string list = __s1 __s13 in
      List.append __s14
        (List.filter (fun (__s16 : string) -> not (List.mem __s16 __s14)) __s15)


let rec check (e : lambda) : bool = if __s1 e = [] then true else false
