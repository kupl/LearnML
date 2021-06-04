exception NotImplemented

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 (__s2 : lambda) : string list =
  match __s2 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> __s8 != __s6) (__s1 __s7)
  | C (__s9, __s10) ->
      let __s11 : string list = __s1 __s9 in

      let __s12 : string list = __s1 __s10 in
      List.append __s11
        (List.filter (fun (__s13 : string) -> not (List.mem __s13 __s11)) __s12)


let rec check (lambda : lambda) : bool =
  match __s1 lambda with
  | [] -> true
  | _ :: _ -> false
  | _ -> raise NotImplemented
