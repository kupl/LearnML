  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec free_var e = match e with
  | V(v) -> [v]
  | P(v, t) -> List.filter (fun x -> x <> v) (free_var t)
  | C(t, u) ->
      let f_t = free_var t in
      let f_u = free_var u in
        List.append f_t (List.filter (fun x -> not (List.mem x f_t)) f_u)

  let check : exp -> bool
  =fun e -> match (free_var e) with
  |[] -> true
  |_::_ -> false
  ;;
