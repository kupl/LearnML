type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
 and var = string

let sta_chk : string list * string -> bool = fun (s_list, str) -> List.mem str s_list

let check : lambda -> bool = fun metr ->
  let rec check : lambda * string list -> bool = fun (lambda, sta_list) ->
  match lambda with
    P(sta, met) -> check(met,sta::sta_list)
  | C(met1, met2) -> check(met1, sta_list) && check(met2, sta_list)
  | V sta -> sta_chk(sta_list,sta) in
  check(metr,[])
