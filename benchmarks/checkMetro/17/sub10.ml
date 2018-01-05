(* Homework 2 - Exercise 4
 * 2011-10492 Jaeyeong Yang *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro: metro -> bool = fun m ->
  let rec auc: metro * name list -> bool = fun (mm, env) ->
    match mm with
    | STATION n -> List.mem n env
    | AREA (n, ml) -> auc (ml, n :: env)
    | CONNECT (m1, m2) -> auc (m1, env) && auc (m2, env)
  in
  auc (m, [])
