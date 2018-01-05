type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
 and name = string

let sta_chk : string list * string -> bool = fun (s_list, str) -> List.mem str s_list

let checkMetro : metro -> bool = fun metr ->
  let rec check : metro * string list -> bool = fun (metro, sta_list) ->
  match metro with
    AREA(sta, met) -> check(met,sta::sta_list)
  | CONNECT(met1, met2) -> check(met1, sta_list) && check(met2, sta_list)
  | STATION sta -> sta_chk(sta_list,sta) in
  check(metr,[])
