type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro: metro -> bool = function m ->
  let rec check: metro * (name list) -> bool = fun (m, nl) ->
    match m with
      | STATION(n) -> (List.exists (fun _n -> _n = n) nl)
      | AREA(n, _m) -> check(_m, n::nl)
      | CONNECT(m1, m2) -> check(m1, nl) && check(m2, nl)
  in check (m, [])
