type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec containCheck (n, ll) : bool =
  match ll with
  |hd::tl ->
    if String.equal hd n then true else containCheck(n,tl)
  |[] -> false

let checkMetro metro : bool =
  let rec check (metro,ll) : bool =
    match metro with
    |STATION n -> containCheck(n,ll)
    |AREA (n,met) -> check (met,n::ll)
    |CONNECT (met1,met2) -> check (met1,ll) && check(met2,ll)
  in check(metro,[])
