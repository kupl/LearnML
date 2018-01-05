
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

module StringSet = Set.Make(String)

let checkMetro : metro -> bool = fun m ->
  let rec contains m s = (
    match m with
      | STATION n -> StringSet.mem n s
      | AREA (n, m0) -> contains m0 (StringSet.add n s)
      | CONNECT (m0, m1) -> (contains m0 s) && (contains m1 s)
  )
  in
    contains m StringSet.empty

