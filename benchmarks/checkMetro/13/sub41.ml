type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro

and name = string

let checkMetro mtr =
  let rec check (mtr, area) =
    match mtr with 
    | STATION nm -> List.mem nm area
    | AREA (nm, _mtr) -> if List.mem nm area then check (_mtr, area) else check (_mtr, nm::area)
    | CONNECT (mtr1, mtr2) -> check (mtr1, area) && check(mtr2, area) in
  check (mtr, [])
