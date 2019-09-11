type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let check mtr =
  let rec check (mtr, area) =
    match mtr with 
    | V nm -> List.mem nm area
    | P (nm, _mtr) -> if List.mem nm area then check (_mtr, area) else check (_mtr, nm::area)
    | C (mtr1, mtr2) -> check (mtr1, area) && check(mtr2, area) in
  check (mtr, [])
