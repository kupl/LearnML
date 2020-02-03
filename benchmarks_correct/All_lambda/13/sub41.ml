type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let check mtr =
  let rec check2 (mtr, area) =
    match mtr with 
    | V nm -> List.mem nm area
    | P (nm, _mtr) -> if List.mem nm area then check2 (_mtr, area) else check2 (_mtr, nm::area)
    | C (mtr1, mtr2) -> check2 (mtr1, area) && check2(mtr2, area) in
  check2 (mtr, [])
