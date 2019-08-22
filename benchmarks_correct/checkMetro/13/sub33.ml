type var = string
type lambda = V of var | P of var * lambda | C of lambda * lambda

let rec isStationIn (areas, n) =
  match areas with
  | [] -> false
  | hd::tl -> (hd = n) || (isStationIn (tl, n))

let rec check_real (areas, lambda) =
  match lambda with
  | P (a, m) -> check_real (a::areas, m)
  | V n -> isStationIn (areas, n)
  | C (a, b) -> check_real (areas, a) && check_real (areas, b)

let check lambda = check_real ([], lambda)
