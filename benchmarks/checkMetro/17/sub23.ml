type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

module Name = struct
  type t = name
  let compare : name -> name -> int = compare
end

module MetroEnvSet = Set.Make(Name)

let rec checkMetro_with_env m env =
  match m with
  | STATION n -> MetroEnvSet.mem n env
  | AREA (n, m') -> checkMetro_with_env m' (MetroEnvSet.add n env)
  | CONNECT (m1, m2) -> checkMetro_with_env m1 env && checkMetro_with_env m2 env

let rec checkMetro m =
  checkMetro_with_env m MetroEnvSet.empty
