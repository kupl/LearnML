type name = string
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro

let rec check metro env =
  match metro with
  | STATION s ->
      (match env with
      | h::t -> if h=s then true else (check metro t)
      | [] -> false)
  | AREA (n, m) -> (check m (n::env))
  | CONNECT (m1, m2) -> (check m1 env) && (check m2 env)

let rec checkMetro metro =
  (let areal = [] in
  match metro with
  | STATION s -> false
  | _ -> (check metro areal))

