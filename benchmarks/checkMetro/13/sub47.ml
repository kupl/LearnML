type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkArea n e =
  match e with
    AREA(a, b) -> (checkArea (n @ [a]) b)
  | CONNECT(a, b) -> ((checkArea n a) && (checkArea n b))
  | STATION a -> List.exists (fun x -> x = a) n

let rec checkMetro e =
  match e with
    AREA(a, b) -> checkArea [a] b
  | CONNECT(a,b) -> (checkMetro a) && (checkMetro b)
  | STATION n -> true (* no works - default value *)
