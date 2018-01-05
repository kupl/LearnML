(*
  CSE/2015-21233/김종권
  Homework 2-3
*)
type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec checkMetro' m l =
  match m with
  | STATION name ->
    List.exists (fun x -> x = name) l
  | AREA (name, m') ->
    checkMetro' m' (name :: l)
  | CONNECT (m1, m2) ->
    (checkMetro' m1 l) && (checkMetro' m2 l)
      
let checkMetro m =
  checkMetro' m []

