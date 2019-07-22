type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec is_mem : name list -> name -> bool
= fun names name ->
  match names with
  | [] -> false
  | hd::tl -> if (hd = name) then true else is_mem tl name

let rec sub_checkMetro : metro -> name list -> bool
= fun met names ->
  match met with
  | STATION n -> is_mem names n
  | AREA (n, m) -> sub_checkMetro m (n::names)
  | CONNECT (m1, m2) -> (sub_checkMetro m1 names) && (sub_checkMetro m2 names)

let rec checkMetro : metro -> bool
= fun met ->
  sub_checkMetro met []