(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-4.ml *)

type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let rec m_list ((arr : name list), (m : metro)) : bool =
  match m with
  | STATION n -> List.mem n arr
  | AREA (n, m_in) -> m_list (List.append arr [n], m_in)
  | CONNECT (m1, m2) -> (m_list (arr, m1)) && (m_list (arr, m2))

let checkMetro : metro -> bool = fun x ->
  let empty : name list = [] in
  m_list (empty, x)
