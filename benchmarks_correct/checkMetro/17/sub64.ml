(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-4.ml *)

type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let rec m_list ((arr : var list), (m : lambda)) : bool =
  match m with
  | V n -> List.mem n arr
  | P (n, m_in) -> m_list (List.append arr [n], m_in)
  | C (m1, m2) -> (m_list (arr, m1)) && (m_list (arr, m2))

let check : lambda -> bool = fun x ->
  let empty : var list = [] in
  m_list (empty, x)
