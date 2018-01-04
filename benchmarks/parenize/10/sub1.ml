type team =
  Korea
| Portugal
| Brazil

type tourna =
  LEAF of team
| NODE of tourna * tourna

let rec parenize x =
match x with
  LEAF leafname -> leafname
| NODE (left, right) -> "(" ^ parenize(left) ^ " " ^ parenize(right) ^ ")"
;;