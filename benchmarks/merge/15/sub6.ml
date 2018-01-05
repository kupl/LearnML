(*
    PL 1-1
    2008-11609 박성원
*)

let rec merge (list1, list2) =
  match (list1, list2) with
  | (list1, []) -> list1
  | ([], list2) -> list2
  | (v1 :: sublist1, v2 :: sublist2) ->
      if v1 > v2
        then v1 :: (merge (sublist1, list2))
        else v2 :: (merge (list1, sublist2))
;;
