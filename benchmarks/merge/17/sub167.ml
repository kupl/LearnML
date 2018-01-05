let rec merge : (int list * int list -> int list) = function(x, y) -> (
  let rec merge_tail x y l = match (x, y) with
  | ([], []) -> l
  | (hd::tl, []) -> (merge_tail tl y (hd::l))
  | ([], hd2::tl2) -> (merge_tail y x l)
  | (hd::tl, hd2::tl2) -> if (hd > hd2) then (
      merge_tail tl y (hd::l)
    ) else merge_tail x tl2 (hd2::l)
  in List.rev (merge_tail x y [])
)

