let rec zipper (l1, l2 : int list * int list) =
  match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (h1 :: t1, h2 :: t2) -> h1 :: h2 :: zipper  (t1, t2);;
 