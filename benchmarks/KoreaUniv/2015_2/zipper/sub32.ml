let rec zipper ((l1 : int list), (l2 : int list)) = match l1 with
  | [] -> l2
  | h1 :: t1 -> match l2 with
                  |[] -> l1
                  |h2 :: t2 -> if (h1 < h2) then h1 :: (zipper (t1, l2))
                              else h2 :: (zipper (l1, t2));;

