let rec zipper (a, b) = match (a, b) with
   | _, [] -> a
   | [], _ -> b
   | first1 :: left1, first2 :: left2 ->
       if first1 < first2 then first1 :: zipper (left1, b) else first2 :: zipper (a, left2)
