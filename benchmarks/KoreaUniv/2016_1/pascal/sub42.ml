 let rec pascal (n1, n2) =
   let x = n1 = n2 in
   match (n1, n2) with
   (n1, 0) -> 1
   |x -> 1
   |_ -> pascal (n1 -1, n2 - 1) + pascal (n1 -1, n2);;
