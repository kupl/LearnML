(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-6
  Homework-# : 1-1
  Excercise-Name : List-Merge
*)

let rec merge (li1, li2) = match (li1, li2) with
  | ([],[]) -> []
  | ([],li) -> li
  | (li,[]) -> li
  | ((h1::t1), (h2::t2)) -> (
    if h1 > h2 
    then (h1 :: (merge (t1, li2)))
    else (h2 :: (merge (li1, t2)))
  )
;;

(*
let rec merge2 = function
  | ([],[]) -> []
  | ([],li) -> li
  | (li,[]) -> li
  | (((h1::t1) as l1), ((h2::t2) as l2)) -> (
    if h1 > h2 
    then (h1 :: (merge2 (t1, l2)))
    else (h2 :: (merge2 (l1, t2)))
  )
;;
*)

