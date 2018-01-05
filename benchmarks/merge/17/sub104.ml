let rec insert x xs =
  match xs with
  |[]-> x::[]
  |hd::tl -> if x>hd
             then x::xs
             else hd::(insert x tl)

let rec merge (l1, l2) =
  match l1 with
  |[] -> l2
  |hd::tl -> insert hd (merge (tl, l2))