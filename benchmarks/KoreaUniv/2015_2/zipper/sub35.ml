let rec zipper : int list * int list -> int list
=fun (a,b) -> [] (* TODO *)
let rec zipper(a,b)=
  match a with
  |[] -> []
  |hd1::tl1->
    match b with
    |[]->[]
    |hd2::tl2::-> if hd1>hd2 then hd2::zipper(a,tl2) else hd1::zipper(tl1,b) ;; 

