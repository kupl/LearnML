let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match a with
  |[] -> b
  |hd::tl -> 
   (match b with
    |[] -> a
    |hd2::tl2 -> if hd+0 <hd2+0 then [hd]@[hd2]@(zipper(tl,tl2)) else [hd2]@[hd]@(zipper(tl, tl2)) );;
