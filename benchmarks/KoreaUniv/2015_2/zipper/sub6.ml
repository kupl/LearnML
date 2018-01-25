let rec zipper : int list * int list -> int list = 
  fun (a, b) ->
  match a with
  | [] -> b
  | hd :: tl ->
     match b with
     | [] -> a
     | hd2 :: tl2 ->
       if hd < hd2 then hd :: zipper(tl, b)
       else hd2 :: zipper(a, tl2);;

