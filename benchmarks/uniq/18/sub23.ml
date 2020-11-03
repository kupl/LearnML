let rec uniq : 'a list -> 'a list
= fun lst ->
 match lst with
    | a :: (b :: _ as t) -> if a = b then uniq t else a :: uniq t
    | smaller -> smaller;;
  
  
uniq [1;2;3;3];;