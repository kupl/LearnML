let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec x = function
    | a :: (b :: _ as t) -> if a = b then x t else a :: x t
    | smaller -> smaller in
  x [] lst;;



uniq [5;6;5;4];;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec duplicates = function
    | a :: (b :: _ as y) -> if a = b then duplicates y else a :: duplicates y
    | z -> z in
  duplicates(l2@l1);;

app [4;5;6;7] [1;2;3;4];;