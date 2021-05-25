(*As the exercise was formulated, I assumed it is guaranteed that there are no duplicates within the two lists.
* Otherwise I would also run the function uniq from the next exercise on the lists.*)

let rec isin l1 a = 
  match l1 with 
    |[] -> false 
    |hd::tl -> if a = hd then true else isin tl a
;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with 
    |[] -> l2
    |hd::tl -> 
      if isin l2 hd then app tl l2 else 
      app tl (l2@[hd])

;;


app [4;5;6;7] [1;2;3;4];;