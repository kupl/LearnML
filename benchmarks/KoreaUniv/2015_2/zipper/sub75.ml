let rec zipper : int list * int list -> int list
=fun (a,b) ->match b with
      |[] -> a
      |hd::tl -> match a with
            |[] -> b
            |hd2::tl2 -> [hd2]@[hd]@zipper (tl2,tl);;

