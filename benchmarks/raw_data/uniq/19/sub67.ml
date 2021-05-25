let uniq : 'a list -> 'a list
= fun lst -> let rec psh lt res =
  match lt with
    [] -> res
    |h::t -> let rec notcontain x l =
       match l with
         [] -> true
         |hh::tt -> x <> hh && notcontain x tt
      in if notcontain h res then psh t (res@[h]) else psh t res
  in psh lst [];;
  
uniq [5;6;5;4];;