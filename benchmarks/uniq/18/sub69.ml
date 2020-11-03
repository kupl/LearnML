let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec dupl x l= 
    match l with
      |[] -> []
      |hd::tl -> if hd=x then dupl x tl else [hd]@dupl x tl
  in
  match lst with
    |[] -> []
    |hd::tl -> [hd]@uniq(dupl hd tl)
;;

uniq [5;6;5;4];;