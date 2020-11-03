let rec cmp
= fun x lst -> match lst with
  |[] -> []
  |hd::tl -> if x = hd then cmp x tl else hd::(cmp x tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
              hd::tl -> hd::uniq (cmp hd tl)
              |[] -> [];; (* TODO *)


uniq [5;6;5;4;9;6;10;5];;