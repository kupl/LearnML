let rec duplicatedCheck 
= fun n l -> match l with
  |[] -> false
  |hd::tl -> if n=hd then true else duplicatedCheck n tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l2 with
  |[] -> l1
  |hd::tl -> if duplicatedCheck hd l1 then app l1 tl else hd::(app l1 tl);;
