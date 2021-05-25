let rec isUniq(* True if m is unique in lst  *)
= fun lst m -> match lst with 
  | [] -> true
  | hd::tl -> if hd=m then false else (isUniq tl  m);;

let rec uniq : 'a list -> 'a list
= fun lst -> let rec unique uniList givenList = match givenList with
  | [] -> uniList
  | hd::tl -> if (isUniq uniList hd) then unique (hd::uniList) tl else unique uniList tl 
  in unique [] lst;;


uniq [3;2;1;3;4];;


