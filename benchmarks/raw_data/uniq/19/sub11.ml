let rec dup l e = match l with
  |[]->false
  |hd::tl -> if (hd == e) then true else dup tl e;;

let uniq : 'a list -> 'a list
= fun lst -> let rec remove res lst = match lst with
  |[]->res
  |hd::tl -> if (dup res hd) == true then remove res tl
                                     else remove (res@[hd]) tl in remove [] lst;;