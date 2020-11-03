let rec dup l e = match l with
  |[]->false
  |hd::tl -> if (hd == e) then true else dup tl e;;

let app l1 l2 = let rec create l1 l2 = match l1 with
  |[]->l2
  |hd::tl-> if (dup l2 hd) == true then create tl l2
                                   else create tl (l2@[hd]) in create l1 l2;;