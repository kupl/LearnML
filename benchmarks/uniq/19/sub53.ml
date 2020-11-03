let rec checklst : 'a -> 'a list -> bool
= fun a lst -> match lst with
  |[] -> true
  |hd::tl -> if hd = a then false
            else checklst a tl;;
            
let rec makelst : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l2 with
  |[] -> []
  |hd::tl -> if checklst hd l1 = true then makelst hd::l1 tl
              else makelst l1 tl;;
  
let uniq : 'a list -> 'a list
= fun lst -> makelst [] lst;;

uniq [5;6;5;4];;