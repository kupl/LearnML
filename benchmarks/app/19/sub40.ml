
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
let rec findelement n l=
  match l with
    |[]->[]
    |hd::tl -> if hd = n then [] else hd::(findelement n tl) in
 match l1 with 
   |[]->l2
   |hd::tl -> (findelement hd l2)@l1;;
 
