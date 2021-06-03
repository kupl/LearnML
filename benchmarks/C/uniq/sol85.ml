
let rec uniq : 'a list -> 'a list
= fun lst ->
 let rec findelement n l =
   match l with
     |[]->[]
     |hd::tl -> if hd = n then findelement n tl else hd::(findelement n tl) in
match lst with
  |[]->[]
  |hd::tl -> hd::uniq(findelement hd tl) ;;

  
