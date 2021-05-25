

let rec uniq : 'a list -> 'a list
= fun lst ->
 match lst with
    | a :: (b :: t) -> if a = b then uniq t else a :: uniq t
    | smaller -> smaller;;
    
    let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  
  uniq(l1@l2);;