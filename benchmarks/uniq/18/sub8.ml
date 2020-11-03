(*let rec uniq : 'a list -> 'a list
= fun lst -> [] (* TODO *);; *)
let rec jonjaehae
= fun l a ->
  match l with
      [] -> false
    | hd::tl -> 
      if a=hd then true
      else jonjaehae tl a;;
      
let rec hab
= fun list1 list2->
  match list2 with
      [] -> list1
    | hd::tl -> 
      if (jonjaehae list1 hd = true) then hab list1 tl
      else hab (list1 @ [hd]) tl;;
        (**)
let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
      [] -> []
    | hd :: tl -> hab [hd] tl;;
uniq [4;8;5;4];;

