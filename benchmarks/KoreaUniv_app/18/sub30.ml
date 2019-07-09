let rec fastrev : 'a list -> 'a list
= fun lst ->
  let rec f
  = fun a b ->
    match a with
      [] -> b
      | hd::tl -> f tl b@[hd]
      
  in 
  f lst [];;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec findf = fun l a ->
    match l with
      [] -> 0
      | hd::tl -> if hd=a then 1 else (findf tl a)
  in
  let rec make = fun a b ->
    match a with
      [] -> b
      | hd::tl -> if (findf b hd)=0 then make tl (hd::b) else make tl b
  in
  fastrev (make l1 (fastrev l2));;
  
app [4;5;6;7] [1;2;3;4];;