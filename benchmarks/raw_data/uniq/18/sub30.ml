let rec fastrev : 'a list -> 'a list
= fun lst ->
  let rec f
  = fun a c->
    match a with
      [] -> c
      | hd::tl -> f tl (hd::c)
  in 
  f lst [];;  


let rec uniq : 'a list -> 'a list
= fun lst -> 
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
  fastrev (make lst []);;
  
  

uniq [5;6;5;4];;
    
