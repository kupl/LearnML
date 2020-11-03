let rec fold f l a  = 
  match l with
    | []-> a
    | hd::tl -> 
      fold f tl (f a hd)


and is_include: 'a -> 'a list -> bool
= fun a li ->
	match li with
	|[]-> false
	|h::t -> if a = h then true else (is_include a t) 

and funct:  'a list -> 'a ->  'a list
= fun a b ->
  if is_include  b a
    then a
    else a @ [b]


and uniq l = (fold funct l []);;

(*(uniq [5;6;5;4;5;10] );;*)
(*uniq [];;*)


