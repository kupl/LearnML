(*problem 7*)
  let rec alist l=
  match l with
  |[]->[]
  |(a,b)::tl->[a]@alist tl
  let rec blist ll=
  match ll with
  |[]->[]
  |(a,b)::tl->[b]@blist tl
  let unzip:('a*'b)list->'a list * 'b list
  =fun lst->
  (alist lst, blist lst);;