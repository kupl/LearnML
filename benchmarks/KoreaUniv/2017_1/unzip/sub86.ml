
(*problem7*)
let unzip :('a *'b) list -> 'a list *'b list =fun lst ->
let rec un1 l1=
match l1 with
|[]->[]
|hd::tl -> (match hd with (x,_)->x)::(un1 tl)
  in let rec un2 l2 =
  match l2 with 
  |[]->[]
  |hd::tl ->(match hd with (_,x)->x)::(un2 tl)
  in ((un1 lst),(un2 lst));;
