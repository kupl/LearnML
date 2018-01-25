let reduce_list l=
match l with 
|[]->[]
|hd::tl->tl;;

let list_first l=
match l with
|[]->raise(Failure "too much")
|hd::tl->hd;;

let rec length l=
match l with
|[]->0
|hd::tl->1+(length tl);;

let rec change : int list->int->int
=fun coins amount->
if(amount>0) then
  if((length coins)>0) then
    if((amount-(list_first coins))>0) then
    ( change coins (amount-(list_first coins)))+(change (reduce_list coins) amount)else
    if((amount-(list_first coins))<0) then 0
     else 1
else 0
else
  if(amount<0) then 0
  else 1;;
