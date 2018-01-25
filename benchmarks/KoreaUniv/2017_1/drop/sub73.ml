exception NO_NEGATIVE_INTEGERS

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
if n == 0 then l
else if n<0 then raise NO_NEGATIVE_INTEGERS
else
  match l with
  |[]->[]
  |hd::tl -> drop tl (n-1);;
