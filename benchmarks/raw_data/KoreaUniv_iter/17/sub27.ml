(*problem 3*) let rec mul f n= fun a -> 
if n= 0 then  (fun x->x) a
else mul f (n-1) (f a)
  let iter: int*(int->int)->(int->int)=fun (n,f)->mul f n