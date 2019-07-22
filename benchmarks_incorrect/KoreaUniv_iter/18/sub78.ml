let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  let a f b= fun x-> f(b(x))in
  if n=0 then f else a f (iter(n-1,f));;
  iter(2,fun x->2+x)0;;