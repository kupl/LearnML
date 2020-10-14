let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  let rec compose g f = fun x -> g(f(x)) in
  if n = 0 || n = 1 then f else compose f (iter(n-1,f));;


iter(3, fun x -> 2+x) 0;;
