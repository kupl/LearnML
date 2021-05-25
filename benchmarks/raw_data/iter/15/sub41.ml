let rec iter : (int * ('a->'a)) -> ('a->'a) = fun (n, f)->  (*val iter : int * ('a->'a) -> ('a->'a)*)
  let idf i = i in
  if n<0 then idf else if n==0 then idf else function x -> (iter (n-1, f)) (f x);;
