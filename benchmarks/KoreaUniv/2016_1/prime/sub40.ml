let rec prime : int -> bool = fun n -> let rec isprime(n,x) = if x==1 then true else if n mod x==0 then false else isprime(n,x-1) in if n<2 then false else isprime(n,n-1);;
