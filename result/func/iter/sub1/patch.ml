exception Error of string

let __s1 (__s2 : int -> int) (__s3 : int -> int) (__s4 : int) : int =
  __s2 (__s3 __s4)


let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  let identity x = x in

  let rec ff x = f (f x) in
  if n = 0 then fun (__s8 : int) -> __s8 else __s1 f (iter (n - 1, f))
