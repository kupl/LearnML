let rec sigma f a b =
  if (a>b) then 0
  else sigma f (a+1) b + (f a)

let test_func a = (a * 2)

