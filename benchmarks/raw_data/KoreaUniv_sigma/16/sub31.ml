let rec sigma f a b =
 if a > b then 0
 else
 let result = f a in
 result + sigma f (a+1) b
