let rec sigma f a b =
if b > a then sigma f a (b-1) + f (b)
else f (a)