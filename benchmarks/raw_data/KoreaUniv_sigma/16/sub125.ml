let rec sigma (f : int -> int) a b = if a <= b then f a + sigma f (a+1) b
else 0;;
    