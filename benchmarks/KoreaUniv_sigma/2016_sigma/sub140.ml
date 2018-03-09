let rec sigma f a b =
 if a == b then f a
 else sigma f a (b-1) + f b;;