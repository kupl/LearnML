let rec sigma f a b = if a = b then b else f b + sigma f a (b - 1)
