let rec sigma f a b = match a with b -> f a | _ -> sigma f (a + 1) b + f a
