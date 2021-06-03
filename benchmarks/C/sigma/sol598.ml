let rec sigma f x y=
    if y <x  then sigma f y x
    else if x==y then f x
    else f y + sigma f x (y-1);;
