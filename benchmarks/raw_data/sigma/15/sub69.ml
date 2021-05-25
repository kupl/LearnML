let rec sigma(a,b,f)=
    if a = b then f(a)
    else if a < b then sigma(a+1,b,f) + f(a)
    else 0
