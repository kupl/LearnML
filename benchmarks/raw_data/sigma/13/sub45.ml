let rec sigma (a, b, f) =
        if a=b then f a
        else if a<b then f a + sigma (a+1, b, f) 
        else 0
