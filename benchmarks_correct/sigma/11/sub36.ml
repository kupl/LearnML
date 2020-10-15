let rec sigma f a b =
        if b>a then f(b)+sigma f a (b-1)
        else if b=a then f(b)
        else raise(Invalid_argument "1st argu must be equal to or less than 2nd argu!")
