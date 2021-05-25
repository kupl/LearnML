let rec sigma(a,b,f)=
        if b>a then f(b)+sigma(a,b-1,f)
        else if b=a then f(b)
        else raise(Invalid_argument "1st argu must be equal to or less than 2nd argu!")
