let rec iter(n,f)=
        if n!=0 then fun x->iter((n-1),f)(f(x))
        else if n=0 then fun x->x
        else raise(Invalid_argument "1st argu must be equal to or bigger than 0!")
