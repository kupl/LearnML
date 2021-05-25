let rec iter(n, f) = 
        let iden f(a) = a in
        let plus f g(a) = f(g(a)) in

        if n=0 then iden f
		else if n<0 then raise (Failure "iter")
        else if n=1 then f
        else plus f (iter((n-1), f))
