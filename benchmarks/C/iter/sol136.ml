let rec iter (n, f) a=
        if(n==0) then a
        else if(n>0) then
                f (iter((n-1), f) a)
        else a
;;
