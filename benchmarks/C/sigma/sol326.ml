let rec sigma n a b  =
        if (a < b) then n a + sigma n (a+1) b
        else if(a > b) then 0
        else n b ;;


