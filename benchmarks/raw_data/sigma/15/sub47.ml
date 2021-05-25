let rec sigma (a, b, n) =
        if (a < b) then n a + sigma((a+1), b, n)
        else if(a > b) then 0
        else n b ;;


