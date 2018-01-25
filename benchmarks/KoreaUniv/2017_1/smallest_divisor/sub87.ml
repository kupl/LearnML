let smallest_devisor n =
  let rec smde n b =
    if (n < b*b) then n
    else (if (n mod b == 0) then b
        else smde n (b+1))
    in smde n 2
