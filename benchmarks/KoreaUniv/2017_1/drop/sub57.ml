(*problem 6*)
  let rec drop l n = match l with
                    | []->[]
                    | hd::tl -> if n=0 then l
                                else (drop tl (n-1));;