let prime n=
    let rec recursive a b=
    if a=1 || a<0 then false
    (* 1과 음수는 소수가 아니다*)
     else if b=1 then true
      else if a mod b=0 then false
      else recursive a (b-1)
       in
       recursive n (n-1);;
