let rec fac n m =
if n<m then fac n (m-1) * m
else n

let rec makit (a, b) =
if b=0 then 1
else if b=1 then a
else fac (a-b+1) a / fac 1 b

let rec pascal (n1, n2) =
if n1-n2<n2 then makit (n1, n1-n2)
else makit (n1, n2)
