
let rec pascal (x,y) =
if x=0||y=0||x=y then 1
else pascal(x-1,y) + pascal(x-1,y-1)

 