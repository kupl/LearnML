(*problem 1*)
let rec pascal(col, row)=
if row = 0 || col = row then 1
else pascal (col-1, row-1) + pascal (col-1,row);;


