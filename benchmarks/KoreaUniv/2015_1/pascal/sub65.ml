(*problem1*)
let rec pascal(x,y)=
	if y=0 || x=y then 1
	else pascal(x-1,y-1)+pascal(x-1,y)
