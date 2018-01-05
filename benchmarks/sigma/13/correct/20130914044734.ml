let rec sigma(sn,en,f)=
	if sn>en then 0
    else if sn=en then (f sn)
    else (f en) + (sigma(sn,(en-1),f))