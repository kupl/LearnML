let rec sigma test first second = 
	if first > second then 0 else
	test (first) + sigma test (first+1) second;;


