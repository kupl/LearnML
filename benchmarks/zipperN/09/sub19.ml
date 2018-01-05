let rec zipperN l=
		match l with
			[] ->[]
			|h1::t1->
			match h1 with
				[]->zipperN t1
				|h2::t2->  h2::zipperN (t1@[t2])
		
		 
		
			
			
