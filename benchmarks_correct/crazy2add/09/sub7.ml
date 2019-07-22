exception Error of string;;
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;
let crazy2add ( ( a : crazy2 ), ( b : crazy2 ) ) =	
	let rec crazy2fulladd ( ( a : crazy2 ), ( b : crazy2 ), ( carry : crazy2 ) ) =
		let realAdder  ( ( h1 : crazy2 ), ( h2 : crazy2 ), ( carry : crazy2 ) ) =
			let getHead ( a : crazy2 ) =
				match a with
					NIL -> 0 |
					ZERO p -> 0 |
					ONE p -> 1 |
					MONE p -> -1 in
			
			let getTail ( a : crazy2 ) =
				match a with
					NIL -> NIL |
					ZERO p -> p |
					ONE p -> p |
					MONE p -> p in

			let trim( a : crazy2 ) =
				if ( a = ZERO( NIL ) ) then NIL
				else a in

			let recur ( ( h1 : crazy2 ), ( h2 : crazy2 ), ( carry : crazy2 ) ) =
				trim( crazy2fulladd( getTail( h1 ), getTail( h2 ), carry ) ) in

			match getHead( h1 ) + getHead( h2 ) + getHead( carry ) with
				-3 -> MONE( recur( h1, h2, MONE NIL ) ) |
				-2 -> ZERO( recur( h1, h2, MONE NIL ) ) |
				-1 -> MONE( recur( h1, h2, NIL ) ) |
				0 -> ZERO( recur( h1, h2, NIL ) ) |
				1 -> ONE( recur( h1, h2, NIL ) ) |
				2 -> ZERO( recur( h1, h2, ONE NIL ) ) |
				3 -> ONE( recur( h1, h2, ONE NIL ) ) |
				_ -> raise ( Error "Invalid Number!!" ) in

		match ( a, b, carry ) with
			( NIL, NIL, NIL ) -> NIL |
			( _, _, _ ) -> realAdder( a, b, carry ) in

	if( a = NIL || b = NIL ) then raise ( Error "All Crazy2 Should be non-NIL!" )
	else crazy2fulladd( a, b, ZERO NIL );;
