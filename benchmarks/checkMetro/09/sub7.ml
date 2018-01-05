type metro = STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
		and name = string;;

let checkMetro ( m : metro ) =
	let module NameSet = Set.Make ( struct
				type t = name
				let compare = compare
		 	    end) in
	let rec checker m = 
		match m with
			STATION( id ) -> NameSet.add id NameSet.empty |
			AREA( id, ch ) -> NameSet.remove id ( checker( ch ) ) |
			CONNECT( ch1, ch2 ) -> NameSet.union ( checker( ch1 ) ) ( checker( ch2 ) ) in
	NameSet.is_empty ( checker( m ) );;