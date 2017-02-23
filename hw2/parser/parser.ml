type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let lambda_of_string s = 
	let s = s ^ ";" in
	let pos  = ref 0 in
	let get() = s.[!pos] in
	let next() = if !pos < String.length s - 1 then pos := !pos + 1 else failwith "oops" in
	let eat x  = if get() <> x then failwith "stop eating" else next() in

	let parse_ident_str()  = 
		let cur = String.make 1 (get ()) in
		next();
		cur in 
	
	let parse_ident() = 
	        Var(parse_ident_str()) in

	let rec parse_abs() = 
		eat '\\';
		let v = parse_ident_str () in
		eat '.';
		let l = parse_lambda() in
		Abs(v, l) 

	and  parse res  =  if ((!pos =  String.length s - 1) || ')' = get())
				then res else  App(res, parse_lambda()) 
	and parse_lambda() = 
		match (get()) with 
		'\\' -> (let res = parse_abs() in
				parse res)
		|'(' -> (eat '('; 
			let res = parse_lambda() in
			eat ')';
			parse res)
		|_   ->  (let res = parse_ident() in
				parse res) in
		
	parse_lambda();;

let string_of_lambda lambda = 
	let rec to_string lambda s = 
		match lambda with 
			| Var (x) -> s ^"("^x^")"
			| Abs (x, y) -> s^"(\\"^x^(to_string y "")^")"
			| App (x, y) -> s^"("^(to_string x "")^(to_string y "")^")" in
	to_string lambda "";;

let rec subst lambda oldVar newVar = 
	match lambda with 
	|Var(x) -> if (x = oldVar) then Var(newVar) else Var(x)
	|App(x, y) -> App(subst x oldVar newVar, subst y oldVar newVar) 
	|Abs(x, y) -> 
		if (x = oldVar) then Abs(x, y) else Abs(x, subst y oldVar newVar) 


let rec is_alpha_equivalent x y = 
	match (x, y) with
	|(Var xx, Var yy) -> (xx = yy)
	|(App(x1, x2), App(y1, y2)) ->
		((is_alpha_equivalent x1 y1) && (is_alpha_equivalent x2  y2))
	|(Abs(x1, x2), Abs(y1, y2)) -> 
		is_alpha_equivalent (subst x2 x1 "t") (subst y2 y1 "t")
	|_ -> false;;

let rec free lambda str  = 
	match lambda with
	|Var(x) -> true 
	|App(x, y) -> (free x str) && (free y str)
	|Abs(x, y) ->
		if (x = str) then false  else free y str;;

