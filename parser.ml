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

lambda_of_string("xy");;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "xy";;
lambda_of_string "(x)";;
(*lambda_of_string "()";;*) 
lambda_of_string "(((((((\\y.y)))))))";;
lambda_of_string "((z))(\\x.\\y.((xy)))";;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "\\x.\\y.xy";;
