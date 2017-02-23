open Parser;;

(* tests for lambda of string and string of lambda parsers*)

print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((xy)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\l.\\i.\\f.\\e.(l)(i)(f)(esgood)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "xy";;
lambda_of_string "(x)";;
lambda_of_string "(((((((\\y.y)))))))";;
lambda_of_string "((z))(\\x.\\y.((xy)))";;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "\\x.\\y.xy";;

(*helper functions for tuple created by T*)

let fst t =
	let (x, y) = t in
		x;;

let snd t =
	let (x, y) = t in
		y;;
(*tests for alpha equivalence of two lambda functions*)

let t1 = (lambda_of_string "(x)", lambda_of_string "(x)");;
let t2 = (lambda_of_string "xy", lambda_of_string "xy");;
let t3 = (lambda_of_string "\\x.xy", lambda_of_string "\\y.yy");;
let t4 = (lambda_of_string "\\x.x", lambda_of_string "\\y.y");;
let t5 = (lambda_of_string "(\\x.x)(z)", lambda_of_string "(\\y.y)(z)");;
print_string (string_of_bool (is_alpha_equivalent (fst t1) (snd t1))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t2) (snd t2))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t3) (snd t3))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t4) (snd t4))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t5) (snd t5))); print_string "\n";; 


(* tests for free substitution of variable to lambda *)

print_string (string_of_bool(free (fst t1)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t2)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t3)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t1)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t4)("z"))); print_string "\n";;
print_string (string_of_bool(free (fst t4)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t5)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t5)("z"))); print_string "\n";;





