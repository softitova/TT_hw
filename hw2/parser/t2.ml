open Parser;;

print_string ("tests for lambda of string and string of lambda parsers\n");;

print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((xy)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\l.\\i.\\f.\\e.(l)(i)(f)(esgood)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z123))(\\x1.\\y1.((x1y1)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y145.((xy145)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z3445))(\\x.\\y.((xy)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x0.\\y1.x0y1")); print_string "\n";;


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
print_string("tests for alpha equivalence of two lambda functions\n");;

let t1 = (lambda_of_string "(x)", lambda_of_string "(x)");;
let t2 = (lambda_of_string "xy", lambda_of_string "xy");;
let t3 = (lambda_of_string "\\x.xy", lambda_of_string "\\y.yy");;
let t4 = (lambda_of_string "\\x.x", lambda_of_string "\\y.y");;
let t5 = (lambda_of_string "(\\x12.x12)(z1)", lambda_of_string "(\\y12.y12)(z1)");;
let t6 = (lambda_of_string "(\\x12.x12)(z881)", lambda_of_string "(\\y12.y12)(z881)");;
let t7 = (lambda_of_string "(\\x1234.x1234)(z122)", lambda_of_string "(\\y1.y1)(z122)");;
print_string (string_of_bool (is_alpha_equivalent (fst t1) (snd t1))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t2) (snd t2))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t3) (snd t3))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t4) (snd t4))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t5) (snd t5))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t6) (snd t6))); print_string "\n";; 
print_string (string_of_bool (is_alpha_equivalent (fst t7) (snd t7))); print_string "\n";; 

print_string("hard version of tests for alpha_equivalence\n")

let tester_eq tup =
	print_string 
		(string_of_bool						
			(is_alpha_equivalent
				(fst tup)
					(snd tup))); print_string "\n";;

let t1 = (lambda_of_string "(x)", lambda_of_string "(y)");;
let t2 = (lambda_of_string "xy", lambda_of_string "xy");;
let t3 = (lambda_of_string "\\x.xy", lambda_of_string "\\y.yy");;
let t4 = (lambda_of_string "\\x.x", lambda_of_string "\\y.y");;
let t5 = (lambda_of_string "(\\x.x)(z)(w)", lambda_of_string "(\\y.y)(z)(w)");;
let t6 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x1x2x3x4", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1y2y3y4");;
let t7 = (lambda_of_string "\\x1.\\x2.\\x3.\\x4.x4x2x3x1", lambda_of_string "\\y1.\\y2.\\y3.\\y4.y1y2y3y4");;

tester_eq t1;
tester_eq t2;
tester_eq t3;
tester_eq t4;
tester_eq t5;
tester_eq t6;
tester_eq t7;

(* tests for free substitution of variable to lambda *)
(*
print_string (string_of_bool(free (fst t1)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t2)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t3)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t1)("y"))); print_string "\n";;
print_string (string_of_bool(free (fst t4)("z"))); print_string "\n";;
print_string (string_of_bool(free (fst t4)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t5)("x"))); print_string "\n";;
print_string (string_of_bool(free (fst t5)("z"))); print_string "\n";;
*)

print_string("tests for free substitution\n");
let theta = lambda_of_string"z" ;;
let alpha = lambda_of_string"(x)(y)(\\x.x)(\\z.z)(\\w.vu)(\\a.b)";;
let var = "x";;
print_string (string_of_bool (free_subst theta alpha var )^"\n");;


let theta = lambda_of_string"a" ;;
let alpha = lambda_of_string"(\\a.b)";;
let var = "b";; 
print_string ((string_of_bool (free_subst theta alpha var))^"\n");;

