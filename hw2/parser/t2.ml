open Parser;;
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
(*lambda_of_string "()";;*)
lambda_of_string "(((((((\\y.y)))))))";;
lambda_of_string "((z))(\\x.\\y.((xy)))";;
lambda_of_string "\\x.\\y.xy";;
lambda_of_string "\\x.\\y.xy";;
