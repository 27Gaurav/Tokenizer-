type token = Var of string | Int_Const of int | Keyword of string | Str_Const of string | 
             Bool_OP of string | Int_OP of string | Bool_Const of bool | Str_OP of string |
             Lparen | Rparen | Comma | Comp_OP of string | EOF | String_Concat 
;;

let is_alpha n  = match n with 
  | 'a' .. 'z'  -> true 
  | _ -> false

let is_alphanum n =
  match n with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true 
  | _ -> false

let is_digit n = match n with 
  | '0'..'9' -> true | _ -> false 

let char_to_string c = String.make 1 c

let check_var str index =
  let rec scan_var value index =
    if index < String.length str && is_alphanum str.[index] then scan_var (value ^ char_to_string str.[index]) (index + 1)
    else (value, index)
  in
  scan_var (char_to_string str.[index]) (index + 1)

let tokenise str =
  let rec tokenise' str index =
    if index >= String.length str then
      [EOF]
    else
      match str.[index] with
        ' ' | '\t' | '\n' -> tokenise' str (index + 1)
      |'(' -> Lparen :: tokenise' str (index + 1)
      |')' -> Rparen :: tokenise' str (index + 1)
      | ',' -> Comma :: tokenise' str (index + 1)
      | '+' | '-' | '*' | '/' -> Int_OP (char_to_string str.[index]) :: tokenise' str (index + 1)
      | '=' | '<' | '>' -> Comp_OP (char_to_string str.[index]) :: tokenise' str (index + 1)
      | '&' | '|' | '!' -> Bool_OP (char_to_string str.[index]) :: tokenise' str (index + 1)
      | '"' -> 
          let rec check_string_const value index = 
            if index + 1 < String.length str && str.[index + 1] <> '"' then
              check_string_const (value ^ char_to_string str.[index]) (index + 1)
            else if index + 1 < String.length str then
              (value ^ char_to_string str.[index], index + 2)
            else
              failwith "string constant Unterminated"
          in
          let (s, index1) = check_string_const "" (index + 1) in
          Str_Const s :: tokenise' str index1
      | '^' -> String_Concat  :: tokenise' str (index + 1)
      | _ ->
          if is_alpha str.[index] || str.[index] = '_'  then
            let (exp, index1) = check_var str index in
            match exp with
            | "if" | "then" | "else" | "let" -> Keyword exp :: tokenise' str index1
            | "true" -> Bool_Const true :: tokenise' str index1
            | "false" -> Bool_Const false :: tokenise' str index1
            | "tuple" -> Keyword exp :: tokenise' str index1
            | _ -> Var exp :: tokenise' str index1
          else if is_digit str.[index] then
            let rec check_eval_int value idx =
              if idx < String.length str && is_digit str.[idx] then
                check_eval_int (value * 10 + int_of_string (char_to_string str.[idx]) ) (idx + 1)
              else
                (value, idx)
            in
            let (num, index1) = check_eval_int (int_of_string (char_to_string str.[index]) ) (index + 1) in
            Int_Const num :: tokenise' str index1
          else
            failwith ("Error : " ^ char_to_string str.[index]^ " can't be tokenised")
  in
  tokenise' str 0

 (*
test cases:

tokenise "x * y + z" ;;

- : token list = [Var "x"; Int_OP "*"; Var "y"; Int_OP "+"; Var "z"; EOF]


tokenise "if x > 0 && y < 10 then \"positive\" else \"negative\"" ;;

- : token list =
[Keyword "if"; Var "x"; Comp_OP ">"; Int_Const 0; Bool_OP "&"; Bool_OP "&";
Var "y"; Comp_OP "<"; Int_Const 10; Keyword "then"; Str_Const "positive";
Keyword "else"; Str_Const "negative"; EOF]


tokenise  "a' + b_" ;;

- : token list = [Var "a\\'"; Int_OP "+"; Var "b_"; EOF]


tokenise "x = 5 || y = 10" ;;

- : token list =
[Var "x"; Comp_OP "="; Int_Const 5; Bool_OP "|"; Bool_OP "|"; Var "y";
Comp_OP "="; Int_Const 10; EOF]


tokenise "\"one, two, three\", \"four, five, six\"" ;;

- : token list =
[Str_Const "one, two, three"; Comma; Str_Const "four, five, six"; EOF]


tokenise "(x + y) * (x - y)" ;;

- : token list =
[Lparen; Var "x"; Int_OP "+"; Var "y"; Rparen; Int_OP "*"; Lparen; Var "x";
Int_OP "-"; Var "y"; Rparen; EOF]


tokenise "x = y || x = z && y = z" ;;

- : token list =
[Var "x"; Comp_OP "="; Var "y"; Bool_OP "|"; Bool_OP "|"; Var "x";
Comp_OP "="; Var "z"; Bool_OP "&"; Bool_OP "&"; Var "y"; Comp_OP "=";
Var "z"; EOF]

tokenise "Abraham = 10" ;;

Exception: (Failure "Error : A can't be tokenised")


tokenise "\"abc\" ^ \"def\"" ;;

- : token list = [Str_Const "abc"; String_Concat; Str_Const "def"; EOF]

tokenise "true && (x > 0) || (y <= 10) && false" ;;

- : token list =
[Bool_Const true; Bool_OP "&"; Bool_OP "&"; Lparen; Var "x"; Comp_OP ">";
Int_Const 0; Rparen; Bool_OP "|"; Bool_OP "|"; Lparen; Var "y"; Comp_OP "<";
Comp_OP "="; Int_Const 10; Rparen; Bool_OP "&"; Bool_OP "&";
Bool_Const false; EOF]


tokenise "x == 10 && y != \"hello\"" ;;

- : token list =
[Var "x"; Comp_OP "="; Comp_OP "="; Int_Const 10; Bool_OP "&"; Bool_OP "&";
Var "y"; Bool_OP "!"; Comp_OP "="; Str_Const "hello"; EOF]






                      *)
