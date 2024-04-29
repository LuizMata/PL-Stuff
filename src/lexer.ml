open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let len = String.length input in 
  let rec tok pos = 
    if pos >= len then []
    else if Str.string_match (Str.regexp "[0-9]+\\|(-[0-9]+)") input pos then
      let value = Str.matched_string input in
      if String.contains value '(' then
        let stripped_value = String.sub value 1 (String.length value - 2) in
        Tok_Int (int_of_string stripped_value) :: tok (pos + String.length value) 
      else
        Tok_Int (int_of_string value) :: tok (pos + String.length value) 
    else if Str.string_match (Str.regexp "(") input pos then 
      Tok_LParen :: (tok (pos + 1))
    else if Str.string_match (Str.regexp ")") input pos then 
      Tok_RParen :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "{") input pos then 
      Tok_LCurly :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "}") input pos then 
      Tok_RCurly :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "\\.") input pos then 
      Tok_Dot :: (tok (pos + 1))
    else if Str.string_match (Str.regexp ">=") input pos then 
      Tok_GreaterEqual :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "<=") input pos then 
      Tok_LessEqual :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "<>") input pos then 
      Tok_NotEqual :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "=") input pos then 
      Tok_Equal :: (tok (pos + 1))
    else if Str.string_match (Str.regexp ">") input pos then 
      Tok_Greater :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "<") input pos then 
      Tok_Less :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "||") input pos then 
      Tok_Or :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "&&") input pos then 
      Tok_And :: (tok (pos + 2))
    else if Str.string_match (Str.regexp ";;") input pos then 
      Tok_DoubleSemi :: (tok (pos + 2))
    else if Str.string_match (Str.regexp ";") input pos then 
      Tok_Semi :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "true\\|false") input pos then
      let value = Str.matched_string input in 
      match value with
      | "true" -> Tok_Bool true :: tok (pos + String.length value)
      | "false" -> Tok_Bool false :: tok (pos + String.length value)
      | _ -> raise (InvalidInputException "bool failed")
    else if Str.string_match (Str.regexp "not\\b") input pos then 
      Tok_Not :: (tok (pos + 3))
    else if Str.string_match (Str.regexp "if\\b") input pos then 
      Tok_If :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "then\\b") input pos then 
      Tok_Then :: (tok (pos + 4))
    else if Str.string_match (Str.regexp "else\\b") input pos then 
      Tok_Else :: (tok (pos + 4))
    else if Str.string_match (Str.regexp "let\\b") input pos then 
      Tok_Let :: (tok (pos + 3))
    else if Str.string_match (Str.regexp "def\\b") input pos then 
      Tok_Def :: (tok (pos + 3))
    else if Str.string_match (Str.regexp "in\\b") input pos then 
      Tok_In :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "rec\\b") input pos then 
      Tok_Rec :: (tok (pos + 3))
    else if Str.string_match (Str.regexp "fun\\b") input pos then 
      Tok_Fun :: (tok (pos + 3))
    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then 
      let value = Str.matched_string input in
      let length2 = String.length value in 
      Tok_ID value :: (tok (pos + length2))
    else if Str.string_match (Str.regexp "+") input pos then 
      Tok_Add :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "->") input pos then 
      Tok_Arrow :: (tok (pos + 2))
    else if Str.string_match (Str.regexp "-") input pos then 
      Tok_Sub :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "\\*") input pos then 
      Tok_Mult :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "/") input pos then 
      Tok_Div :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "\\^") input pos then 
      Tok_Concat :: (tok (pos + 1))
    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then 
      let value = Str.matched_group 0 input in 
      let processed = String.sub value 1 (String.length value - 2) in
      Tok_String processed :: tok (pos + String.length value)
    else if Str.string_match (Str.regexp "[ ]+") input pos then 
      let value = Str.matched_string input in 
      tok (pos + String.length value)
    else if Str.string_match (Str.regexp "[\t]+") input pos then 
      let value = Str.matched_string input in 
      tok (pos + String.length value)
    else if Str.string_match (Str.regexp "[\n]+") input pos then 
      let value = Str.matched_string input in 
      tok (pos + String.length value)
    else raise(InvalidInputException "invalid input character")
  in tok 0