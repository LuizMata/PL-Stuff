open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
          (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
  let (t,e) = parse_Expr toks in
  if t <> [] then raise (InvalidInputException "Parse Error") else (t,e)
and parse_Expr toks = match lookahead toks with
|Some Tok_Let -> parse_LetExpr toks
|Some Tok_If -> parse_IfExpr toks
|Some Tok_Fun -> parse_FunctionExpr toks
|_ -> parse_OrExpr toks

and parse_LetExpr toks =     
  let t1 = match_token toks Tok_Let in
  let (t2, is_rec) = match lookahead t1 with 
  |Some Tok_Rec -> ((match_token t1 Tok_Rec), true) 
  |_ -> (t1, false)
  in
  let (t3, id) = match lookahead t2 with
  |Some Tok_ID x -> ((match_token t2 (Tok_ID x)), x)
  |_ -> raise (InvalidInputException "LetExpr Error")
  in
  let t4 = match_token t3 Tok_Equal in
  let (t5, e1) = parse_Expr t4  in
  let t6 = match_token t5 Tok_In in 
  let (t7, e2) = parse_Expr t6 in
  (t7, Let(id, is_rec, e1, e2 ))

and parse_FunctionExpr toks =
  let t1 = match_token toks Tok_Fun in
  let (t2, id) = match lookahead t1 with
  |Some Tok_ID x -> ((match_token t1 (Tok_ID x)), x)
  |_ -> raise (InvalidInputException "Parse_FunctionExpr Error")
  in
  let t3 = match_token t2 Tok_Arrow in
  let (t4,e1) = parse_Expr t3 in 
  (t4, Fun(id,e1))

and parse_IfExpr toks = 
  match lookahead toks with
  |Some Tok_If ->
  let t1 = match_token toks Tok_If in 
  let (t2, e1) = parse_Expr t1 in 
  let t3 = match_token t2 Tok_Then in
  let (t4, e2) = parse_Expr t3 in 
  let t5 = match_token t4 Tok_Else in
  let (t6, e3) = parse_Expr t5 in (t6, If(e1,e2,e3))
  |_-> raise (InvalidInputException "Parse_IfExpr Error")

and parse_OrExpr toks = 
  let (t1,e1) = parse_AndExpr toks in
  match lookahead t1 with
  |Some Tok_Or -> let t2 = match_token t1 Tok_Or in 
    let (t3,e2) = parse_OrExpr t2 in (t3, Binop(Or, e1,e2))
  |_ -> (t1,e1)
  
and parse_AndExpr toks = 
  let (t1,e1) = parse_EqualityExpr toks in
  match lookahead t1 with 
  |Some Tok_And -> let t2 =  match_token t1 Tok_And in 
      let (t3,e2) = parse_AndExpr t2 in (t3, Binop(And, e1, e2))
  |_ -> (t1,e1)


and parse_EqualityExpr toks = 
  let (t1,e1) = parse_RelationalExpr toks in 
  match lookahead t1 with
  |Some Tok_NotEqual -> let t2 = match_token t1 Tok_NotEqual in 
    let (t3,e2) = parse_EqualityExpr t2 in (t3, Binop(NotEqual,e1,e2))
  |Some Tok_Equal -> let  t2 = match_token t1 Tok_Equal in
    let (t3,e2) = parse_EqualityExpr t2 in (t3, Binop(Equal, e1,e2))
  |_ -> (t1,e1)

and parse_RelationalExpr toks =
  let (t1,e1) = parse_AdditiveExpr toks in 
  match lookahead t1 with
  |Some Tok_Less -> let t2 = match_token t1 Tok_Less in 
    let (t3,e2) = parse_RelationalExpr t2 in (t3, Binop(Less,e1,e2)) 
  |Some Tok_LessEqual -> let t2 = match_token t1 Tok_LessEqual in 
    let (t3,e2) = parse_RelationalExpr t2 in (t3, Binop(LessEqual,e1,e2)) 
  |Some Tok_Greater -> let t2 = match_token t1 Tok_Greater in 
    let (t3,e2) = parse_RelationalExpr t2 in (t3, Binop(Greater,e1,e2)) 
  |Some Tok_GreaterEqual -> let t2 = match_token t1 Tok_GreaterEqual in 
    let (t3,e2) = parse_RelationalExpr t2 in (t3, Binop(GreaterEqual,e1,e2)) 
  |_ -> (t1,e1)

and parse_AdditiveExpr toks = 
  let (t1,e1) = parse_MultiplicativeExpr toks in
  match lookahead t1 with 
  |Some Tok_Add -> let t2 = match_token t1 Tok_Add in 
    let (t3, e2) = parse_AdditiveExpr t2 in (t3, Binop(Add, e1, e2))
  |Some Tok_Sub -> let t2 = match_token t1 Tok_Sub in 
    let (t3, e2) = parse_AdditiveExpr t2 in (t3, Binop(Sub, e1, e2))
  |_ -> (t1,e1) 

and parse_MultiplicativeExpr toks =
  let (t1,e1) = parse_ConcatExpr toks in 
  match lookahead t1 with 
  |Some Tok_Mult -> let t2 = match_token t1 Tok_Mult in 
    let (t3,e2) = parse_MultiplicativeExpr t2 in (t3, Binop(Mult,e1,e2)) 
  |Some Tok_Div -> let t2 = match_token t1 Tok_Div in 
    let (t3,e2) = parse_MultiplicativeExpr t2 in (t3, Binop(Div,e1,e2)) 
  |_ -> (t1,e1)

and parse_ConcatExpr toks =
  let (t1,e1) = parse_UnaryExpr toks in
  match lookahead t1 with 
  |Some Tok_Concat -> let t2 = match_token t1 Tok_Concat in
    let (t3,e2) = parse_ConcatExpr t2 in (t3, Binop(Concat,e1,e2))
  |_ -> (t1,e1)
  
and parse_UnaryExpr toks = 
  match lookahead toks with 
  |Some Tok_Not -> let t1 = match_token toks Tok_Not in 
    let (t2, e1) = parse_UnaryExpr t1 in (t2, Not(e1))
  |_ -> let (t1,e1) = parse_AppExpr toks in (t1,e1)
  
and parse_AppExpr toks = 
  let (t1, e1) = parse_SelectExpr toks in
  match lookahead t1 with
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly) -> 
    let (t2, e2) = parse_PrimaryExpr t1 in
    (t2, App (e1, e2))
    | _ -> (t1, e1)

  and parse_SelectExpr toks = 
    let (t1, e1) = parse_PrimaryExpr toks in 
    match lookahead t1 with 
    | Some Tok_Dot -> let t2 = match_token t1 Tok_Dot in 
      let (t3, name) = 
      match lookahead t2 with 
      | Some (Tok_ID id) -> (match_token t2 (Tok_ID id), id)
      | _-> raise (InvalidInputException "Parse_SelectExpr Failed!")
      in (t3, Select (Lab(name), e1))
    | _ -> (t1, e1) 
  
  and parse_PrimaryExpr toks = 
    match lookahead toks with
    | Some Tok_Int a -> let t1 = match_token toks (Tok_Int a) in (t1, (Int a))
    | Some Tok_Bool b -> let t1 = match_token toks (Tok_Bool b) in (t1, (Bool b))
    | Some Tok_String s -> let t1 = match_token toks (Tok_String s) in (t1, (String s))
    | Some Tok_ID id -> let t1 = match_token toks (Tok_ID id) in (t1, (ID id))
    | Some Tok_LParen -> let t1 = match_token toks Tok_LParen in 
      let (t2,e1) = parse_Expr t1 in
      let t3 = match_token t2 Tok_RParen in (t3, e1)         
    | _ -> parse_RecordExpr toks 

and parse_RecordExpr toks = 
  match lookahead toks with
  |Some Tok_LCurly -> let t1 = match_token toks Tok_LCurly in
     let (t2,e1) = parse_RecordBodyExpr t1 in 
          let t3 = match_token t2 Tok_RCurly
          in (t3,e1)
  |_ -> raise (InvalidInputException "Parse_RecordExpr Error")

(*RecordBodyExpr -> Tok_ID = Expr ; RecordBodyExpr | Tok_ID = Expr | epsilon*)
  and parse_RecordBodyExpr toks = 
  match lookahead toks with 
  | Some Tok_RCurly -> (toks, Record[])
  | _ ->  
    let rec aux toks acc =
      let (t1,id) =
        match lookahead toks with 
        |Some Tok_ID id -> (match_token toks (Tok_ID id),id)
        |_ -> raise (InvalidInputException "expected Tok_ID") 
      in
      let t2 =
      match lookahead t1 with 
      |Some Tok_Equal -> match_token t1 Tok_Equal
      |_ -> raise (InvalidInputException "expected Tok_Equal")
      in
      let (t3, e1) = parse_Expr t2 in
      match lookahead t3 with
      |Some Tok_Semi -> let t4 = match_token t3 Tok_Semi in aux t4 (((Lab id), e1)::acc)
      |_ -> (((Lab id), e1)::acc, t3)
    in let (a,b) = aux toks [] in (b,Record (List.rev a))
  |_ -> raise (InvalidInputException "Parse_RecordBodyExpr Error")
  
(* Part 3: Parsing mutop *)
let rec parse_mutop toks = 
  let (t1,e) = parse_Mutop toks in
  if t1 <> [] then raise (InvalidInputException "Parse_mutop Error") else (t1,e)

and parse_Mutop toks =
  match lookahead toks with 
  |Some Tok_Def -> parse_DefMutop toks
  |Some Tok_DoubleSemi -> ([], NoOp)
  |_ -> parse_ExprMutop toks

and parse_DefMutop toks = 
  match lookahead toks with
  |Some Tok_Def -> 
	let t1 = match_token toks Tok_Def in
	let (t2, x) = 
	match lookahead t1 with
	|Some (Tok_ID id) -> let t3 = match_token t1 (Tok_ID id) in (t3,id)
	|_ -> raise (InvalidInputException "Parse_DefMutop Error")
    in
    let t4 = match_token t2 Tok_Equal in
    let rem l = match List.rev l with
	        |[] -> []
	        |_::t -> List.rev t
    in 
    let t5 = rem t4 in 
    let (t6, e1) = parse_expr t5 in (t6, Def(x, e1))
  |_ -> raise (InvalidInputException "Parse_DefMutop Error")

and parse_ExprMutop toks = 
  let rem l = match List.rev l with
	      |[] -> []
	      |_::t-> List.rev t
  in
  let x = rem toks in
  let (t1, e1) = parse_expr x in (t1, Expr(e1))




