open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)
let rec eval_expr env e = 
  match e with
  |Int x -> Int x
  |Bool x -> Bool x
  |String x -> String x
  |ID v -> lookup env v
  |Fun (v,e) -> Closure(env,v,e)
  |Not e -> begin
    let x = eval_expr env e in match x with
    |Bool a -> let tmp = not a in Bool(tmp)
    |_ -> raise (TypeError "Not Error") end
  |Binop (operator,e1,e2) ->
    let x = eval_expr env e1 in
    let y = eval_expr env e2 in
    let result =
    match operator with
    |Add -> 
      begin
      match x,y with
      |(Int a,Int b) -> Int(a+b)
      |_ -> raise (TypeError "Add Error")
      end
    |Sub ->
      begin
      match x,y with
      |(Int a,Int b) -> Int(a-b)
      |_ -> raise (TypeError "Sub Error")
      end
    |Mult -> 
      begin
      match x,y with
      |(Int a,Int b) -> Int(a*b)
      |_ -> raise (TypeError "Mult Error")
      end
    |Div -> 
      begin
      match x,y with
      |(Int a,Int b) -> if b = 0 then raise (DivByZeroError) else Int(a/b)
      |_ -> raise (TypeError "Div Error")
      end
    |Concat -> 
         begin
         match x,y with
         |(String a,String b) -> String(a^b)
         |_ -> raise (TypeError "Concat Error")
         end
    |Greater -> 
          begin
          match x,y with
          |(Int a,Int b) -> Bool(a>b)
          |_ -> raise (TypeError "Greater Error")
          end
    |Less -> 
       begin
       match x,y with
       |(Int a,Int b) -> Bool(a<b)
       |_ -> raise (TypeError "Less Error")
       end
    |GreaterEqual -> 
               begin
               match x,y with
               |(Int a,Int b) -> Bool(a>=b)
               |_ -> raise (TypeError "GreaterEqual Error")
               end
    |LessEqual -> 
            begin
            match x,y with
            |(Int a,Int b) -> Bool(a<=b)
            |_ -> raise (TypeError "LessEqual Error")
            end
    |Equal -> 
        begin
        match x,y with
        |(Int a, Int b) -> Bool(a=b)
        |(Bool a, Bool b) -> Bool(a=b)
        |(String a, String b) -> Bool(a=b)
        |_ -> raise (TypeError "Equal Error")
        end
    |NotEqual -> 
           begin
           match x,y with
           |(Int a, Int b) -> Bool(a<>b)
           |(Bool a, Bool b) -> Bool(a<>b)
           |(String a, String b) -> Bool(a<>b)
           |_ -> raise (TypeError "NotEqual Error")
           end
    |Or -> 
      begin
      match x,y with
      |(Bool a, Bool b) -> Bool(a||b)
      |_ -> raise (TypeError "Or Error")
      end
    |And -> 
      begin
      match x,y with
      |(Bool a, Bool b) -> Bool(a&&b)
      |_ -> raise (TypeError "And Error")
      end
   |_ -> raise (TypeError "Binop Error")
   in result
  
  |If (e1,e2,e3) -> 
    let x = eval_expr env e1 in
    let result = match x with
           |Bool x -> if x then let y = eval_expr env e2 in y 
          else let z = eval_expr env e3 in z
           |_ -> raise (TypeError "If Error")
    in result
  |App (e1,e2) ->
    let x = eval_expr env e1 in
    let y = eval_expr env e2 in
    let result = 
    begin
    match x with 
    |Closure(en, id, exp2) -> let en2 = extend en id y in
          eval_expr en2 exp2
    |_ -> raise (TypeError "App Error")
    end
    in result
  |Let (v,is_rec,e1,e2) ->
    if is_rec then 
      let env = extend_tmp env v in
      let a = eval_expr env e1 in
      let () = update env v a in eval_expr env e2
    else
      let a = eval_expr env e1 in 
      let env = extend env v a in eval_expr env e2
  |Closure(env1,v,e) -> Closure(env1,v,e)
  |Record entry_lst -> 
    let x = List.map(fun (l,e) -> (l, eval_expr env e)) entry_lst in (Record x)
  |Select (label, e) -> 
    let x = eval_expr env e in
    begin
    match x with 
    |Record lst -> 
      let rec fsearch = function
       |[] ->  raise (SelectError "Label Error")
       |(a,b)::t -> if a = label then b else fsearch t
      in fsearch lst
    |_ -> raise (TypeError "Select Error")
    end
  |_ -> raise (TypeError "Error")
  
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
   
let eval_mutop env mu = 
  match mu with
  |Def (a,b) -> let env = extend_tmp env a in 
		let e1 = eval_expr env b in let () = update env a e1 in
		(env, Some e1)
  |Expr e1 -> (env, Some(eval_expr env e1))
  |NoOp -> (env, None)



