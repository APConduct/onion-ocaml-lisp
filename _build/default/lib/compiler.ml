exception Compile_error of string

let rec quote_to_value expr =
  match expr with
  | Ast.Atom s -> Bytecode.VAtom s
  | Ast.Number n -> VNum n
  | List exprs -> VList (List.map quote_to_value exprs)
  | Ast.Quote e -> quote_to_value e
  | TypedAtom (s, _) -> VAtom s
  | CompTime e -> quote_to_value e
  | CompTimeAssign (_, _) -> raise (Compile_error "Cannot quote CompTimeAssign")
  | CompTimeIf (_, _, _) -> raise (Compile_error "Cannot quote CompTimeIf")
  | QuasiQuote _ -> raise (Compile_error "Cannot quote QuasiQuote")
  | Unquote _ -> raise (Compile_error "Cannot quote Unquote")
  | MacroDef (_, _, _) -> raise (Compile_error "Cannot quote MacroDef")
  | MacroCall (_, _) -> raise (Compile_error "Cannot quote MacroCall")

let rec compile expr =
  match expr with
  | Ast.Number n -> [Bytecode.PUSH_NUM n]
  | Ast.Atom s -> [PUSH_ATOM s]
  | Ast.TypedAtom (name, _) -> [PUSH_ATOM name]
  | Ast.Quote e -> [QUOTE (quote_to_value e)]
  | Ast.CompTime e ->
    let value = Comptime_state.eval_comptime e in
    (match value with
      | Bytecode.VNum n -> [PUSH_NUM n]
      | Bytecode.VAtom s -> [PUSH_ATOM s]
      | Bytecode.VList vs ->
        let push_list vs =
          List.concat (List.map (fun v ->
            match v with
            | Bytecode.VNum n -> [Bytecode.PUSH_NUM n]
            | Bytecode.VAtom s -> [PUSH_ATOM s]
            | Bytecode.VList _ -> raise (Compile_error "Nested lists in comptime not supported")
          ) vs) @ [Bytecode.LIST (List.length vs)]
            in
            push_list vs)
  | Ast.List [] -> raise (Compile_error "Cannot compile empty list")
  | Ast.List [Atom "quote"; e] -> [QUOTE (quote_to_value e)]
  | Ast.List (Ast.Atom "comptime" :: [e]) -> compile (Ast.CompTime e)
  | Ast.List (Ast.Atom "+" :: args) ->
  let compiled = List.concat (List.map compile args) in
  compiled @ [CALL_BUILTIN "+"]
  | Ast.List (Ast.Atom "-" :: args) ->
  let compiled = List.concat (List.map compile args) in
  compiled @ [CALL_BUILTIN "-"]
  | Ast.List (Ast.Atom "*" :: args) ->
  let compiled = List.concat (List.map compile args) in
  compiled @ [CALL_BUILTIN "*"]
  | Ast.List (Ast.Atom "=" :: args) ->
  let compiled = List.concat (List.map compile args) in
  compiled @ [CALL_BUILTIN "="]
  | Ast.List (Ast.Atom "<" :: args) ->
    let compiled = List.concat (List.map compile args) in
    compiled @ [CALL_BUILTIN "<"]
  | Ast.List (Ast.Atom ">" :: args) ->
    let compiled = List.concat (List.map compile args) in
    compiled @ [CALL_BUILTIN ">"]
  | Ast.List (Ast.Atom "list" :: args) ->
    let compiled = List.concat (List.map compile args) in
    compiled @ [CALL_BUILTIN "list"]
  | Ast.List (Ast.Atom "car" :: [e]) ->
    let compiled = compile e in
    compiled @ [CALL_BUILTIN "car"]
  | Ast.List (Ast.Atom "cdr" :: [e]) ->
    let compiled = compile e in
    compiled @ [CALL_BUILTIN "cdr"]
  (* | Ast.List (Ast.Atom "cons" :: [a; b]) ->
    let compiled_a = compile a in
    let compiled_b = compile b in
    compiled_a @ compiled_b @ [CONS] *)
  | List (Ast.Atom "if" :: [cond; then_expr; else_expr]) ->
  let cond_code = compile cond in
  let then_code = compile then_expr in
  let else_code = compile else_expr in
  cond_code @ [IF (then_code, else_code)]
  | _ -> raise (Compile_error "Unknown form")
