let comptime_env = ref []
  exception Runtime_error of string
  exception Comptime_error of string

  let rec eval_comptime expr =
      match expr with
      | Ast.Number n -> Bytecode.VNum n
      | Ast.Atom name ->
        (try List.assoc name !comptime_env with Not_found ->
          raise (Runtime_error ("Cannot evaluate atom at compile time: " ^ name)))
      | Ast.Quote e -> eval_quoted_comptime e
      | Ast.List [] -> raise (Runtime_error "Cannot evaluate empty list")
      | Ast.TypedAtom (name, _) ->
        (try List.assoc name !comptime_env with Not_found ->
          raise (Runtime_error ("Cannot evaluate typed atom at compile time: " ^ name)))
      | Ast.CompTime e -> eval_comptime e
      | Ast.CompTimeAssign (_, _) -> raise (Runtime_error "Cannot evaluate CompTimeAssign at compile time")
      | Ast.CompTimeIf (_, _, _) -> raise (Runtime_error "Cannot evaluate CompTimeIf at compile time")
      | Ast.QuasiQuote _ -> raise (Runtime_error "Cannot evaluate QuasiQuote at compile time")
      | Ast.Unquote _ -> raise (Runtime_error "Cannot evaluate Unquote at compile time")
      | Ast.MacroDef (_, _, _) -> raise (Runtime_error "Cannot evaluate MacroDef at compile time")
      | Ast.MacroCall (_, _) -> raise (Runtime_error "Cannot evaluate MacroCall at compile time")
      | List (Ast.Atom "+" :: args) ->
        let nums = List.map eval_comptime args in
        let sum = List.fold_left (fun acc v ->
          match v with
          | Bytecode.VNum n -> acc + n
          | _ -> raise (Runtime_error "Type error: + expects numbers")) 0 nums in VNum sum
      | List(Ast.Atom "-" :: args) ->
        (match args with
          | [] -> raise(Runtime_error "- requires at least one argument")
          | [single] ->
            (match eval_comptime single with
              | Bytecode.VNum n -> VNum (-n)
              | _ -> raise(Runtime_error "Type error: - expects numbers"))
          | first :: rest ->
          let first_val = (match eval_comptime first with
            | Bytecode.VNum n -> n
            | _ -> raise(Runtime_error "Type error: - expects numbers")) in
          let rest_nums = List.map eval_comptime rest in
          let result = List.fold_left (fun acc v ->
          match v with
          | Bytecode.VNum n -> acc - n
          | _ -> raise (Runtime_error "Type error: - expects numbers")) first_val rest_nums in
          VNum result)
      | List (Ast.Atom "*" :: args) ->
      let nums = List.map eval_comptime args in
      let product = List.fold_left (fun acc v ->
        match v with
        | Bytecode.VNum n -> acc * n
        | _ -> raise (Runtime_error "Type errorL * expects number")) 1 nums in
      VNum product
      | List (Ast.Atom "=" :: [left; right]) ->
        let l = eval_comptime left in
        let r = eval_comptime right in
        (match (l, r) with
          | (VNum a, VNum b) -> if a = b then VNum 1 else VNum 0
          | (VAtom a, VAtom b) -> if a = b then VNum 1 else VNum 0
          | _ -> raise (Runtime_error "Type error: = requires same types"))
      | List (Ast.Atom "<" :: [left; right]) ->
        let l = eval_comptime left in
        let r = eval_comptime right in
        (match (l, r) with
          | (VNum a, VNum b) -> if a < b then VNum 1 else VNum 0
          | _ -> raise (Runtime_error "Type error: < expects numbers"))
      | List (Ast.Atom ">" :: [left; right]) ->
        let l = eval_comptime left in
        let r = eval_comptime right in
        (match (l, r) with
          | (VNum a, VNum b) -> if a > b then VNum 1 else VNum 0
          | _ -> raise (Runtime_error "Type error: > expects numbers"))
      | List (Ast.Atom "list" :: args) -> VList (List.map eval_comptime args)
      | List ( Ast.Atom "car" :: [arg]) ->
        (match eval_comptime arg with
          | VList (x :: _) -> x
          | _ -> raise (Runtime_error "car requires non-empty list"))
      | List (Ast.Atom "cdr" :: [arg]) ->
        (match eval_comptime arg with
          | VList (_ :: rest) -> VList rest
          | _ -> raise(Runtime_error "cdr requires a non-empty list"))
      | List _ -> raise (Runtime_error "Cannot evaluate this form at compile time")

      and eval_quoted_comptime expr =
        match expr with
        | Ast.Atom s -> VAtom s
        | Ast.Number n -> VNum n
        | List exprs -> VList (List.map eval_quoted_comptime exprs)
        | Ast.Quote e -> eval_quoted_comptime e
        | TypedAtom (s, _) -> VAtom s
        | CompTime e -> eval_comptime e
        | CompTimeAssign (_, _) -> raise (Runtime_error "Cannot evaluate CompTimeAssign in quoted context")
        | CompTimeIf (_, _, _) -> raise (Runtime_error "Cannot evaluate CompTimeIf in quoted context")
        | QuasiQuote _ -> raise (Runtime_error "Cannot evaluate QuasiQuote in quoted context")
        | Unquote _ -> raise (Runtime_error "Cannot evaluate Unquote in quoted context")
        | MacroDef (_, _, _) -> raise (Runtime_error "Cannot evaluate MacroDef in quoted context")
        | MacroCall (_, _) -> raise (Runtime_error "Cannot evaluate MacroCall in quoted context")

      let rec value_to_expr v =
        match v with
        | Bytecode.VNum n -> Ast.Number n
        | Bytecode.VAtom s -> Ast.Atom s
        | Bytecode.VList vs -> List (List.map value_to_expr vs)
