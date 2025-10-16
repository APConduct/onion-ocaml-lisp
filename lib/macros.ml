

type macro_def = {
  name: string;
  params: string list;
  body: Ast.expr;
}

(* Find a macro definition by name *)
let find_macro name macros =
  List.find_opt (fun m -> m.name = name) macros

(* Substitute macro parameters with arguments in the macro body *)
let rec substitute params args body =
  match body with
  | Ast.Atom s ->
      (match List.assoc_opt s (List.combine params args) with
      | Some v -> v
      | None -> Ast.Atom s)
  | Ast.Number n -> Ast.Number n
  | Ast.List lst -> Ast.List (List.map (substitute params args) lst)
  | Ast.Quote e -> Ast.Quote (substitute params args e)
  | Ast.TypedAtom (s, t) -> Ast.TypedAtom (s, substitute params args t)
  | Ast.CompTime e -> Ast.CompTime (substitute params args e)
  | Ast.CompTimeAssign (s, e) -> Ast.CompTimeAssign (s, substitute params args e)
  | Ast.CompTimeIf (a, b, c) -> Ast.CompTimeIf (substitute params args a, substitute params args b, substitute params args c)
  | Ast.QuasiQuote e -> Ast.QuasiQuote (substitute params args e)
  | Ast.Unquote e -> Ast.Unquote (substitute params args e)
  | Ast.MacroDef (n, ps, b) -> Ast.MacroDef (n, ps, substitute params args b)
  | Ast.MacroCall (n, es) -> Ast.MacroCall (n, List.map (substitute params args) es)

(* Expand macros in an AST using the macro environment *)
let rec expand_macros macros expr =
  match expr with
  | Ast.MacroDef (name, params, body) ->
      (* Macro definitions are not expanded further *)
      Ast.MacroDef (name, params, body)
  | Ast.MacroCall (name, args) ->
      (match find_macro name macros with
      | Some macro ->
          let expanded_args = List.map (expand_macros macros) args in
          let substituted = substitute macro.params expanded_args macro.body in
          expand_macros macros substituted
      | None -> Ast.MacroCall (name, List.map (expand_macros macros) args))
  | Ast.List (Ast.Atom "defmacro" :: Ast.Atom name :: Ast.List params :: body :: []) ->
      let param_names =
        List.map (function Ast.Atom s -> s | _ -> failwith "Macro params must be atoms") params
      in
      Ast.MacroDef (name, param_names, body)
  | Ast.List lst -> Ast.List (List.map (expand_macros macros) lst)
  | Ast.Quote e -> Ast.Quote (expand_macros macros e)
  | Ast.TypedAtom (s, t) -> Ast.TypedAtom (s, expand_macros macros t)
  | Ast.CompTime e -> Ast.CompTime (expand_macros macros e)
  | Ast.CompTimeAssign (s, e) -> Ast.CompTimeAssign (s, expand_macros macros e)
  | Ast.CompTimeIf (a, b, c) -> Ast.CompTimeIf (expand_macros macros a, expand_macros macros b, expand_macros macros c)
  | Ast.QuasiQuote e -> Ast.QuasiQuote (expand_macros macros e)
  | Ast.Unquote e -> Ast.Unquote (expand_macros macros e)
  | _ -> expr

(* Extract macro definitions from a top-level expr, updating macro_env *)
let update_macro_env expr macro_env =
  match expr with
  | Ast.MacroDef (name, params, body) ->
      let macro = { name; params; body } in
      macro :: List.filter (fun m -> m.name <> name) macro_env
  | _ -> macro_env
