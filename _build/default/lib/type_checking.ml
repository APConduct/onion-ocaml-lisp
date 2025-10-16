exception Type_error of string

let rec infer_type (env : Types.type_env) expr =
  match expr with
  | Ast.Number _ -> Types.TInt

  | Atom name ->
      (try List.assoc name env
       with Not_found -> Types.TAny)

  | TypedAtom (_, typ) -> infer_type env typ  (* FIXED: infer type of Ast.expr, not assume it's Types.typ *)

  | Quote _ -> TAtom

  | CompTime e -> infer_type env e

  | List [] -> raise (Type_error "Cannot infer type of empty list")

  | List (Atom "+" :: args) ->
      List.iter (fun arg ->
        let t = infer_type env arg in
        if t <> TInt && t <> TAny then
          raise (Type_error ("+ expects Int, got " ^ type_to_string t))
      ) args;
      TInt

  | List (Atom "-" :: args) ->
      List.iter (fun arg ->
        let t = infer_type env arg in
        if t <> TInt && t <> TAny then
          raise (Type_error ("- expects Int, got " ^ type_to_string t))
      ) args;
      TInt

  | List (Atom "*" :: args) ->
      List.iter (fun arg ->
        let t = infer_type env arg in
        if t <> TInt && t <> TAny then
          raise (Type_error ("* expects Int, got " ^ type_to_string t))
      ) args;
      TInt

  | List (Atom "=" :: [left; right]) ->
      let lt = infer_type env left in
      let rt = infer_type env right in
      if lt <> TAny && rt <> TAny && lt <> rt then
        raise (Type_error ("= expects same types, got " ^ type_to_string lt ^ " and " ^ type_to_string rt));
      TInt

  | List (Atom "<" :: [left; right]) ->
      let lt = infer_type env left in
      let rt = infer_type env right in
      if (lt <> TInt && lt <> TAny) || (rt <> TInt && rt <> TAny) then
        raise (Type_error "< expects Int arguments");
      TInt

  | List (Atom ">" :: [left; right]) ->
      let lt = infer_type env left in
      let rt = infer_type env right in
      if (lt <> TInt && lt <> TAny) || (rt <> TInt && rt <> TAny) then
        raise (Type_error "> expects Int arguments");
      TInt

  | List (Atom "list" :: args) ->
      (match args with
       | [] -> TList TAny
       | first :: rest ->
           let elem_type = infer_type env first in
           List.iter (fun arg ->
             let t = infer_type env arg in
             if t <> TAny && elem_type <> TAny && t <> elem_type then
               raise (Type_error ("list has inconsistent element types"))
           ) rest;
           TList elem_type)

  | List (Atom "car" :: [arg]) ->
      (match infer_type env arg with
       | TList elem_type -> elem_type
       | TAny -> TAny
       | t -> raise (Type_error ("car expects List, got " ^ type_to_string t)))

  | List (Atom "cdr" :: [arg]) ->
      (match infer_type env arg with
       | TList elem_type -> TList elem_type
       | TAny -> TAny
       | t -> raise (Type_error ("cdr expects List, got " ^ type_to_string t)))

  (* | CompTime e -> infer_type env e *)

  | CompTimeAssign (_, e) -> infer_type env e

  | CompTimeIf (_, then_e, else_e) ->
      let then_type = infer_type env then_e in
      let else_type = infer_type env else_e in
      if then_type <> TAny && else_type <> TAny && then_type <> else_type then
        raise (Type_error ("comptime-if branches have different types"));
      then_type

  | QuasiQuote _ -> TAny
  | Unquote e -> infer_type env e
  | MacroDef (_, _, _) -> TAtom
  | MacroCall (_, _) -> TAny  (* Type checked after expansion *)

  | _ -> TAny

and type_to_string t =
  match t with
  | TInt -> "Int"
  | TAtom -> "Atom"
  | TList elem -> "List[" ^ type_to_string elem ^ "]"
  | TFn (args, ret) ->
      "(" ^ String.concat " -> " (List.map type_to_string args) ^ " -> " ^ type_to_string ret ^ ")"
  | TVar v -> v
  | TAny -> "Any"
