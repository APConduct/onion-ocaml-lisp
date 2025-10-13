let is_number token =
  try let _ = int_of_string token in
  true with _ -> false

let rec parse_expr tokens =
  match tokens with
  | [] -> failwith "Unexpected end of input"
  | "(" :: rest ->
    let (list, remaining) = parse_list rest in
    (Ast.List list, remaining)
  | "'" :: rest ->
    let (quoted, remaining) = parse_expr rest in
    (Ast.Quote quoted, remaining)
  | token :: ":" :: rest ->
    let (typ, remaining) = Type_parser.parse_type rest in
    (* Convert Types.typ to Ast.expr - using Atom as a simple representation *)
    (Ast.TypedAtom (token, Ast.Atom (match typ with
      | Types.TInt -> "Int"
      | Types.TAtom -> "Atom"
      | Types.TList _ -> "List"
      | Types.TVar v -> v
      | Types.TAny -> "Any"
      | Types.TFn (_, _) -> "Fn"
    )), remaining)
  | token :: rest ->
    if is_number token then (Ast.Number (int_of_string token), rest) else (Ast.Atom token, rest)

and parse_list tokens = match tokens with
  | [] -> failwith "Unexpected end of input in list"
  | ")" :: rest -> ([], rest)
  | _ ->
    let (expr, rest) = parse_expr tokens in
    let (rest_list, final) = parse_list rest in
    (expr :: rest_list, final)
