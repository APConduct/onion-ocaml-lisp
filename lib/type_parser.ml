

let rec parse_type tokens =
  match tokens with
  | [] -> failwith "Unexpected end of input in type"
  | "Int" :: rest -> (Types.TInt, rest)
  | "Atom" :: rest -> (Types.TAtom, rest)
  | "(" :: "List" :: rest ->
    let (elem_type, remaining) = parse_type rest in
    (match remaining with
      | ")" :: final -> (Types.TList elem_type, final)
      | _ -> failwith "Expected ')' in list type")
  | token :: rest when String.length token > 0 && token.[0] = '\'' -> (Types.TVar token, rest)
  | _ -> failwith "Unknown type"
