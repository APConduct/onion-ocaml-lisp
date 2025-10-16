let macro_env = ref ([] : Onion.Macros.macro_def list)

let () =
  Printf.printf "Lisp Compiler with macros (type 'quit' to exit) \n";
  Printf.printf "Examples:\n";
  Printf.printf "    (defmacro when (cond body) `(if, cond, body))\n";
  Printf.printf "    (when (> 5 3) (+ 10 20))\n";
  Printf.printf "    (defmacro twice (x) '(+ ,x ,x))\n";
  Printf.printf "    (twice 5)\n";
  Printf.printf ">  ";

  let quit = ref false in
  while not !quit do
    flush stdout;
    try
      let input = read_line () in
      if input = "quit" then (
        quit := true;
        Onion.Comptime_state.comptime_env := [];
        macro_env := [];
      ) else if input = "comptime-env" then (
        Printf.printf "Comptime Environment:\n";
        List.iter (fun (name, value) ->
          match value with
          | Onion.Bytecode.VNum n -> Printf.printf "  %s = %d\n" name n
          | Onion.Bytecode.VAtom s -> Printf.printf "  %s = '%s\n" name s
          | Onion.Bytecode.VList _ -> Printf.printf "  %s = <list>\n" name
        ) !(Onion.Comptime_state.comptime_env);
        Printf.printf ">  "
      ) else if input = "macros" then (
        Printf.printf "Defined Macros:\n";
        List.iter (fun m ->
          Printf.printf "  %s(%s)\n" m.Onion.Macros.name (String.concat ", " m.Onion.Macros.params)
        ) !macro_env;
        Printf.printf ">  "
      ) else if input <> "" then (
        let tokens = Onion.Lexer.lex input in
        let ast, _ = Onion.Expr_parser.parse_expr tokens in
        let expanded = Onion.Macros.expand_macros !macro_env ast in
        (* If it's a macro definition, update macro_env and don't evaluate *)
        (match expanded with
        | Onion.Ast.MacroDef (name, _, _) ->
            macro_env := Onion.Macros.update_macro_env expanded !macro_env;
            Printf.printf "Macro '%s' defined.\n>  " name
        | _ ->
            let (typ, bytecode) = Onion.Compiler.compile_with_types_ast expanded in
            let result = Onion.Vm.execute_instruction bytecode [] in
            (match result with
            | [v] -> Printf.printf "%s : %s\n" (Onion.Vm.stack_value_to_string v) (Onion.Type_checking.type_to_string typ)
            | _l ->
              Printf.printf "Stack: [%s]\n" (String.concat "; " (List.map Onion.Vm.stack_value_to_string result)));
            Printf.printf ">  "
        )
      ) else Printf.printf ">  "
    with
    | Onion.Type_checking.Type_error msg ->
      Printf.printf "Type Error: %s\n" msg;
      Printf.printf ">  "
    | Onion.Comptime_state.Comptime_error msg ->
      Printf.printf "Comptime Error: %s\n" msg;
      Printf.printf ">  "
    | Onion.Comptime_state.Runtime_error msg ->
      Printf.printf "Runtime Error: %s\n" msg;
      Printf.printf ">  "
    | Failure msg ->
      Printf.printf "Runtime Error: %s\n" msg;
      Printf.printf ">  "
    | End_of_file -> quit := true
  done
