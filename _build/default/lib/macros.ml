type macro_def = {
  name: string;
  params: string list;
  body: Ast.expr;
}

let macro_env = ref []
