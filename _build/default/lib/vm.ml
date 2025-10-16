type stack_value =
  | SNum of int
  | SAtom of string
  | SList of stack_value list

let rec stack_value_to_string v =
  match v with
  | SNum n -> string_of_int n
  | SAtom s -> s
  | SList vals -> "(" ^ (String.concat " " (List.map stack_value_to_string vals)) ^ ")"

let rec take n lst =
  if n <= 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | x :: xs ->
      let (taken, rest) = take (n - 1) xs in
      (x :: taken, rest)

let rec execute_instruction instrs stack =
  match instrs with
  | [] -> stack
  | Bytecode.PUSH_NUM n :: rest -> execute_instruction rest (SNum n :: stack)
  | Bytecode.PUSH_ATOM s :: rest -> execute_instruction rest (SAtom s :: stack)
  | Bytecode.QUOTE v :: rest ->
    let stack_v = (match v with
      | Bytecode.VNum n -> SNum n
      | Bytecode.VAtom s -> SAtom s
      | Bytecode.VList vs -> SList (List.map (fun v ->
        match v with
        | Bytecode.VNum n -> SNum n
        | Bytecode.VAtom s -> SAtom s
        | Bytecode.VList _ -> failwith "Nested quoted lists") vs)) in
    execute_instruction rest (stack_v :: stack)
  | Bytecode.ADD :: rest -> (match stack with
    | SNum a :: SNum b :: rest_stack -> execute_instruction rest (SNum (a + b) :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: + requires two numbers"))
  | Bytecode.SUB :: rest ->
    (match stack with
      | SNum a :: SNum b :: rest_stack -> execute_instruction rest (SNum (b-a) :: rest_stack)
      | _ -> raise (Comptime_state.Runtime_error "Type error: - requires two numbers"))
  | Bytecode.MUL :: rest -> (match stack with
    | SNum a :: SNum b :: rest_stack -> execute_instruction rest (SNum (a * b) :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: * requires two numbers"))
  | Bytecode.EQ :: rest -> (match stack with
    | SNum a :: SNum b :: rest_stack ->
      let result = if a = b then SNum 1 else SNum 0 in execute_instruction rest (result :: rest_stack)
    | SAtom a :: SAtom b :: rest_stack ->
      let result = if a = b then SNum 1 else SNum 0 in execute_instruction rest (result :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: = requires same types"))
  | Bytecode.LT :: rest -> (match stack with
    | SNum a :: SNum b :: rest_stack ->
      let result = if b < a then SNum 1 else SNum 0 in
      execute_instruction rest (result :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: < requires numbers"))
  | Bytecode.GT :: rest ->
    (match stack with
      | SNum a :: SNum b :: rest_stack ->
        let result = if b > a then SNum 1 else SNum 0 in
        execute_instruction rest (result :: rest_stack)
      | _ -> raise (Comptime_state.Runtime_error "Type error: > requires numbers"))
  | Bytecode.LIST n :: rest ->
    let (list_elems, rest_stack) = take n stack in
    execute_instruction rest (SList (List.rev list_elems) :: rest_stack)
  | Bytecode.CAR :: rest -> (match stack with
    | SList (x :: _) :: rest_stack -> execute_instruction rest (x :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: car requires a non-empty list"))
  | Bytecode.CDR :: rest -> (match stack with
    | SList (_ :: xs) :: rest_stack -> execute_instruction rest (SList xs :: rest_stack)
    | _ -> raise (Comptime_state.Runtime_error "Type error: cdr requires a non-empty list"))
  | Bytecode.CALL_BUILTIN "+" :: rest ->
    let rec sum_all acc s = match s with
      | [] -> acc
      | SNum n :: xs -> sum_all (acc + n) xs
      | _ -> raise (Comptime_state.Runtime_error "Type error: + expoects numbers") in
    let result = sum_all 0 stack in execute_instruction rest [SNum result]
  | Bytecode.CALL_BUILTIN "-" :: rest -> (match stack with
    | [SNum a] -> execute_instruction rest [SNum (-a)]
    | SNum a :: xs ->
      let rec sub_all acc s =
      match s with
      | [] -> acc
      | SNum n :: xs -> sub_all (acc - n) xs
      | _ -> raise (Comptime_state.Runtime_error "Type error: - expects numbers") in
      let result = sub_all a xs in execute_instruction rest [SNum result]
    | _ -> raise (Comptime_state.Runtime_error "Type error: - expects numbers"))
  | Bytecode.CALL_BUILTIN "*" :: rest ->
    let rec mul_all acc s =
    match s with
    | [] -> acc
    | SNum n :: xs -> mul_all (acc * n) xs
    | _ -> raise (Comptime_state.Runtime_error "Type error: * expects numbers") in
    let result = mul_all 1 stack in execute_instruction rest [SNum result]
  | Bytecode.CALL_BUILTIN "=" :: rest -> (match stack with
    | [SNum a; SNum b] -> execute_instruction rest [if a = b then SNum 1 else SNum 0]
    | [SAtom a; SAtom b] -> execute_instruction rest [if a = b then SNum 1 else SNum 0]
    | _ -> raise (Comptime_state.Runtime_error "Type error: = requires same types"))
  | Bytecode.CALL_BUILTIN "<" :: rest -> (match stack with
    | [SNum a; SNum b] -> execute_instruction rest [if a < b then SNum 1 else SNum 0]
    | _ -> raise (Comptime_state.Runtime_error "Type error: < requires numbers"))
  | Bytecode.CALL_BUILTIN ">" :: rest -> (match stack with
    | [SNum a; SNum b] -> execute_instruction rest [if a > b then SNum 1 else SNum 0]
    | _ -> raise (Comptime_state.Runtime_error "Type error: > requires numbers"))
  | Bytecode.IF (then_code, else_code) :: rest ->
    (match stack with
      | SNum 0 :: rest_stack ->
        let result_stack = execute_instruction else_code rest_stack in
        execute_instruction rest result_stack
      | SAtom "false" :: rest_stack ->
        let result_stack = execute_instruction else_code rest_stack in
        execute_instruction rest result_stack
      | _ :: rest_stack ->
        let result_stack = execute_instruction then_code rest_stack in
        execute_instruction rest result_stack
      | [] -> raise (Comptime_state.Runtime_error "IF requires a condition"))
  | Bytecode.POP :: rest -> (match stack with
    | _ :: rest_stack -> execute_instruction rest rest_stack
    | [] -> raise (Comptime_state.Runtime_error "POP on empty stack"))
  | _ -> raise (Comptime_state.Runtime_error "Unknown instruction")
