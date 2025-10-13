let is_whitespace c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

let rec lex_helper input pos acc =
  if pos >= String.length input then
    List.rev acc
  else
    let c = input.[pos] in
    if is_whitespace c then
      lex_helper input (pos + 1) acc
    else if c = '(' then
      lex_helper input ( pos + 1 ) ( "(" :: acc )
    else if c = ')' then
      lex_helper input ( pos + 1 ) ( ")" :: acc)
    else if c = '\'' then
      lex_helper input (pos + 1) ( "'" :: acc)
    else if c = ':' then lex_helper input ( pos + 1 ) ( ":" :: acc )
    else
      let end_pos = ref (pos + 1) in
      while !end_pos < String.length input && not (is_whitespace input.[!end_pos]) &&
      input.[!end_pos] <> '(' &&
      input.[!end_pos] <> ')' &&
      input.[!end_pos] <> '\'' &&
      input.[!end_pos] <> ':' do
        incr end_pos
      done;
      let token = String.sub input pos (!end_pos - pos) in
      lex_helper input !end_pos (token :: acc)

let lex input = lex_helper input 0 []
