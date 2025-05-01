Nonterminals
  root
  statement
  statements
  args
  expr
  comments
  comment
.

Terminals
  ';'
  '('
  ')'
  '.'
  ','
  comment_start
  comment_end
  name
  msg_obj
  funct
  string
  chars
  name
.

Rootsymbol
   root
.

root -> statements : wrap_with_func('$1').

statements -> statement : ['$1'].
statements -> statement ';' statements : ['$1'|'$3'].

statement -> funct '(' args ')' : convert_funct('$1', '$3').

args -> expr : '$1'.
args -> expr ',' expr : ['$1', '$3'].

expr -> msg_obj '.' name : to_map_get('$3').
expr -> msg_obj '.' name '.' name : to_map_get('$3').
expr -> string : '$1'.
expr -> chars : '$1'.

comments -> comment.
comments -> comment comments.
comment -> comment_start comment_end.
comment -> comment_start chars comment_end.
comment -> comment_start name comment_end.


Erlang code.


to_list([Expr]) ->
    to_list(Expr);
to_list(Expr) when is_binary(Expr) ->
    binary_to_list(Expr);
to_list(Expr) ->
    Expr.

to_map_get({name,1,AttrName}) ->
    list_to_binary(io_lib:format("maps:get(~s,Msg)",[to_list(AttrName)])).

convert_funct({funct,1,FunctName}, Expr) ->
    case FunctName of
        count ->
            list_to_binary(io_lib:format("erlang:length(~s)",[to_list(Expr)]));
        Unknown ->
            list_to_binary(io_lib:format("~s(~p)",[Unknown,Expr]))
    end.

wrap_with_func(V) ->
    list_to_binary(io_lib:format("fun (Msg) -> ~s end",[V])).
