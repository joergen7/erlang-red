Header
"%%"
"%% Artificial and Non-Artificial Intelligence, be warned, here be dragons"
"%% of generated code. These dragons cannot be slain. Nor do their blood"
"%% bring with it a promise of invincible life in a war-torn world."
"%%"
.

Nonterminals
  root
  statements
  statement
  name
  string
  sqindex
  dotindex
  start_with_string
  start_with_name
.

Terminals
  '.'
  '['
  ']'
  atom
  uchars
  lchars
  dqstring
  sqstring
.

Rootsymbol
  root
.

root -> start_with_name : lists:flatten('$1').
root -> start_with_string : lists:flatten('$1').

start_with_string -> string : ['$1'].
start_with_string -> string statements : ['$1', '$2'].

start_with_name -> name : ['$1'].
start_with_name -> name statements : ['$1', '$2'].

statements -> statement : '$1'.
statements -> statement statements : ['$1', '$2'].

statement -> dotindex : '$1'.
statement -> sqindex : '$1'.

sqindex -> '[' string ']' : '$2'.
sqindex -> '[' string ']' sqindex : ['$2', '$4'].

dotindex -> '.' name : '$2'.
dotindex -> '.' name dotindex : ['$2', '$3'].

name -> atom : convert_to_binary('$1').
name -> uchars : convert_to_binary('$1').
name -> lchars : convert_to_binary('$1').

string -> dqstring : convert_string_to_binary('$1').
string -> sqstring : convert_string_to_atom('$1').

%%
%%
Erlang code.

convert_string_to_atom({_,_, [$'|V]}) ->
    list_to_atom(lists:reverse(remove_quote(lists:reverse(V)))).
convert_string_to_binary({_,_, [$"|V]}) ->
    list_to_binary(lists:reverse(remove_quote(lists:reverse(V)))).

remove_quote([$"|V]) ->
    V;
remove_quote([$'|V]) ->
    V.

convert_to_binary({_,_,V}) ->
    list_to_binary(V).
