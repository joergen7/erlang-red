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
  sqstatement
  dotstatement
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

root -> statements : lists:flatten('$1').

statements -> statement  : ['$1'].
statements -> statement '.' statements : ['$1', '$3'].

statement -> dotstatement : '$1'.
statement -> sqstatement : '$1'.
statement -> dotstatement sqstatement : ['$1', '$2'].
statement -> sqstatement '.' dotstatement : ['$1','$3'].

sqstatement -> '[' string ']' : '$2'.
sqstatement -> '[' string ']' sqstatement : ['$2', '$4'].

dotstatement -> name : '$1'.
dotstatement -> name '.' dotstatement : ['$1', '$3'].

name -> atom : convert_to_atom('$1').
name -> uchars : convert_to_atom('$1').
name -> lchars : convert_to_atom('$1').

string -> dqstring : convert_to_binary('$1').
string -> sqstring : convert_to_binary('$1').

%%
%%
Erlang code.

convert_to_binary({_,_, [$'|V]}) ->
    list_to_atom(lists:reverse(remove_quote(lists:reverse(V))));
convert_to_binary({_,_, [$"|V]}) ->
    list_to_binary(lists:reverse(remove_quote(lists:reverse(V)))).

remove_quote([$"|V]) ->
    V;
remove_quote([$'|V]) ->
    V.

convert_to_atom({_,_,V}) ->
    list_to_atom(V).
