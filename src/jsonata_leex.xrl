% Inspiration and code and ideas taken from:
%
% - https://andrealeopardi.com/posts/tokenizing-and-parsing-in-elixir-using-leex-and-yecc/
% - https://github.com/jsonata-js/jsonata/blob/master/src/parser.js
% - https://github.com/eskil/example-elixir-parser?tab=readme-ov-file
% - https://github.com/alpaca-lang/alpaca/blob/main/src/alpaca_scan.xrl
% - ... and many more
%
% No AI was harmed nor hypnotized nor hallucinated to in this work.
%

%%
%% This is not intended to be a complete JSONata lexer nor in any way
%% is this work complete.
%% Instead this just enough work to get those JSONata stanza that I use
%% for my flows to work.
%%
%% The challenge of creating a uuid using this is not included -->
%%  https://github.com/jsonata-js/jsonata/pull/731#issuecomment-2593730718
%%


Definitions.

D          = [0-9]
INT        = {D}+
NAME       = [a-zA-Z_][a-zA-Z0-9_]*
WHITESPACE = [\s\t\n\r]
INPUT      = \$\$


Rules.

\/\*  : {token, {commment_start, TokenLine}}.
\*\/  : {token, {commment_end, TokenLine}}.

!=    : {token, {neq,          TokenLine}}.
:=    : {token, {colon_eq,     TokenLine}}.
==    : {token, {eq,           TokenLine}}.

\+    : {token, {op_plus,      TokenLine}}.
\-    : {token, {op_minus,     TokenLine}}.
\*    : {token, {op_multiple,  TokenLine}}.
\/    : {token, {op_divide,    TokenLine}}.
\(    : {token, {open_bracket, TokenLine}}.
\)    : {token, {close_backet, TokenLine}}.
{     : {token, {open_brace,   TokenLine}}.
}     : {token, {close_brace,  TokenLine}}.
\[    : {token, {open_square,  TokenLine}}.
\]    : {token, {close_sqaure, TokenLine}}.
\:    : {token, {colon,        TokenLine}}.
\;    : {token, {semicolon,    TokenLine}}.
\?    : {token, {question,     TokenLine}}.
\=    : {token, {equal,    TokenLine}}.
\%    : {token, {percent,      TokenLine}}.
\<    : {token, {lt,     TokenLine}}.
\>    : {token, {gt,  TokenLine}}.
\"    : {token, {doublequote,  TokenLine}}.
\'    : {token, {singlequote,  TokenLine}}.
and   : {token, {op_and,       TokenLine}}.
or    : {token, {op_or,        TokenLine}}.
\&    : {token, {ampersand,    TokenLine}}.
\$    : {token, {dollar,       TokenLine}}.
\.    : {token, {dot,          TokenLine}}.
,     : {token, {comma,        TokenLine}}.
!     : {token, {bang,         TokenLine}}.

% Function names taken from
%  https://github.com/jsonata-js/jsonata/blob/master/src/functions.js
abs                : {token, {funct, TokenLine, TokenChars}}.
append             : {token, {funct, TokenLine, TokenChars}}.
assert             : {token, {funct, TokenLine, TokenChars}}.
average            : {token, {funct, TokenLine, TokenChars}}.
base64decode       : {token, {funct, TokenLine, TokenChars}}.
base64encode       : {token, {funct, TokenLine, TokenChars}}.
boolean            : {token, {funct, TokenLine, TokenChars}}.
ceil               : {token, {funct, TokenLine, TokenChars}}.
contains           : {token, {funct, TokenLine, TokenChars}}.
count              : {token, {funct, TokenLine, TokenChars}}.
decodeUrl          : {token, {funct, TokenLine, TokenChars}}.
decodeUrlComponent : {token, {funct, TokenLine, TokenChars}}.
distinct           : {token, {funct, TokenLine, TokenChars}}.
each               : {token, {funct, TokenLine, TokenChars}}.
encodeUrl          : {token, {funct, TokenLine, TokenChars}}.
encodeUrlComponent : {token, {funct, TokenLine, TokenChars}}.
error              : {token, {funct, TokenLine, TokenChars}}.
exists             : {token, {funct, TokenLine, TokenChars}}.
filter             : {token, {funct, TokenLine, TokenChars}}.
floor              : {token, {funct, TokenLine, TokenChars}}.
foldLeft           : {token, {funct, TokenLine, TokenChars}}.
formatBase         : {token, {funct, TokenLine, TokenChars}}.
formatNumber       : {token, {funct, TokenLine, TokenChars}}.
join               : {token, {funct, TokenLine, TokenChars}}.
keys               : {token, {funct, TokenLine, TokenChars}}.
length             : {token, {funct, TokenLine, TokenChars}}.
lookup             : {token, {funct, TokenLine, TokenChars}}.
lowercase          : {token, {funct, TokenLine, TokenChars}}.
map                : {token, {funct, TokenLine, TokenChars}}.
match              : {token, {funct, TokenLine, TokenChars}}.
max                : {token, {funct, TokenLine, TokenChars}}.
merge              : {token, {funct, TokenLine, TokenChars}}.
min                : {token, {funct, TokenLine, TokenChars}}.
not                : {token, {funct, TokenLine, TokenChars}}.
number             : {token, {funct, TokenLine, TokenChars}}.
pad                : {token, {funct, TokenLine, TokenChars}}.
power              : {token, {funct, TokenLine, TokenChars}}.
random             : {token, {funct, TokenLine, TokenChars}}.
replace            : {token, {funct, TokenLine, TokenChars}}.
reverse            : {token, {funct, TokenLine, TokenChars}}.
round              : {token, {funct, TokenLine, TokenChars}}.
shuffle            : {token, {funct, TokenLine, TokenChars}}.
sift               : {token, {funct, TokenLine, TokenChars}}.
single             : {token, {funct, TokenLine, TokenChars}}.
sort               : {token, {funct, TokenLine, TokenChars}}.
split              : {token, {funct, TokenLine, TokenChars}}.
spread             : {token, {funct, TokenLine, TokenChars}}.
sqrt               : {token, {funct, TokenLine, TokenChars}}.
string             : {token, {funct, TokenLine, TokenChars}}.
substring          : {token, {funct, TokenLine, TokenChars}}.
substringAfter     : {token, {funct, TokenLine, TokenChars}}.
substringBefore    : {token, {funct, TokenLine, TokenChars}}.
sum                : {token, {funct, TokenLine, TokenChars}}.
trim               : {token, {funct, TokenLine, TokenChars}}.
type               : {token, {funct, TokenLine, TokenChars}}.
uppercase          : {token, {funct, TokenLine, TokenChars}}.
zip                : {token, {funct, TokenLine, TokenChars}}.

%%
%% NodeRED specials
clone         : {token, {funct, TokenLine, TokenChars}}.
env           : {token, {funct, TokenLine, TokenChars}}.
flowContext   : {token, {funct, TokenLine, TokenChars}}.
fromMillis    : {token, {funct, TokenLine, TokenChars}}.
globalContext : {token, {funct, TokenLine, TokenChars}}.
millis        : {token, {funct, TokenLine, TokenChars}}.
moment        : {token, {funct, TokenLine, TokenChars}}.
now           : {token, {funct, TokenLine, TokenChars}}.
parseInteger  : {token, {funct, TokenLine, TokenChars}}.
reduce        : {token, {funct, TokenLine, TokenChars}}.
toMillis      : {token, {funct, TokenLine, TokenChars}}.

function           : {token, {funct_def, TokenLine}}.



{NAME}        : {token, {name, TokenLine, TokenChars}}.
{WHITESPACE}+ : skip_token.
{INPUT}       : {token, {msg_obj, TokenLine}}.

%% Taken from
%%    https://github.com/alpaca-lang/alpaca/blob/main/src/alpaca_scan.xrl

%% Integer
{D}+       : {token, {int, TokenLine, list_to_integer(TokenChars)}}.

%% Float
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.

Erlang code.
