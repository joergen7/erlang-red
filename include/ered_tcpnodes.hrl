-define(SetSessionId, '_sessionid' => SessionId).
-define(GetSessionId, '_sessionid' := SessionId).

-define(SetBacklog, '_backlog' => Backlog).
-define(GetBacklog, '_backlog' := Backlog).
-define(EmptyBacklog, '_backlog' => []).
-define(DefineBacklog(V), '_backlog' => V).

-define(GetPort, <<"port">> := Port).
-define(GetHost, <<"host">> := Host).
