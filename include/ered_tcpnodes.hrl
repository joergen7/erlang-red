-define(SetSessionId, '_sessionid' => SessionId).
-define(GetSessionId, '_sessionid' := SessionId).

-define(IsClient, <<"beserver">> := <<"client">>).
-define(IsReply, <<"beserver">> := <<"reply">>).

-define(SetBacklog, '_backlog' => Backlog).
-define(GetBacklog, '_backlog' := Backlog).
-define(EmptyBacklog, '_backlog' => []).
