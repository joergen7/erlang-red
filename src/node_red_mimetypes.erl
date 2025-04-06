-module(node_red_mimetypes).

-export([mt/1]).

compute_mime_type([]) ->
    {<<"application">>,<<"octet-stream">>,[]};

%%
%% Because some files have no extenstions, Cowboy defualts to either
%% octet stream and then denying the file (because the browser requests
%% appication/json), hardcode the mimetype for certain files.
%%
compute_mime_type([Path|Paths]) ->
    case Path of
        "plugins" ->
            {<<"application">>, <<"json">>,[]};
        "nodes" ->
            {<<"application">>, <<"json">>,[]};
        "user" ->
            {<<"application">>, <<"json">>,[]};
        _ ->
            compute_mime_type(Paths)
    end.

mt(FileName) ->
    Mt = cow_mimetypes:all(FileName),

    case Mt of
        {<<"application">>,<<"octet-stream">>,[]} ->
            compute_mime_type(string:tokens(binary_to_list(FileName),"/"));
        _ ->
            Mt
    end.
