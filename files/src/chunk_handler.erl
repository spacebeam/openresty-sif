-module(chunk_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
% we're doing json
-export([to_json/2]).
-export([from_json/2]).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
      Methods = [<<"GET">>, <<"HEAD">>, <<"POST">>,
                 <<"DELETE">>, <<"OPTIONS">>],
      {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

to_json(Req, State) ->
    Body = <<"{\"message\": \"Goodbye! ..and thanks for all the fish!\"}">>,
    {Body, Req, State}.

from_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {ok, Data, Req1} = cowboy_req:read_body(Req),
            Payload1 = jsx:decode(Data, [return_maps]),
            Payload2 = maps:get(<<"payload">>, Payload1, "nothing"),
            FileId = maps:get(<<"uuid">>,
                                Payload1,
                                uuid:uuid_to_string(uuid:get_v4(), binary_standard)),
            DataId = maps:get(<<"payload_uuid">>,
                                Payload1,
                                uuid:uuid_to_string(uuid:get_v4(), binary_standard)),
            lager:warning(FileId),
            lager:warning(DataId),
            Message = #{<<"uuid">> => DataId},
            % thanks for all the fish!
            Req2 = cowboy_req:reply(201,
                                    #{<<"content-type">> => <<"application/json">>},
                                    jsx:encode(Message), Req1);
        _ ->
            {true, Req, State}
    end.
