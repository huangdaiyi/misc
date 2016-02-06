-module (signature_utils).
-export([aws4_signature/5]).

-import(bin_to_hex, [bin_to_hex/1]).

ftime(datetime, {{Y,Mo,D},{H,M,S}}) ->
    lists:flatten(io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ", [Y,Mo,D,H,M,S]));
ftime(date, {{Y,Mo,D}, _}) ->
    lists:flatten(io_lib:format("~4..0b~2..0b~2..0b", [Y,Mo,D])).

sign(Key, Data) ->
    crypto:hmac(sha256, Key, Data).

url_encode(Url) ->
    Segments = filename:split(Url),
    url_encode(Segments, []).

url_encode([], Segments) ->
    filename:join(lists:reverse(Segments));
url_encode(["/"|R], Acc) ->
    url_encode(R, ["/"|Acc]);
url_encode([Segment|R], Acc) ->
    url_encode(R, [http_uri:encode(Segment)|Acc]).

%%
% URI = "/examplebucket/myphoto.jpg"
% <HTTPMethod>\n
% <CanonicalURI>\n
% <CanonicalQueryString>\n
% <CanonicalHeaders>\n
% <SignedHeaders>\n
% <HashedPayload>
-spec create_canonical_request(Method, URI, QueryString, Headers, Payload) -> {string(), string()} when
    Method :: atom(),
    URI :: string(),
    QueryString :: [{Name, Value}],
    Headers :: [{Name, Value}],
    Name :: string(),
    Value :: string(),
    Payload :: string().
create_canonical_request(Method, URI, QueryString, Headers, HashedPayload) ->
    HTTPMethod = string:to_upper(atom_to_list(Method)),
    EncodingQueryString = [{http_uri:encode(Name), http_uri:encode(Value)} || {Name, Value} <- QueryString],
    CanonicalQueryString = string:join([ Name ++ "=" ++ Value || {Name, Value} <- lists:keysort(1, EncodingQueryString) ],  "&"),
    NormalizedHeaders = [ {string:to_lower(Name), Value} || {Name, Value} <- lists:keysort(1, Headers)],
    SignedHeaders = string:join([ Name || {Name, _Value} <- NormalizedHeaders ], ";"),
    CanonicalHeaders = string:join([ Name ++ ":" ++ string:strip(Value) ++ "\n" || {Name, Value} <- NormalizedHeaders ],  ""),
    {SignedHeaders, lists:flatten(io_lib:format("~s\n~s\n~s\n~s\n~s\n~s", [HTTPMethod, url_encode(URI), CanonicalQueryString, CanonicalHeaders, SignedHeaders, HashedPayload]))}.


calculate_signature(Secret_Key, Timestamp, RegionName, ServiceName) ->
    KData = sign("AWS4" ++ Secret_Key, Timestamp),
    KRegion = sign(KData, RegionName),
    KService = sign(KRegion, ServiceName),
    sign(KService, "aws4_request").

aws4_signature({Access_Key, Secret_Key, Region, Service}, Method, Url, Headers, HashedPayload) ->

    UTC = calendar:universal_time(),
    DateStamp = ftime(date, UTC),
    AMZDate = ftime(datetime, UTC),

    {ok, {_Scheme, _UserInfo, Host, Port, Uri, Query}} = http_uri:parse(Url),
    QueryString = [],%mochiweb_util:parse_qs(string:strip(Query, left, $?)),

    Headers1 = [{string:to_lower(Key), Value} || {Key, Value} <- Headers],
    Headers2 = case proplists:is_defined("host", Headers1) of
            false ->
                SafeHost = case Port of
                    80 -> Host;
                    _ -> lists:flatten(io_lib:format("~s:~b", [Host, Port]))
                end,
                [{"host", SafeHost}|Headers1];
            true -> Headers1
    end,
    Headers3 = [{"x-amz-date", AMZDate} | proplists:delete("x-amz-date", Headers2)],
    SafeHeaders = [{"x-amz-content-sha256", HashedPayload} | proplists:delete("x-amz-content-sha256", Headers3)],

    % Create a Canonical Request
    {SignedHeaders, CanonicalRequest} = create_canonical_request(Method, Uri, QueryString, SafeHeaders, HashedPayload),

    % Create a String to Sign
    CredentialScope = lists:flatten(io_lib:format("~s/~s/~s/aws4_request", [DateStamp, Region, Service])),
    HashedCanonicalRequest = string:to_lower(binary_to_list(bin_to_hex(crypto:hash(sha256, CanonicalRequest)))),
    StringToSign = lists:flatten(io_lib:format("AWS4-HMAC-SHA256\n~s\n~s\n~s", [AMZDate, CredentialScope, HashedCanonicalRequest])),


    % Calculate Signature
    SigningKey = calculate_signature(Secret_Key, DateStamp, Region, Service),

    % Calculate siguature
    Signature = string:to_lower(binary_to_list(bin_to_hex(crypto:hmac(sha256, SigningKey, StringToSign)))),

    Authorization = lists:flatten(io_lib:format("AWS4-HMAC-SHA256 Credential=~s/~s, SignedHeaders=~s, Signature=~s", [Access_Key, CredentialScope, SignedHeaders, Signature])),
    [{"authorization", Authorization}|SafeHeaders].

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").

ftime_test_() ->
    TestCases = [{datetime, {{2015, 01,01}, {8,52,0}}, "20150101T085200Z"},
                        {datetime, {{2015, 12,12}, {0,0,0}}, "20151212T000000Z"},
                        {date, {{2015, 12,12}, {0,0,0}}, "20151212"},
                        {date, {{2015, 01,01}, {0,0,0}}, "20150101"}],
    lists:map(fun({Type, Args, Expect}) ->
        ?_assertEqual(ftime(Type, Args), Expect)
    end, TestCases).
-endif.
