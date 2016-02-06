-module (s3demo).
-author("hardy.d.huang@newegg.com").
-behaviour(gen_server).

-export([init/1,  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get_buckets/0, put_object/2, get_object/2, encryption/2,
		get_object_metadata/1, delete/1, get_encryption_metadata/2, upload_encryption/2, create_bucket/1]).

-define (BUCKETNAME, "anzytest").
-define (ENCRYPTION_BUCKETNAME, "anzyperformance").

%%%
% gen_server callback function.
%%%

init(_Args) ->
    {ok, {AccessKeyID, SecretAccessKey}} = application:get_env(sqsdemo, sss),
    erlcloud_s3:configure(AccessKeyID, SecretAccessKey),
    {ok, [{AccessKeyID, SecretAccessKey}]}.


handle_call(get_buckets, _From, State) ->
	Buckets = erlcloud_s3:list_buckets(),
	%io:format("buckets:~p~n", [Buckets]),
	{reply, Buckets, State};

handle_call({get_object, BucketName, Key, Name}, _From, State) ->
	Rets = erlcloud_s3:get_object(BucketName, Key),
	NewPath = filename:join(create_temp(), Name),
	file:write_file(NewPath, proplists:get_value(content, Rets)),
	{reply, NewPath, State};

handle_call({put_object, BucketName, Key, FilePath}, _From, State) ->
	case file:read_file(FilePath) of
		{ok, BinVal} -> 
            Options = [{meta,[{"sha256", "111223213213123123123123"}]}],
			Rets = erlcloud_s3:put_object(BucketName, Key, BinVal, Options);
		{error, Reason} ->
			Rets = Reason
	end,
	{reply, Rets, State};

handle_call({get_object_metadata, KeyName}, _From, State) ->
	Rets = erlcloud_s3:get_object_metadata(?BUCKETNAME, KeyName),
	{reply, Rets, State};


handle_call({encryption_object, KeyName, KeyWord}, _From, State) ->
	[{AccessKeyID, SecretAccessKey}] = State,
	Rets = encryption_object(?BUCKETNAME,KeyName,AccessKeyID,SecretAccessKey,KeyWord),
	{reply, Rets, State};

handle_call({del_object, KeyName}, _From, State) ->
    [{AccessKeyID, SecretAccessKey}] = State,
    Rets = delete_object(?BUCKETNAME, KeyName, AccessKeyID, SecretAccessKey),
    {reply, Rets, State};
handle_call({upload_encryption, FileName, KeyWord}, _From, State) ->
    [{AccessKeyID, SecretAccessKey}] = State,
    Rets = upload_encryption2(?BUCKETNAME, FileName, KeyWord, AccessKeyID, SecretAccessKey),
    {reply, Rets, State};

handle_call({create_bucket, Name}, _From, _State) ->
     Rets = erlcloud_s3:create_bucket(Name),
     {reply, Rets, _State};

handle_call({encryption_meta, KeyName, KeyWord}, _From, State) ->
    [{AccessKeyID, SecretAccessKey}] = State,
    Rets = get_encryption_metadata2(?BUCKETNAME, KeyName, KeyWord, AccessKeyID, SecretAccessKey),
    {reply, Rets, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_buckets() ->
	gen_server:call(?MODULE, get_buckets).

put_object(Key, FileName) ->
	gen_server:call(?MODULE, {put_object,?BUCKETNAME,Key, FileName}).

get_object(Key, Name) ->
	gen_server:call(?MODULE, {get_object, ?BUCKETNAME, Key, Name}).


encryption(KeyName, KeyWord) ->
 	gen_server:call(?MODULE, {encryption_object, KeyName, KeyWord}).

get_object_metadata(KeyName) ->
	gen_server:call(?MODULE, {get_object_metadata, KeyName}).

delete(Kename) ->
    gen_server:call(?MODULE, {del_object, Kename}).


get_encryption_metadata(KeyName, KeyWord) ->
    gen_server:call(?MODULE, {encryption_meta, KeyName, KeyWord}).

upload_encryption(FileName, KeyWord) ->
    gen_server:call(?MODULE, {upload_encryption, FileName, KeyWord}).

create_bucket(Name) ->
    gen_server:call(?MODULE, {create_bucket, Name}).


get_encryption_metadata2(BucketName, KeyName, KeyWord, Access_Key, Secret_Key) ->
    NewPwd = format_32_password(KeyWord),
    RequestHeaders =
        [
         {"host", BucketName ++ ".s3.amazonaws.com"},
         {"x-amz-server-side-encryption-customer-algorithm", "AES256"},
         {"x-amz-server-side-encryption-customer-key",  base64:encode_to_string(NewPwd)},
         {"x-amz-server-side-encryption-customer-key-md5",  base64:encode_to_string(erlcloud_util:md5(NewPwd))}
         ],
    Url = string:join(["https://", BucketName ,".s3.amazonaws.com", "/00-hardytest/copy/", KeyName, ".copy"], ""),
    send_http(head, Url, RequestHeaders,  <<>>,  Access_Key, Secret_Key).



upload_encryption2(BucketName, KeyName, KeyWord, Access_Key, Secret_Key) ->
    NewPwd = format_32_password(KeyWord),
    RequestHeaders =
        [
         {"host", BucketName ++ ".s3.amazonaws.com"},
         {"x-amz-server-side-encryption-customer-algorithm", "AES256"},
         {"x-amz-server-side-encryption-customer-key",  base64:encode_to_string(NewPwd)},
         {"x-amz-server-side-encryption-customer-key-md5",  base64:encode_to_string(erlcloud_util:md5(NewPwd))}
         ],
    Url = string:join(["https://", BucketName ,".s3.amazonaws.com", "/00-hardytest/put/", KeyName, ".copy"], ""),
    send_http(head, Url, RequestHeaders,  <<>>,  Access_Key, Secret_Key).
	



create_temp() ->
    {A,B,C}=now(),
	Path = filename:join("./download", lists:flatten(io_lib:format("~p~p~p",[A,B,C]))) ++ "/",
	filelib:ensure_dir(Path),
	Path.




delete_object(BucketName, KeyName, Access_Key, Secret_Key) ->
    RequestHeaders = [
            {"host", BucketName ++ ".s3.amazonaws.com"}
        ],
    Url = string:join(["http://", BucketName, ".s3.amazonaws.com/", KeyName], ""),
    send_http(delete, Url, RequestHeaders, <<>>, Access_Key, Secret_Key).

% x-amz-server-side-encryption-customer-key: Base64(YourKey)
% x-amz-server-side-encryption-customer-key-MD5 : Base64(MD5(YourKey))
encryption_object(SrcBucketName, SrcKeyName, Access_Key, Secret_Key, KeyWord) when is_list(KeyWord) ->
    % Author = signature_utils:make_author_string(Access_Key, Secret_Key,  "ap-northeast-1", "s3"),
    % io:format(Author),
    NewPwd = format_32_password(KeyWord),
    SourceKey = string:strip(SrcKeyName, both, $/),
    RequestHeaders =
        [
         {"host", SrcBucketName ++ ".s3.amazonaws.com"},
         {"x-amz-server-side-encryption-customer-algorithm", "AES256"},
         {"x-amz-server-side-encryption-customer-key",  base64:encode_to_string(NewPwd)},
         {"x-amz-server-side-encryption-customer-key-md5",  base64:encode_to_string(erlcloud_util:md5(NewPwd))},
         {"x-amz-copy-source", string:join(["/", SrcBucketName, "/", SourceKey], "")}, %[SrcBucketName, $/, SrcKeyName]},
         {"x-amz-metadata-directive", "COPY"},%COPY%REPLACE
         {"x-amz-meta-sss", "11111111112222222222234"}% proplists:get_value(metadata_directive, Options)},

         % {"x-amz-copy-source-if-match", "0ec30779668cb901891ba11ff3d8cb02"},%proplists:get_value(if_match, Options)},
         %{"x-amz-copy-source-if-none-match", "30 Dec 2014 01:43:55 GMT"}
          % {"x-amz-copy-source-if-unmodified-since", "20141230"},
         % {"x-amz-copy-source-if-modified-since", proplists:get_value(if_modified_since, Options)}
         %{"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}
          %{"authorization", Author},
         ],
         %aws4_signature({Access_Key, Secret_Key, Region, Service}, Method, Url, Headers, HashedPayload)
        % aws4_signature(Access_Key, Secret_Key, Region, Service, Method, URI, QueryString, Headers,HashedPayload) ->
        
        %io:format("~p~n",[lists:flatten(AuthorHeader)]),
        Url = string:join(["https://", SrcBucketName ,".s3.amazonaws.com", "/00-hardytest/copy/", SourceKey, ".copy"], ""),
        send_http(put, Url , RequestHeaders, <<>>,  Access_Key, Secret_Key).


     



format_32_password(PassWord) when is_binary(PassWord) ->
    string:left(binary_to_list(PassWord), 32, $-);
format_32_password(PassWord) when is_list(PassWord) ->
    string:left(PassWord, 32, $-).


% -spec send_http(Method, Url, HttpHeaders, Body) -> {StatusCode,  HttpHeaders, Content} when
%     Method :: get | post | put,
%     Url :: string(),
%     HttpHeaders :: [HttpHeader],
%     HttpHeader :: {string(), string()},
%     StatusCode :: pos_integer(),
%     Content :: string(),
%     Body :: any().

%%%
% send a http request.
%%%
send_http(Method, Url, HttpHeaders, Body, Access_Key, Secret_Key) ->
    send_http(Method, Url, HttpHeaders, Body,  Access_Key, Secret_Key, 10000).

send_http(Method, Url, HttpHeaders, Body, Access_Key, Secret_Key, Timeout) ->
    BodySha256 = string:to_lower(binary_to_list(bin_to_hex:bin_to_hex(crypto:hash(sha256, Body)))),
    Content_type = proplists:get_value("content-type", HttpHeaders, "text/plain"),

    TmpHeader = [{"content-length", integer_to_list(byte_size(Body))},{"content-type", Content_type}
    |proplists:delete("content-length",HttpHeaders)],
    

    AuthorHeader = signature_utils:aws4_signature({Access_Key, Secret_Key, "ap-northeast-1", "s3"}, Method, Url, TmpHeader, BodySha256),
    %SafeHttpHeaders = proplists:delete("content-type", AuthorHeader),
    Request = case Method of
        M when M =:=post; M =:= put  -> {Url, proplists:delete("content-type", AuthorHeader), Content_type, Body};
        _ -> {Url, AuthorHeader}
    end,
    Result = httpc:request(Method, Request,[{ssl,[{verify,0}]}, {timeout, Timeout}], []),
    case Result of
        {ok, {{_HttpVersion, StatusCode, _Description}, Headers, Content}} -> {ok, StatusCode, Headers, Content};
        {error, _Message} -> throw({error, 500, list_to_binary(io_lib:format("connect ~p failed.", [Url]))})
    end.


