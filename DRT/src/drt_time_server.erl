%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc This module define a server process that send remind and report email.
%%% @end
%%%----------------------------------------------------------------------------

-module(drt_time_server).
-behavior(gen_server).

%% API
-export([start_link/0, stop/0]).
%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% -export([test_send/0]).
-export([loop/1]).

-include("drt.hrl").

%%%================================================
%%% gen_server callbacks
%%%================================================

init(_LoopData) ->
	% {ok, null}.
	{ok, null, 0}.

terminate(_Reason, _LoopData) ->
	ok.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

handle_call(_Message, _From, LoopData) ->
	% Reply = send_mail(),
	Reply = ok,
	{reply, Reply, LoopData}.

handle_info(timeout, State) ->
	loop(getEmail(email)),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%================================================
%%% API
%%%================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

% test_send() ->
% 	gen_server:call(?MODULE, send).

%%%==============================================
%%% Internal functions
%%%==============================================

% from sys.config
getEmail(Type) ->
	DefaultEmail = ["Bill.L.Wang@newegg.com"],
	case application:get_env(drt, Type, DefaultEmail) of
		[] -> DefaultEmail;
		Email -> Email
	end.

process([], Data, Dets) ->
	dets:close(Dets),
	Data;
process([H|T], Data, Dets) ->
	case dets:lookup(Dets, H) of
		[] -> process(T, [H|Data], Dets);
		_ -> process(T, Data, Dets)
	end.

send_remind([], _Users) -> ok;
send_remind([User|Records], Users) ->
	Body = "<!DOCTYPE HTML>
<html>
<head>
<title>Daily Report Remind</title>
</head>
<body style=\"color:#0099CC\">
	<p>
       Dear " ++ string:substr(User, 1, string:str(User,".")-1) ++",<br/><br/>
       <Strong>Please <a href=\"http://10.16.76.245:8080\">submit</a> daily reports in a timely manner(Before 18:00).</strong>
    </p>
    <p style=\"word-wrap:break-word; text-align:justify\">
       And please remind your colleagues:<br/><br/>
       "++ string:join(getUsers(Users), "|") ++"
    </p>
    <p style=\"font-style: italic;\">
       Cheers,<br/>
       DDRT
	</p>
</body>
</html>",
	Subject = "(Info)Submit Daily Report Remind",
	Cc = "",
	Mail = #mail{to=User, cc=Cc, subject=Subject, body=Body},
	spawn(drt_mail, send_mail, [Mail]),
	send_remind(Records, Users).

send_mail() ->
	{ok, Dets} = dets:open_file("../../report"),
	To = string:join(getEmail(to), ";"),
	Cc = string:join(getEmail(email) ++ getEmail(cc), ";"),
	Subject = "(Report)DFIS Daily Report" ++ getNowTime(),
	Body = "<!DOCTYPE HTML><html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
<title>DFIS Daily Report</title>
    <style type=\"text/css\">
	   *{ margin:0; padding: 0; }
	   body { font-family: Calibri}
	   h1 { font-size: 14pt; font-weight: bold; margin: 10px; text-align: center; color:#0099CC;}
	   .data { margin: 0px 10px 10px; }
	   table, th, td { border: 1px solid #D4E0EE; border-collapse: collapse; color: #333; 10pt;}
	   th { background: #33B5E5; color:#FFF; }
	   td, th { padding: 3px 7px 3px 7px; }
	   tbody tr { background: #FCFDFE; }
	   td { word-break:break-all; word-wrap:break-word;}
	   td.center { text-align: center; }
	   td.large { width: 40%; }
       td.small { width: 20%; }
	</style>
</head>
<body>
    <h1>DFIS Daily Report</h1>
    <div class=\"data\">
        <table style=\"width: 100%\">
            <thead>
                <tr>
                    <th>Team Member</th>
                    <th>Today</th>
                    <th>Issue</th>
                    <th>Next</th>
                </tr>
            </thead>
            <tbody>" ++ string:join(getBody(getEmail(email), [], Dets),"") ++ "</tbody></table></div></body></html>",
	Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
	spawn(drt_mail, send_mail, [Mail]),
	ok.

% format report body
getBody([], Body, Dets) -> dets:close(Dets), Body;
getBody([User|T], Body, Dets) ->
	case dets:lookup(Dets, User) of
		[] ->
			Body1 = "<tr><td class=\"center\">" ++ string:substr(User, 1, string:len(User)-11) ++"</td><td class=\"large\">N/A</td><td class=\"small\">N/A</td><td class=\"small\">N/A</td></tr>",
			getBody(T, [Body1|Body], Dets);
		[{_, [{content, Content},{issue, Issue},{next, Next}]}] ->
			Body1 = "<tr><td class=\"center\">" ++ string:substr(User, 1, string:len(User)-11) 
					++"</td><td class=\"large\">"++ Content ++"</td><td class=\"small\">"
					++ Issue++"</td><td class=\"small\">"
					++ Next ++"</td></tr>",
			getBody(T, [Body1|Body], Dets)
	end.

% Get now time and format
getNowTime() ->
	{{Y,M,D},_} = calendar:local_time(),
	string:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D)],"-").

% clean report dets
cleanReportDets() ->
	{ok, Dets} = dets:open_file("../../report"),
	dets:delete_all_objects(Dets),
	dets:close(Dets).

% get user list
getUsers(Users) ->
	Fun = fun(User) -> string:substr(User, 1, string:len(User)-11) end,
	lists:map(Fun, Users).

% main logic
loop(Records) ->
	io:format("Enter looper ... ~n"),
	{ok, Dets} = dets:open_file("../../report"),
	NewRecords = process(Records, [], Dets)
	{_,{H,M,_}} = calendar:local_time(),
	if
		(H == 16) andalso ((M >= 30) andalso (M < 35)) -> send_remind(NewRecords, NewRecords), timer:sleep(5*60*1000), loop(NewRecords);
        (H == 17) andalso (M < 5) -> send_remind(NewRecords, NewRecords), timer:sleep(5*60*1000), loop(NewRecords);
        (H == 17) andalso ((M >= 25) andalso (M < 30)) -> send_remind(NewRecords, NewRecords), timer:sleep(5*60*1000), loop(NewRecords);
        (H == 18) andalso (M < 5) -> send_mail(), timer:sleep(5*60*1000), loop(NewRecords);
        (H == 0) andalso (M < 5)-> cleanReportDets(), timer:sleep(5*60*1000), loop(getEmail(email));
        true -> timer:sleep(1*60*1000), loop(NewRecords)

	end.