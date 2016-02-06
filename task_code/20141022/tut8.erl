-module (tut8).
-export ([format_temps/1]).


format_temps(ListCitys) ->
  ConvertedList = convert_list_to_c(ListCitys),
  print_list(ConvertedList),
  {Max_c,Min_c}= find_max_and_min(ConvertedList),
  print_max_min(Max_c,Min_c).

  convert_list_to_c([{Name,{f,F}}|Rest]) ->
  	[{Name,{c,(F-32)*5/9}} | convert_list_to_c(Rest)];
  convert_list_to_c([City|Rest]) ->
  	[City|convert_list_to_c(Rest)];
  convert_list_to_c([])->
  	[].

print_list([{Name,{c,C}}|Rest]) ->
 io:format("~-15w ~w c ~n",[Name,C]),
 print_list(Rest);
 print_list([]) ->
 ok.

find_max_and_min([Temp|Rest]) ->
    find_max_and_min(Rest,Temp,Temp).

    find_max_and_min([{Name,{c,Temp}}|Rest],
    	{MaxName,{c,Max_Temp}},
    	{MinName,{c,Min_Temp}}) ->
    if
    	Temp > Max_Temp ->
    		MaxCity = {Name,{c,Temp}};
    	true -> MaxCity = {MaxName,{c,Max_Temp}}
    end,
    if
    	Temp < Min_Temp ->
    		MinCity = {Name,{c,Temp}};
    	true -> MinCity = {MinName,{c,Min_Temp}}	
    end,
  find_max_and_min(Rest,MaxCity,MinCity);

  find_max_and_min([],MaxCity,MinCity) ->
   {MaxCity,MinCity}.


   print_max_min({MaxName,{c,C1}},
   	{MinName,{c,C2}}) ->
   io:format("max temperature was ~w c in ~w~n",[MaxName,C1]),
   io:format("min temperature was ~w c in ~w~n",[MinName,C2]).
  
