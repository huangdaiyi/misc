{application, hello_erlang, [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['hello_erlang_app','hello_erlang_sup']},
	{registered, [hello_erlang_sup]},
	{applications, [kernel,stdlib]},
	{mod, {hello_erlang_app, []}}
]}.