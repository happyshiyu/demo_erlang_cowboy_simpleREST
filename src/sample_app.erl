-module(sample_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(REF, http).

%% API.

start(_Type, _Args) ->
    Handler = sample_rest_handler,
	Dispatch = cowboy_router:compile(
                 [{'_', [{"/api/1/items", Handler, []}
                        ,{"/api/1/item/:id", Handler, []}
                        ]}
                 ]),
    Env = #{env => #{dispatch => Dispatch}
           },
	{ok, _} = cowboy:start_clear(?REF, [{port,6773}], Env),
	sample_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(?REF).
