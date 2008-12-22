%% This file is part of Twoorl.
%% 
%% Twoorl is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Twoorl is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Twoorl.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Copyright Nick Gerakines, 2008
%%
%% @author Nick Gerakines <nick@gerakines.net> [http://blog.socklabs.com/]
%% @copyright Nick Gerakines, 2008
%% @doc Manages the yaws component of the application.
-module(twoorl.webserver).
-behaviour(gen_server).

-include_lib("yaws/include/yaws.hrl").
-include("twoorl.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/1]).

start_link(Args) ->
    .gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% These Args are carried here from the supervisor
init([ServerConfigs]) ->
    process_flag(trap_exit, true),
    case .application:start(yaws) of
        ok -> set_conf(ServerConfigs);
        Error -> {stop, Error}
    end.

set_conf(ServerConfigs) ->
    GC = .yaws_config:make_default_gconf(false, "twoorl"),
    SCs = .lists:map(
		fun(Sconf) ->
			#sconf{
				port = .proplists:get_value(port, Sconf, 80),
				servername = .proplists:get_value(servername, Sconf, "localhost"),
				listen = .proplists:get_value(listen, Sconf, {0,0,0,0}),
				docroot = .proplists:get_value(docroot, Sconf, "www"),
				appmods = .proplists:get_value(appmods, Sconf, [{"/", erlyweb}]),
				opaque = .proplists:get_value(opaque, Sconf, [{"appname", "twoorl"}])
			    }
		end, ServerConfigs),
    try .yaws_api:setconf(GC, [SCs]) of
        ok -> {ok, started};
        Errora -> {stop, Errora}
    catch
        Errorb -> {stop, Errorb}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> .application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
