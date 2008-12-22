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
%% @doc Provide the supervisor tree structure for the application.
-module(twoorl.sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
    .supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% The callback must include the package name as it will be called from an external process.
init(Args) ->
    
    % Supervisor config: http://www.erlang.org/doc/design_principles/sup_princ.html#5.2
    % Restart Strategy: one_for_one | one_for_all | rest_for_one
    % MaxR, MaxT: If more than MaxR number of restarts occur in the last MaxT seconds,
    % then the supervisor terminates all the child processes and then itself.
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 10,
    
    % Child specification: http://www.erlang.org/doc/design_principles/sup_princ.html#spec
    % Id: internal child id used by the supervisor
    % StartFunc: {M=<callback_module>, F, A}
    % PACKAGE USERS: You might want to provide the FULL PACKAGE NAME as this function
    % will be called by a the supervisor module - that lives in the . package!
    % Restart: permanent | temporary | transient
    % Shutdown: brutal_kill | wait up to Timeout milisecs for cleanup | infinity (if child is a sup)
    % Type: supervisor | worker
    % Modules: [<callback_module>] | dynamic (when child is a gen_event)
    
    ChildId = twoorl.webserver,
    % We're passing the webserver configs that were read in the .app file
    StartFunc = {twoorl.webserver, start_link, [Args]},
    Restart = permanent,
    Shutdown = 20000, % This is why it takes so long for the app to shutdown!
    Type = worker,
    Modules = [sup],
    
    {ok, {{RestartStrategy, MaxR, MaxT},
	  [{ChildId, StartFunc, Restart, Shutdown, Type, Modules}]}}.
