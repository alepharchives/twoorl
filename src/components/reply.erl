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
%% Copyright Yariv Sadan, 2008
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan, 2008

-module(reply).
-compile(export_all).

save_replies(MsgId, Recipients) ->
    Replies = [reply:new_with([{usr_id, Recipient:id()}, {msg_id, MsgId}])
	       || Recipient <- Recipients],
    reply:insert(Replies).
