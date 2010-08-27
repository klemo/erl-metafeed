%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Misc utility functions for erl-metafeed
%%%
%%% Created : 25 Aug 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(utils).

-include_lib("xmerl/include/xmerl.hrl").
-export([get_titles/1]).

%%%-------------------------------------------------------------------
%% Extracts titles from feed Items and returns them as list.
%%%-------------------------------------------------------------------
get_titles(Items) ->
    lists:reverse(get_titles(Items, [])).
get_titles([], Titles) ->
    Titles;
get_titles([Item|Rest], Titles) ->
    [Node|_] = xmerl_xpath:string("//title/text()", Item),
    #xmlText{value=Title} = Node,
    get_titles(Rest, [Title|Titles]).