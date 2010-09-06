%%%-------------------------------------------------------------------
%%% File    : feed_parser.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Provides basic operations on feeds
%%% Created : 14 Apr 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(feed_parser).
-include_lib("xmerl/include/xmerl.hrl").
-export([fetch/1, filter/2, sort/2, union/2, tail/2, unique/1, replace/2]).

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%% Feed parser API.
%% Called from metafeed interpreter module.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Fetch feed from given URL
%%%-------------------------------------------------------------------
fetch(Url) ->
    parse_feed(aggregator:read(Url)).

%%%-------------------------------------------------------------------
%% Filter feed items that contain Text in Elements
%%%-------------------------------------------------------------------
filter({contains, Text, Elements}, Items) ->
    lists:filter(fun(X) ->
                         eval_contains(X, Text, Elements) end,
                 Items);

%%%-------------------------------------------------------------------
%% Filter feed items that do not contain Text in Elements
%%%-------------------------------------------------------------------
filter({does_not_contain, Text, Elements}, Items) ->
    lists:filter(fun(X) ->
                         not eval_contains(X, Text, Elements) end,
                 Items);

%%%-------------------------------------------------------------------
%% Filter feed items with text in Element equal to Text
%%%-------------------------------------------------------------------
filter({equal, Text, Element}, Items) ->
    lists:filter(fun(X) ->
                         eval_equal(X, Text, Element) end,
                 Items).


%%%-------------------------------------------------------------------
%% Replace Str1 with Str2 in Elements
%%%-------------------------------------------------------------------
replace({Str1, Str2, Element}, Items) ->
    r_s({Str1, Str2, Element}, Items).

%%%-------------------------------------------------------------------
%% sort Items based on Element in ascending order
%%%-------------------------------------------------------------------
sort({ascending, Element}, Items) ->
    lists:sort(fun(X, Y) ->
                       compare(ascending, Element, X, Y) end,
               Items);

%%%-------------------------------------------------------------------
%% Sort Items based on Element in descending order
%%%-------------------------------------------------------------------
sort({descending, Element}, Items) ->
    lists:sort(fun(X, Y) ->
                        compare(descending, Element, X, Y) end,
               Items).

%%%-------------------------------------------------------------------
%% Merge two feeds into union
%%%-------------------------------------------------------------------
union(Items1, Items2) ->
    %first make sure feed items are sorted on pubDate descending
    %make union by merging two lists of items using custom order func
    lists:merge(fun(X, Y) ->
                        temporal_order(X, Y) end,
                sort({descending, "pubDate"}, Items1),
                sort({descending, "pubDate"}, Items2)).

%%%-------------------------------------------------------------------
%% Take first Count feed items
%%%-------------------------------------------------------------------
tail(Count, Items) ->
    lists:sublist(Items, Count).

%%%-------------------------------------------------------------------
%% Removes duplicate items from feed
%%%-------------------------------------------------------------------
unique(Items) ->
    % first make sure feed items are sorted on guid, then remove
    % duplicates
    remove_duplicates(
      sort({ascending, "guid"}, Items)).

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%% Feed parser implementation functions.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Read feed from given URL and return list of parsed feed items
%%%-------------------------------------------------------------------
parse_feed(Data) ->
    case xmerl_scan:string(Data) of
        {error, _} ->
            throw({fetch, "error parsing fetch source"});
        {ParsResult, _} ->
            case length(xmerl_xpath:string("/rss", ParsResult)) of
                0 -> throw({fetch, "not an rss feed"});
                _ -> true
            end,
            xmerl_xpath:string("//item", ParsResult)
    end.

%%%-------------------------------------------------------------------
%% Removes duplicate items from feed based on guid
%% Feed must be sorted on guid before calling this function
%%%-------------------------------------------------------------------
remove_duplicates([]) ->
    [];
remove_duplicates(Items) ->
    lists:reverse(remove_duplicates(Items, [])).

remove_duplicates([Item,NextItem|T], Result) ->
    XPathExpr = "//guid/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, Item),
    [Node2|_] = xmerl_xpath:string(XPathExpr, NextItem),
    case string:equal(Node1#xmlText.value, Node2#xmlText.value) of
        true ->
            remove_duplicates([NextItem|T], Result);
        false ->
            remove_duplicates([NextItem|T], [Item|Result])
    end;

remove_duplicates([X|_], Result) ->
    [X|Result].

%%%-------------------------------------------------------------------
%% Function for temporal ordering of two feed items
%%%-------------------------------------------------------------------
temporal_order(X, Y) ->
    XPathExpr = "//pubDate/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, X),
    [Node2|_] = xmerl_xpath:string(XPathExpr, Y),
    httpd_util:convert_request_date(Node1#xmlText.value) >
        httpd_util:convert_request_date(Node2#xmlText.value).

%%%-------------------------------------------------------------------
%% Function for comparison of two feed items based on feed Element.
%% Type is ascending or descending.
%%%-------------------------------------------------------------------
compare(Type, Element, X, Y) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, X),
    [Node2|_] = xmerl_xpath:string(XPathExpr, Y),
    % date comparison is special case
    Date = string:equal(Element, "pubDate"),
    if
        Date ->
            FC = fun(DateText) ->
                         httpd_util:convert_request_date(DateText)
                 end;          
        true -> FC = fun(Text) -> Text end
    end,
    case Type of
        ascending ->
            FC(Node1#xmlText.value) < FC(Node2#xmlText.value);
        descending ->
            FC(Node1#xmlText.value) > FC(Node2#xmlText.value)
    end.

%%%-------------------------------------------------------------------
%% Evaluates to true if feed Item Element equals Text
%%%-------------------------------------------------------------------
eval_equal(Item, Text, Element) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    [Node|_] = xmerl_xpath:string(XPathExpr, Item),
    string:equal(Node#xmlText.value, Text).

%%%-------------------------------------------------------------------
%% Check if Item contains Text in any of Elements array
%%%-------------------------------------------------------------------
eval_contains(_, _, []) ->
    false;

eval_contains(Item, Text, [Element|RestElements]) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    case contains_text(Text,
                       xmerl_xpath:string(XPathExpr, Item)) of
        true -> true;
        _ -> eval_contains(Item, Text, RestElements)
        
    end.

%%%-------------------------------------------------------------------
%% Search for string in parsed xml
%%%-------------------------------------------------------------------

contains_text(_, []) ->
    false;

%% handle xmlElement
contains_text(Text, [H|T]) when is_record(H, xmlElement) ->
    [contains_text(Text, H#xmlElement.content)|contains_text(Text, T)];

%% when match is found on xmlText actually replace strings
contains_text(Text, [H|T]) when is_record(H, xmlText) ->
    case string:str(H#xmlText.value, Text) of
        0 -> contains_text(Text, T);
        _ -> true
    end.

%%%-------------------------------------------------------------------
%% Replace string in parsed feed structure
%% {Str1, Str2, Element} = Args
%% Any occurence of Str1 will be replaced with Str2 in elements with
%% name equal to Element
%%%-------------------------------------------------------------------

r_s(Args, Item) ->
    r_s(Args, Item, false).

r_s(_, [], _) ->
    [];

%% handle xmlElement
r_s(Args, [H|T], false) when is_record(H, xmlElement) ->
    utils:log("Current node ~p~n", [H#xmlElement.name]),
    {_, _, Element} = Args,
    Elem_name = atom_to_list(H#xmlElement.name),
    %% element name match
    if Elem_name == Element ->
            Match = true;
       %% continue recursive traversal
       true ->
            Match = false
    end,
    New_element = H#xmlElement{
                    content = r_s(Args, H#xmlElement.content, Match)
                   },
    [New_element|r_s(Args, T)];

%% just continue traversal on this matched xmlElement
r_s(Args, [H|T], true) when is_record(H, xmlElement) ->
    [r_s(Args, H#xmlElement.content, true)|r_s(Args, T)];

%% when match is found on xmlText actually replace strings
r_s(Args, [H|T], true) when is_record(H, xmlText) ->
    {Str1, Str2, _} = Args,
    New_value = re:replace(H#xmlText.value, Str1, Str2, [{return,list}]),
    [H#xmlText{value = New_value }|r_s(Args, T)];

%% skip other xmerl stuff
r_s(Args, [H|T], _) ->
    [H|r_s(Args, T)].
