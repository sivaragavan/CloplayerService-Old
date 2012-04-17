%% Event server
-module(newsserv).
-export([start/0, terminate/0, init/0, loop/1,
         execute_api/4, story_add/7, topics_sources/0]).

-include("cloreader.hrl").

%%% User Interface

start() ->
    register(?MODULE, Pid1=spawn(?MODULE, init, [])),
    Pid1.

terminate() ->
    ?MODULE ! shutdown.

init() ->
    
    TechTopic = #topic{name=list_to_binary("Technology"), sourceList=[
								      {"TechCrunch", "http://feeds.feedburner.com/TechCrunch/"},
								      {"Technology Review", "http://feeds.technologyreview.com/technology_review_in_computing"},
								      {"Wired", "http://feeds.wired.com/wired/index"}
								      %%{"The Week", "http://theweek.com/topic/sub_section/science_tech/technology.rss"}
								     ],
		       sourcePriority=dict:from_list(
				       [
					{3, []},
					{2, ["Wired"]},
					{1, ["Wired", "Technology Review"]}
				       ]
				      )},
    
    IndiaTopic = #topic{name=list_to_binary("India"), sourceList=[
								  %%{"The Hindu", "http://www.hindu.com/rss/02hdline.xml"},
								  {"Times Of India", "http://timesofindia.feedsportal.com/c/33039/f/533916/index.rss"},
								  {"Hindustan Times", "http://feeds.hindustantimes.com/HT-IndiaSectionPage-Topstories"}
								 ],
			sourcePriority=dict:from_list(
				       [
					{3, []},
					{2, ["Hindustan Times"]},
					{1, ["Hindustan Times", "Times Of India"]}
				       ]
				      )},
    
    
    WorldTopic = #topic{name=list_to_binary("World"), sourceList=[
								  {"CNN", "http://rss.cnn.com/rss/edition_world.rss"},
								  {"BBC", "http://feeds.bbci.co.uk/news/world/rss.xml"},
								  {"Reuters", "http://feeds.reuters.com/Reuters/worldNews"}
								  %%{"The Hindu", "http://www.hindu.com/rss/03hdline.xml"},
								  %%{"The Week", "http://theweek.com/topic/sub_section/news_opinion/world_news_opinion.rss"}
								 ],
			sourcePriority=dict:from_list(
				       [
					{3, []},
					{2, ["BBC"]},
					{1, ["BBC", "Reuters"]}
				       ]
				      )},
    
    
    SportsTopic = #topic{name=list_to_binary("Sports"), sourceList=[
								    %%{"Crickinfo", "http://static.espncricinfo.com/rss/livescores.xml"}
								    {"Espn / Star", "http://www.espnstar.com/headlines-rss/"}
								    %%{"The Hindu", "http://www.hindu.com/rss/07hdline.xml"}
								   ],
			 sourcePriority=dict:from_list(
				       [
					{3, []},
					{2, ["Espn / Star"]},
					{1, ["Espn / Star"]}
				       ]
				      )},
    
    
    BusinessTopic = #topic{name=list_to_binary("Business"), sourceList=[
									%%{"Forbes", "http://www.forbes.com/markets/feed/"},
									{"Economic Times", "http://economictimes.feedsportal.com/c/33041/f/534024/index.rss"}
									%%{"The Hindu", "http://www.hindu.com/rss/06hdline.xml"},
									%%{"The Week", "http://theweek.com/topic/sub_section/business/world_business.rss"}
								       ],
			   sourcePriority=dict:from_list(
				       [
					{3, []},
					{2, ["Economic Times"]},
					{1, ["Economic Times"]}
				       ]
				      )},
    
    
    S = #state{topics=dict:from_list([
				       {1, TechTopic},
				       {2, IndiaTopic},
				       {3, WorldTopic},
				       {4, SportsTopic},
				       {5, BusinessTopic}
				      ])
	      },

    S1 = add_user(S, 123, [{1,{3,false}},{2,{2,false}},{3,{1,false}},{4,{3,true}}]),
    loop(S1).

execute_api(Method, Path, Req, Args) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, Method, Path, Req, Args},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
	    [{error, timeout}]
    end.


story_add(TopicId, HeadlineText, Guid, Link, DetailText, Date, Source) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, add, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}}},
    receive
	{Ref, Msg} ->
	    Msg
    after 5000 ->
	    [{error, timeout}]
    end.

topics_sources() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {topics, sources}},
    receive
	{Ref, Msg} ->
	    Msg
    after 5000 ->
	    [{error, timeout}]
    end.


%%% The Server itself

loop(S=#state{}) ->
    receive
        {Pid, MsgRef, 'GET', ["api", UserIdString, "create"], Req, Args} ->
	    UserId = element(1,string:to_integer(UserIdString)),
	    io:format("User Created : ~p", [UserId]),
	    S1 = add_user(S, UserId, []),
            Pid ! {MsgRef, [{userId, UserId}]},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", "set"], Req, Args} ->
	    UserId = element(1,string:to_integer(UserIdString)),
	    PrefTopics = jsx:json_to_term(list_to_binary(getArg("prefTopics", Req, Args))),
	    PrefIds = name_to_id_dict(PrefTopics, []),
	    S1 = topics_set(S, UserId, PrefIds),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "stories", "next"], _Req, _Args} ->
	    UserId = element(1,string:to_integer(UserIdString)),
	    {S1, {StoryId, TopicId}} = get_next_story(S, UserId),
	    if
		StoryId > 0 ->
		    Topic = dict:fetch(TopicId, S#state.topics),
		    Story = dict:fetch(StoryId, Topic#topic.storyList),
		    User = dict:fetch(UserId, S#state.users),
		    Pid ! {MsgRef, [{storyId, StoryId}, {topicId, TopicId}, {topicName, Topic#topic.name}, {headlineText, Story#story.headlineText}, {date, Story#story.date}, {source, Story#story.source}, {detailText, Story#story.detailText}, {guid, Story#story.guid}, {link, Story#story.link}, {total, length(User#user.storyStack)}]};
		true ->
		    Pid ! {MsgRef, [{storyId, StoryId}, {topicId, TopicId}]}
	    end,
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "stories", "previous"], _Req, _Args} ->
	    UserId = element(1,string:to_integer(UserIdString)),
	    {S1, {StoryId, TopicId}} = get_previous_story(S, UserId),
            if
                StoryId > 0 ->
                    Topic = dict:fetch(TopicId, S#state.topics),
                    Story = dict:fetch(StoryId, Topic#topic.storyList),
                    Pid ! {MsgRef, [{storyId, StoryId}, {topicId, TopicId}, {topicName, Topic#topic.name}, {headlineText, Story#story.headlineText}, {date, Story#story.date}, {source, Story#story.source}, {detailText, Story#story.detailText}, {guid, Story#story.guid}, {link, Story#story.link}]};
                true ->
                    Pid ! {MsgRef, [{storyId, StoryId}, {topicId, TopicId}]}
            end,
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "more"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = increase_topic_priority(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "less"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = decrease_topic_priority(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "only"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = only_topic(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", "all", "unmute"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
	    S1 = unmute_all(S, UserId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "mute"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = mute_topic(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "unmute"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = unmute_topic(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "add"], Req, Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    Priority = getIntArg("priority", Req, Args),
	    S1 = add_topic(S, UserId, TopicId, Priority),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", TopicIdString, "remove"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
            TopicId = element(1,string:to_integer(TopicIdString)),
	    S1 = remove_topic(S, UserId, TopicId),
	    PrefTopicList = get_user_prefs(S1, UserId),
	    Pid ! {MsgRef, PrefTopicList},
            loop(S1);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", "all"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
	    PrefTopicList = get_user_prefs(S, UserId),
            Pid ! {MsgRef, PrefTopicList},
            loop(S);
        {Pid, MsgRef, 'GET', ["api", UserIdString, "topics", "new"], _Req, _Args} ->
            UserId = element(1,string:to_integer(UserIdString)),
	    User = dict:fetch(UserId, S#state.users),
            PrefTopicList = dict:fold(fun(K,V,A) -> [K|A] end,[], User#user.prefTopics),
            AllTopicList = dict:fold(fun(K,V,A) -> [K|A] end,[], S#state.topics),
	    NewTopicList = AllTopicList -- PrefTopicList,
	    Result = lists:map(fun(TopicId) -> Topic = dict:fetch(TopicId, S#state.topics), {Topic#topic.name, TopicId} end, NewTopicList),
            Pid ! {MsgRef, Result},
            loop(S);
        {Pid, MsgRef, 'GET', ["api", "topics", "all"], _Req, _Args} ->
            TopicList = dict:fold(fun(K,V,A) -> [{V#topic.name, K}|A] end,[], S#state.topics),
            Pid ! {MsgRef, TopicList},
            loop(S);
	{Pid, MsgRef, 'GET', Path, _Req, _Args} ->
	    Pid ! {MsgRef, [{errorResponse, Path}]},
	    loop(S);
	{Pid, MsgRef, {topics, sources}} ->
	    SourceList = dict:fold(fun(K,V,A) -> [{K, V#topic.sourceList}|A] end,[], S#state.topics),
            Pid ! {MsgRef, SourceList},
            loop(S);
	{Pid, MsgRef, {story, add, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}}} ->
            S1 = add_story(S, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}),
	    Pid ! {MsgRef, true},    
	    loop(S1)
    end.


%% Next and Previous Utils

get_next_story(S, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    Current = {CurrentStory, _} = User#user.current,
    StoryStack = User#user.storyStack,
    {NewCurrent, NewStoryStack} = pop_story_stack(StoryStack),
    NewUser = User#user{current = NewCurrent, storyStack = NewStoryStack}, 
    if
	CurrentStory > 0  ->
	    ReadStories = NewUser#user.readStories,
	    NewReadStories = [Current | ReadStories],
	    NewUserAgain = NewUser#user{readStories = NewReadStories},
	    NewUsers = dict:store(UserId, NewUserAgain, S#state.users),
	    {S#state{users = NewUsers}, NewCurrent};
	true ->
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
	    {S#state{users = NewUsers}, NewCurrent}
    end.

pop_story_stack([]) ->
    {{0,0}, []};
pop_story_stack(StoryStack) ->
    [NewCurrent | NewStoryStack] = StoryStack,
    {NewCurrent, NewStoryStack}.

get_previous_story(S, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    Current = {CurrentStory, _} = User#user.current,
    ReadStories = User#user.readStories,
    if
        CurrentStory > 0  ->
	    NewCurrent = {NewCurrentStory, _} = get_nth_read_story(1, ReadStories),
	    if
		NewCurrentStory > 0 ->
		    StoryStack = User#user.storyStack,
		    NewStoryStack = [Current | StoryStack],
		    NewUser = User#user{current = {-1,0}, storyStack = NewStoryStack}, 
		    NewUsers = dict:store(UserId, NewUser, S#state.users),
		    {S#state{users=NewUsers}, NewCurrent};
		true ->
		    {S, NewCurrent}
	    end;
	true ->
	    NewCurrent = {NewCurrentStory, _} = get_nth_read_story(abs(CurrentStory - 1), ReadStories),
	    if
		NewCurrentStory > 0 ->
		    NewUser = User#user{current = {CurrentStory - 1, 0}}, 
		    NewUsers = dict:store(UserId, NewUser, S#state.users),
		    {S#state{users=NewUsers}, NewCurrent};
		true ->
		    {S, NewCurrent}
	    end
    end.

get_nth_read_story(N, ReadStories) ->
    N1 = length(ReadStories),
    if
	N1 < N ->
	    {0, 0};
	true ->
	    lists:nth(N, ReadStories)
    end.


%% Add User and Stories

name_to_id_dict([], Acc) -> Acc;
name_to_id_dict([{_Name, Prefs} | Rest], Acc) ->
    PrefDict = dict:from_list(Prefs),
    Id = dict:fetch(list_to_binary("id"), PrefDict),
    Priority = dict:fetch(list_to_binary("priority"), PrefDict),
    Muted = dict:fetch(list_to_binary("muted"), PrefDict),
    name_to_id_dict(Rest, [{Id, {Priority, Muted}} | Acc]).    


add_user(S, UserId, PrefTopics) ->
    case dict:find(UserId, S#state.users) of
        {ok, _} ->
	    %%User = dict:fetch(UserId, S#state.users),
	    %%ReadStories = [current | User.readStories], 
	    %%NewUser = User#user{nextStories = [], readStories = ReadStories, current = {0,0}},
	    %%NewUsers = dict:store(UserId, NewUser, S#state.users),
	    %%rebuild_story_stack(S#state{users=NewUsers}, UserId),
	    S;
	error ->
	    NewUser = #user{prefTopics = dict:from_list(PrefTopics)},
            NewUsers = dict:store(UserId, NewUser, S#state.users),
            S1 = S#state{users=NewUsers},
	    rebuild_story_stack(S1, UserId)
    end.


topics_set(S, UserId, PrefTopics) ->
    User = dict:fetch(UserId, S#state.users),
    NewUser = User#user{prefTopics = dict:from_list(PrefTopics)},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

add_story(S, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}) ->
    Topic = dict:fetch(TopicId, S#state.topics),
    StoryList = dict:to_list(Topic#topic.storyList),
    case lists:any(fun({_, Story}) -> Story#story.guid == list_to_binary(Guid) end, StoryList) of
	false ->
	    NewStoryId = Topic#topic.lastStoryId + 1,
	    io:format("~p-~p : Processing update~n", [TopicId, NewStoryId]),
	    %%io:format("~w : Processing update~n", [HeadlineText]),
	    NewStory = #story{headlineText=list_to_binary(HeadlineText),date=list_to_binary(httpd_util:rfc1123_date(Date)), source=list_to_binary(Source), guid=list_to_binary(Guid), link=list_to_binary(Link), detailText=list_to_binary(DetailText)},
	    NewStoryList = dict:store(NewStoryId, NewStory, Topic#topic.storyList),
	    NewStoryStack = S#state.storyStack ++ [{NewStoryId, TopicId}],
	    NewTopic = Topic#topic{storyList=NewStoryList, lastStoryId=NewStoryId},
	    NewTopics = dict:store(TopicId, NewTopic, S#state.topics),
	    S1 = push_story_to_users(S, {NewStoryId, TopicId}, NewStory, NewTopic, dict:fetch_keys(S#state.users)),
	    S1#state{topics=NewTopics, storyStack=NewStoryStack};
	true ->
	    S
    end.


%% Story Stack Operations

push_story_to_users(S, _, _, _, []) -> 
    S;
push_story_to_users(S, {StoryId, TopicId}, NewStory, NewTopic, [UserId | UserList]) ->
    push_story_to_users(push_story_to_user(S, {StoryId, TopicId}, NewStory, NewTopic, UserId), {StoryId, TopicId}, NewStory, NewTopic, UserList).

push_story_to_user(S, NewStoryId, NewStory, NewTopic, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    case is_relevant_for_user(NewStoryId, NewStory, NewTopic, User) of 
	true ->
	    StoryStack = User#user.storyStack,    
	    NewStoryStack = [ NewStoryId | StoryStack],
	    NewUser = User#user{storyStack = NewStoryStack},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
	    S#state{users=NewUsers};
	 false ->
	    S
    end.
	    

is_relevant_for_user({StoryId, TopicId}, Story, Topic, User) ->
    case dict:find(TopicId, User#user.prefTopics) of
	{ok, TopicPref} ->
	    is_relevant_for_topic_volume({StoryId, TopicId}, Story, Topic, User, TopicPref);
	error ->
	    false
    end.
    
is_relevant_for_topic_volume({StoryId, TopicId}, Story, Topic, User, {Priority, Muted}) ->
    {ok, SourceList} = dict:find(Priority, Topic#topic.sourcePriority),
    case index_of(binary_to_list(Story#story.source), SourceList) of
	not_found ->
            is_relevant_topic_muted({StoryId, TopicId}, Story, Topic, User, {Priority, Muted});
	Value ->
	    false
    end.

is_relevant_topic_muted({StoryId, TopicId}, Story, Topic, User, {Priority, false}) ->
    is_relevant_already_read({StoryId, TopicId}, Story, Topic, User, {Priority, false});
is_relevant_topic_muted({StoryId, TopicId}, Story, Topic, User, {Priority, true}) ->
    false.

is_relevant_already_read({StoryId, TopicId}, Story, Topic, User, {Priority, false}) ->
    case index_of({StoryId, TopicId}, User#user.readStories) of
	not_found ->
	    if
		{StoryId, TopicId} == User#user.current ->
		    false;
		true ->
		    true
	    end;
	Value ->
	    false
    end.
    

index_of(Item, List) ->
     index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

    
rebuild_story_stack(S, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    NewUser = User#user{storyStack=[]},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_process(S#state{users=NewUsers}, UserId, S#state.storyStack).

rebuild_process(S, _, []) ->
    S;
rebuild_process(S, UserId, [{StoryId, TopicId} | StoryList]) ->
    Topic = dict:fetch(TopicId, S#state.topics),
    Story = dict:fetch(StoryId, Topic#topic.storyList),
    S1 = push_story_to_user(S, {StoryId, TopicId}, Story, Topic, UserId),
    rebuild_process(S1, UserId, StoryList).
    
    

getArg(Key, Req, Args) ->
    case Req:get_variable(Key, Args) of
	undefined ->
	    Req:ok([{"Content-Type", "text/xml"}], "<misultin_test><error>" + Key + " not specified</error></misultin_test>");
	Value ->
	    Value
    end.

getIntArg(Key, Req, Args) ->
    element(1,string:to_integer(getArg(Key, Req, Args))).


get_user_prefs(S, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    PrefTopicList = dict:fold(fun(K=TopicId,V={Priority, Muted},A) -> Topic = dict:fetch(TopicId, S#state.topics), [{Topic#topic.name, [{id, TopicId}, {priority, Priority}, {muted, Muted}]}|A] end,[], User#user.prefTopics).


increase_topic_priority(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    Pref = {Priority, Muted} = dict:fetch(TopicId, User#user.prefTopics),
    NewPref = {Priority + 1, Muted},
    NewPrefTopics = dict:store(TopicId, NewPref, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

decrease_topic_priority(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    Pref = {Priority, Muted} = dict:fetch(TopicId, User#user.prefTopics),
    NewPref = {Priority - 1, Muted},
    NewPrefTopics = dict:store(TopicId, NewPref, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

mute_topic(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    Pref = {Priority, Muted} = dict:fetch(TopicId, User#user.prefTopics),
    NewPref = {Priority, true},
    NewPrefTopics = dict:store(TopicId, NewPref, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).
    
unmute_topic(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    Pref = {Priority, Muted} = dict:fetch(TopicId, User#user.prefTopics),
    NewPref = {Priority, false},
    NewPrefTopics = dict:store(TopicId, NewPref, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

add_topic(S, UserId, TopicId, Priority) ->
    User = dict:fetch(UserId, S#state.users),
    NewPref = {Priority, false},
    NewPrefTopics = dict:store(TopicId, NewPref, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

remove_topic(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    NewPrefTopics = dict:erase(TopicId, User#user.prefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).

unmute_all(S, UserId) ->
    User = dict:fetch(UserId, S#state.users),
    NewPrefTopics = process_unmute_all(User#user.prefTopics, dict:to_list(User#user.prefTopics)), 
    NewUser = User#user{prefTopics=NewPrefTopics},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).
    
process_unmute_all(PrefTopics, []) -> 
    PrefTopics;
process_unmute_all(PrefTopics, [{TopicId, {Priority, _}} | Rest]) ->
    NewPref = {Priority, false},
    NewPrefTopics = dict:store(TopicId, NewPref, PrefTopics),
    process_unmute_all(NewPrefTopics, Rest).
    
    
only_topic(S, UserId, TopicId) ->
    User = dict:fetch(UserId, S#state.users),
    NewPrefTopics = process_mute_all(User#user.prefTopics, dict:to_list(dict:erase(TopicId, User#user.prefTopics))), 
    Pref = {Priority, Muted} = dict:fetch(TopicId, User#user.prefTopics),
    NewPref = {Priority, false},
    NewPrefTopics1 = dict:store(TopicId, NewPref, NewPrefTopics),
    NewUser = User#user{prefTopics=NewPrefTopics1},
    NewUsers = dict:store(UserId, NewUser, S#state.users),
    rebuild_story_stack(S#state{users=NewUsers}, UserId).
    
process_mute_all(PrefTopics, []) -> 
    PrefTopics;
process_mute_all(PrefTopics, [{TopicId, {Priority, _}} | Rest]) ->
    NewPref = {Priority, true},
    NewPrefTopics = dict:store(TopicId, NewPref, PrefTopics),
    process_mute_all(NewPrefTopics, Rest).
    


			  
    
