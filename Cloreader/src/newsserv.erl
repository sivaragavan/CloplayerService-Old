%% Event server
-module(newsserv).
-export([start/0, terminate/0, init/0, loop/1,
         topics_all/0, topics_details/2, user_pref/1, user_pref_set/2, story_next/2, story_previous/2, story_peek/2, story_get/2, story_details/2, story_mark/3, story_add/7, find_next/2, topics_sources/0]).

-include("cloreader.hrl").

%%% User Interface

start() ->
    register(?MODULE, Pid1=spawn(?MODULE, init, [])),
    Pid1.

terminate() ->
    ?MODULE ! shutdown.

init() ->
    %% Loading topics from a static file could be done here.
    %% You would need to pass an argument to init (maybe change the functions
    %% start/0 and start_link/0 to start/1 and start_link/1) telling where the
    %% resource to find the topics is. Then load it from here.
    %% Another option is to just pass the topics straight to the server
    %% through this function.
    
    TechTopic = #topic{name=list_to_binary("Technology"), sourceList=[
								      {"TechCrunch", "http://feeds.feedburner.com/TechCrunch/"},
								      {"Technology Review", "http://feeds.technologyreview.com/technology_review_in_top_stories"},
								      {"Wired", "http://feeds.wired.com/wired/index"}
								      %%{"The Week", "http://theweek.com/topic/sub_section/science_tech/technology.rss"}
								     ]},
    
    IndiaTopic = #topic{name=list_to_binary("India"), sourceList=[
								  %%{"The Hindu", "http://www.hindu.com/rss/02hdline.xml"},
								  {"Times Of India", "http://timesofindia.feedsportal.com/c/33039/f/533916/index.rss"},
								  {"Hindustan Times", "http://feeds.hindustantimes.com/HT-IndiaSectionPage-Topstories"}
								 ]},
    
    WorldTopic = #topic{name=list_to_binary("World"), sourceList=[
								  {"CNN", "http://rss.cnn.com/rss/edition_world.rss"},
								  {"BBC", "http://feeds.bbci.co.uk/news/world/rss.xml"},
								  {"Reuters", "http://feeds.reuters.com/Reuters/worldNews"}
								  %%{"The Hindu", "http://www.hindu.com/rss/03hdline.xml"},
								  %%{"The Week", "http://theweek.com/topic/sub_section/news_opinion/world_news_opinion.rss"}
								 ]},

    SportsTopic = #topic{name=list_to_binary("Sports"), sourceList=[
								    %%{"Crickinfo", "http://static.espncricinfo.com/rss/livescores.xml"}
								    {"Espn / Star", "http://www.espnstar.com/headlines-rss/"}
								    %%{"The Hindu", "http://www.hindu.com/rss/07hdline.xml"}
								   ]},

    BusinessTopic = #topic{name=list_to_binary("Business"), sourceList=[
									%%{"Forbes", "http://www.forbes.com/markets/feed/"},
									{"Economic Times", "http://economictimes.feedsportal.com/c/33041/f/534024/index.rss"}
									%%{"The Hindu", "http://www.hindu.com/rss/06hdline.xml"},
									%%{"The Week", "http://theweek.com/topic/sub_section/business/world_business.rss"}
								       ]},

    loop(#state{topics=dict:from_list([
				       {1, TechTopic},
				       {2, IndiaTopic},
				       {3, WorldTopic},
				       {4, SportsTopic},
				       {5, BusinessTopic}
				      ])
	       }).

topics_all() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {topics, all}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

topics_details(TopicId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {topics, details, TopicId, UserId}},
    receive
        {Ref, Name, NewStories} -> [{name, Name}, {newStories, NewStories}]
    after 5000 ->
        {error, timeout}
    end.

user_pref(UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {user, pref, UserId}},
    receive
        {Ref, Msg} -> [{prefTopics, Msg}]
    after 5000 ->
        {error, timeout}
    end.

user_pref_set(UserId, Pref) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {user, pref, set, UserId, Pref}},
    receive
        {Ref, true} -> [{success, true}]
    after 5000 ->
        {error, timeout}
    end.

story_next(TopicId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, next, TopicId, UserId}},
    receive
        {Ref, Msg} -> [{nextStoryId, Msg}]
    after 5000 ->
        {error, timeout}
    end.

story_previous(TopicId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, previous, TopicId, UserId}},
    receive
        {Ref, Msg} -> [{previousStoryId, Msg}]
    after 5000 ->
        {error, timeout}
    end.

story_peek(TopicId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, peek, TopicId, UserId}},
    receive
        {Ref, Msg} -> [{peekStoryId, Msg}]
    after 5000 ->
        {error, timeout}
    end.

story_get(TopicId, StoryId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, get, TopicId, StoryId}},
    receive
        {Ref, HeadlineText, Guid, Date, Source} -> [{headlineText, HeadlineText}, {guid, Guid}, {date, Date}, {source, Source}]
    after 5000 ->
        {error, timeout}
    end.

story_details(TopicId, StoryId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, details, TopicId, StoryId}},
    receive
        {Ref, DetailText, Link} -> [{detailText, DetailText}, {link, Link}]
    after 5000 ->
        {error, timeout}
    end.

story_mark(TopicId, StoryId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, mark, TopicId, StoryId, UserId}},
    receive
        {Ref, true} -> [{success, true}]
    after 5000 ->
        {error, timeout}
    end.

story_add(TopicId, HeadlineText, Guid, Link, DetailText, Date, Source) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, add, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}}},
    receive
        {Ref, true} -> [{success, true}]
    after 5000 ->
        {error, timeout}
    end.

topics_sources() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {topics, sources}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

%%% The Server itself

loop(S=#state{}) ->
    receive
        {Pid, MsgRef, {topics, all}} ->
	    Names = dict:fold(fun(K,V,A) -> [{list_to_binary(integer_to_list(K)), V#topic.name}|A] end,[], S#state.topics),
            Pid ! {MsgRef, Names},
            loop(S);
        {Pid, MsgRef, {topics, sources}} ->
	    SourceList = dict:fold(fun(K,V,A) -> [{K, V#topic.sourceList}|A] end,[], S#state.topics),
            Pid ! {MsgRef, SourceList},
            loop(S);
        {Pid, MsgRef, {topics, details, TopicId, UserId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    S1 = add_user(S, UserId),
	    User = dict:fetch(UserId, S1#state.users),
	    ReadStories = dict:fetch(TopicId, User#user.readStories),
	    LatestStory = Topic#topic.lastStoryId,
	    Pid ! {MsgRef, Topic#topic.name, LatestStory - length(ReadStories)},
	    loop(S1);
        {Pid, MsgRef, {user, pref, UserId}} ->
	    S1 = add_user(S, UserId),
	    User = dict:fetch(UserId, S1#state.users),
	    Pid ! {MsgRef, User#user.prefTopics},
	    loop(S1);
        {Pid, MsgRef, {user, pref, set, UserId, Pref}} ->
	    S1 = add_user(S, UserId),
	    User = dict:fetch(UserId, S1#state.users),
	    NewUser = User#user{prefTopics=Pref},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
	    Pid ! {MsgRef, true},
            loop(S#state{users=NewUsers});
        {Pid, MsgRef, {story, next, TopicId, UserId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    User = dict:fetch(UserId, S#state.users),
            ReadStories = dict:fetch(TopicId, User#user.readStories),
	    LatestStory = Topic#topic.lastStoryId,
	    CurrentStory = find_next(ReadStories, LatestStory),
	    Pid ! {MsgRef, CurrentStory},
	    NewUser = User#user{currentStoryId=CurrentStory},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
            loop(S#state{users=NewUsers});
        {Pid, MsgRef, {story, previous, TopicId, UserId}} ->
	    User = dict:fetch(UserId, S#state.users),
            ReadStories = dict:fetch(TopicId, User#user.readStories),
	    Pid ! {MsgRef, find_previous(ReadStories)},
            loop(S);
        {Pid, MsgRef, {story, peek, TopicId, UserId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    User = dict:fetch(UserId, S#state.users),
            ReadStories = dict:fetch(TopicId, User#user.readStories),
	    LatestStory = Topic#topic.lastStoryId,
	    CurrentStory = User#user.currentStoryId,
	    Pid ! {MsgRef, find_peek(ReadStories, CurrentStory, LatestStory)},
            loop(S);
        {Pid, MsgRef, {story, get, TopicId, StoryId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    Story = dict:fetch(StoryId, Topic#topic.storyList),
            Pid ! {MsgRef, Story#story.headlineText, Story#story.guid, Story#story.date, Story#story.source},
            loop(S);
        {Pid, MsgRef, {story, details, TopicId, StoryId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    Story = dict:fetch(StoryId, Topic#topic.storyList),
            Pid ! {MsgRef, Story#story.detailText, Story#story.link},
            loop(S);
        {Pid, MsgRef, {story, mark, TopicId, StoryId, UserId}} ->
	    User = dict:fetch(UserId, S#state.users),
	    ReadStories = dict:fetch(TopicId, User#user.readStories),
	    NewReadStories = dict:store(TopicId, [StoryId | ReadStories], User#user.readStories),
	    NewUser = User#user{readStories=NewReadStories},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
            Pid ! {MsgRef, true},
            loop(S#state{users=NewUsers});
        {Pid, MsgRef, {story, add, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}}} ->
	    S1 = add_story(S, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}),
            Pid ! {MsgRef, true},
            loop(S1);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    end.


add_story(S, TopicId, {HeadlineText, Guid, Link, DetailText, Date, Source}) ->
    Topic = dict:fetch(TopicId, S#state.topics),
    StoryList = dict:to_list(Topic#topic.storyList),    
    case lists:any(fun({_, Story}) -> Story#story.guid == list_to_binary(Guid) end, StoryList) of
	false ->
	    io:format("Processing update~n"),
	    NewStoryId = Topic#topic.lastStoryId + 1,
	    NewStory = #story{headlineText=list_to_binary(HeadlineText),date=list_to_binary(Date), source=list_to_binary(Source), guid=list_to_binary(Guid), link=list_to_binary(Link), detailText=list_to_binary(DetailText)},
	    NewStoryList = dict:store(NewStoryId, NewStory, Topic#topic.storyList),
	    NewTopic = Topic#topic{storyList=NewStoryList, lastStoryId=NewStoryId},
	    NewTopics = dict:store(TopicId, NewTopic, S#state.topics),
	    S#state{topics=NewTopics};
	true ->
	    S
    end.

add_user(S, UserId) ->
    case dict:find(UserId, S#state.users) of
	{ok, _} -> 
	    S;
	error -> 
	    NewUser = #user{prefTopics=[1,2,3,4,5], readStories=dict:from_list([{1, []}, {2, []}, {3, []}, {4, []}, {5, []}])},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
	    S#state{users=NewUsers}
    end.
		
find_next(_, 0) -> 0;
find_next(ReadStories, LatestStory) ->
    case lists:member(LatestStory, ReadStories) of
	false -> LatestStory;
	true -> find_next(ReadStories, LatestStory - 1)
    end.

find_peek(_, _,0) -> 0;
find_peek(ReadStories, CurrentStory, LatestStory) ->
    case LatestStory == CurrentStory of
	false ->
	    case lists:member(LatestStory, ReadStories) of
		false -> LatestStory;
		true -> find_peek(ReadStories, CurrentStory, LatestStory - 1)
	    end;
	true ->
	    find_peek(ReadStories, CurrentStory, LatestStory - 1)
    end.

find_previous([]) -> 0;
find_previous([Last | _ReadStories]) ->
    Last.
