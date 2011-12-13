%% Event server
-module(newsserv).
-export([start/0, terminate/0, init/0, loop/1,
         topics_all/0, topics_details/2, story_next/2, story_get/2, story_details/2, story_mark/3, find_next/2]).

-record(state, {topics, 
                users}).

-record(topic, {name="",
                lastStoryId="0",
                storyList=dict:new()}).

-record(story, {headlineText="",
                time="",
                source="",
		detailText="",
		image=""}).

-record(user, {readStories=dict:new()}).

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

    TechStory1 = #story{headlineText=list_to_binary("Tech1"),detailText=list_to_binary("TechDet1")},
    TechStory2 = #story{headlineText=list_to_binary("Tech2"),detailText=list_to_binary("TechDet2")},
    TechStory3 = #story{headlineText=list_to_binary("Tech3"),detailText=list_to_binary("TechDet3")},
    TechStory4 = #story{headlineText=list_to_binary("Tech4"),detailText=list_to_binary("TechDet4")},
    SportsStory1 = #story{headlineText=list_to_binary("Sports1"),detailText=list_to_binary("StoryDet1")},
    SportsStory2 = #story{headlineText=list_to_binary("Sports2"),detailText=list_to_binary("StoryDet2")},
    SportsStory3 = #story{headlineText=list_to_binary("Sports3"),detailText=list_to_binary("StoryDet3")},
    HydStory1 = #story{headlineText=list_to_binary("Hyd1"),detailText=list_to_binary("HydDet1")},
    HydStory2 = #story{headlineText=list_to_binary("Hyd2"),detailText=list_to_binary("HydDet2")},
    HydStory3 = #story{headlineText=list_to_binary("Hyd3"),detailText=list_to_binary("HydDet3")},

    Topic1 = #topic{name=list_to_binary("Technology"), lastStoryId=4, storyList=dict:from_list([{1, TechStory1}, {2, TechStory2}, {3, TechStory3}, {4, TechStory4}])},
    Topic2 = #topic{name=list_to_binary("Sports"), lastStoryId=3, storyList=dict:from_list([{1, SportsStory1}, {2, SportsStory2}, {3, SportsStory3}])},
    Topic3 = #topic{name=list_to_binary("Hyderabad"), lastStoryId=3, storyList=dict:from_list([{1, HydStory1}, {2, HydStory2}, {3, HydStory3}])},

    User1 = #user{readStories=dict:from_list([{1, [1,3]}, {2, [1,2,3]}, {3, []}])},
    User2 = #user{readStories=dict:from_list([{1, [1, 2, 4]}, {2, [1]}, {3, [1]}])},

    loop(#state{topics=dict:from_list([{1, Topic1}, {2, Topic2}, {3, Topic3}]), users=dict:from_list([{1, User1}, {2, User2}])}).

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

story_next(TopicId, UserId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, next, TopicId, UserId}},
    receive
        {Ref, Msg} -> [{nextStoryId, Msg}]
    after 5000 ->
        {error, timeout}


    end.

story_get(TopicId, StoryId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, get, TopicId, StoryId}},
    receive
        {Ref, Msg} -> [{headlineText, Msg}]
    after 5000 ->
        {error, timeout}
    end.

story_details(TopicId, StoryId) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {story, details, TopicId, StoryId}},
    receive
        {Ref, Msg} -> [{detailText, Msg}]
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

%%% The Server itself

loop(S=#state{}) ->
    receive
        {Pid, MsgRef, {topics, all}} ->
	    Names = dict:fold(fun(K,V,A) -> [[{topicId, K}, {name, V#topic.name}]|A] end,[], S#state.topics),
            Pid ! {MsgRef, Names},
            loop(S);
        {Pid, MsgRef, {topics, details, TopicId, UserId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    S1 = add_user(S, UserId),
	    User = dict:fetch(UserId, S1#state.users),
	    io:format("User : ~w", [User]),
	    ReadStories = dict:fetch(TopicId, User#user.readStories),
	    LatestStory = Topic#topic.lastStoryId,
	    Pid ! {MsgRef, Topic#topic.name, LatestStory - length(ReadStories)},
	    loop(S1);
        {Pid, MsgRef, {story, next, TopicId, UserId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    User = dict:fetch(UserId, S#state.users),
            ReadStories = dict:fetch(TopicId, User#user.readStories),
	    LatestStory = Topic#topic.lastStoryId,
	    Pid ! {MsgRef, find_next(ReadStories, LatestStory)},
            loop(S);
        {Pid, MsgRef, {story, get, TopicId, StoryId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    Story = dict:fetch(StoryId, Topic#topic.storyList),
            Pid ! {MsgRef, Story#story.headlineText},
            loop(S);
        {Pid, MsgRef, {story, details, TopicId, StoryId}} ->
	    Topic = dict:fetch(TopicId, S#state.topics),
	    Story = dict:fetch(StoryId, Topic#topic.storyList),
            Pid ! {MsgRef, Story#story.detailText},
            loop(S);
        {Pid, MsgRef, {story, mark, TopicId, StoryId, UserId}} ->
	    User = dict:fetch(UserId, S#state.users),
	    ReadStories = dict:fetch(TopicId, User#user.readStories),
	    NewReadStories = dict:store(TopicId, [StoryId | ReadStories], User#user.readStories),
	    NewUser = User#user{readStories=NewReadStories},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
            Pid ! {MsgRef, true},
            loop(S#state{users=NewUsers});
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    end.

add_user(S, UserId) ->
    case dict:find(UserId, S#state.users) of
	{ok, _} -> 
	    S;
	error -> 
	    NewUser = #user{readStories=dict:from_list([{1, []}, {2, []}, {3, []}])},
	    NewUsers = dict:store(UserId, NewUser, S#state.users),
	    S#state{users=NewUsers}
    end.
		
find_next(_, 0) -> 0;
find_next(ReadStories, LatestStory) ->
    case lists:member(LatestStory, ReadStories) of
	false -> LatestStory;
	true -> find_next(ReadStories, LatestStory - 1)
    end.

    
