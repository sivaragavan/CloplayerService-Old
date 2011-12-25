%% Event server
-module(feedserv).
-export([start/0, terminate/0, init/0, loop/0]).

start() ->
    inets:start(),
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
    loop().

%%% The Server itself

loop() ->
    refresh_topics(newsserv:topics_sources()),
    timer:sleep(30000),
    loop().

refresh_topics([]) -> ok;
refresh_topics([{TopicId, Sources} | Rest]) ->
    refresh_sources(TopicId, Sources),
    refresh_topics(Rest).

refresh_sources(_,[]) -> ok;
refresh_sources(TopicId, [{SourceName, SourceUrl} | Rest]) ->
    {feed, _Source, _Url, Stories} = feedparser:parse_url(SourceUrl),
    insert_stories(TopicId, Stories, SourceName),
    refresh_sources(TopicId, Rest).

insert_stories(_,[],_) -> ok;
insert_stories(TopicId, [{feedentry, HT, Date, Link, DT} | Rest], Source) ->
    newsserv:story_add(TopicId, HT, Link, Link, DT, Date, Source),
    insert_stories(TopicId, Rest, Source).
    
