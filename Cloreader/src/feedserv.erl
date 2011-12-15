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
    {feed, Source0, Url0, Stories0} = feedparser:parse_url("http://timesofindia.feedsportal.com/c/33039/f/533916/index.rss"),
    insert_stories(1, Stories0, Source0),
    {feed, Source, Url, Stories} = feedparser:parse_url("http://rss.cnn.com/rss/edition.rss"),
    insert_stories(1, Stories, Source),
    {feed, Source1, Url1, Stories1} = feedparser:parse_url("http://feeds.feedburner.com/TechCrunch/"),
    insert_stories(1, Stories1, Source1),
    timer:sleep(30000),
    loop().

insert_stories(_,[],_) -> ok;
insert_stories(TopicId, [{feedentry, HT, Date, Link, DT} | Rest], Source) ->
    newsserv:story_add(TopicId, HT, Link, Link, DT, Date, Source),
    insert_stories(TopicId, Rest, Source).
    
