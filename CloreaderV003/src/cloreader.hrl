
-record(state, {
          topics,
          users=dict:new(),
	  storyStack=[]
         }
       ).

-record(topic, {
          name,
          sourceList,
          storyList=dict:new(),
	  lastStoryId=0,
	  sourcePriority=dict:new()
         }
       ).

-record(story, {
          headlineText="",
          date="",
          source="",
          detailText="",
          guid="",
          link=""
         }
       ).

-record(user, {
	  prefTopics=dict:new(),
          readStories=[],
	  storyStack=[],
	  nextStories=[],
	  current={0,0}
         }
       ).
