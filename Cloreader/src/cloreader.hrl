
-record(state, {
          topics,
          users=dict:new()
         }
       ).

-record(topic, {
          name="",
          lastStoryId=0,
          storyList=dict:new(),
          sourceList=[]
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
	  currentStoryId=0,
	  prefTopics,
          readStories=dict:new()
         }
       ).
