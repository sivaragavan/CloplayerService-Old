
-record(state, {
          topics,
          users=dict:new()
         }
       ).

-record(topic, {
          name,
          sourceList,
          storyList=dict:new(),
	  userList=[],
	  lastStoryId=0
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
	  prefTopics=[1,2,3,4,5],
          readStories=[],
	  storyStack=[],
	  current={0,0}
         }
       ).
