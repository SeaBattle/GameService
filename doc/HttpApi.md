# GameService http interface
### Create game
**/create** - create a new game.  
POST request with body JSON:

    {
        "vsn" : "ClientVersion",
        "uid" : "UserId",
        "rules" : [...{"RuleKey" : "RuleValue"}...],
        "ttl" : "GamePublishingTTL",
        "private" : true|false
    }
Where `vsn` is a **string** user's client version.  
`uid` is a **string** containing user id, by which this user can be found.  
`rules` is a list of objects with **string** key and **string** value. Is **optional**.  
`ttl` is a time, game is published. Is **optional**.  
`private` is a **boolean**. If set to true - other users can join this game only by game id.  

Success reply:  

    {
        "result" : true, "game_id" : "GameId", "code" : 0
    }

Error reply:

    {
        "result" : false, "code" : 400
    }   

### Connect to game
**/connect** user is connecting to game by game_id.

POST request with body JSON:

    {
        "game_id" : "GameId",
        "vsn" : "ClientVersion",
        "uid" : "UserId"
    }
Where `game_id` is a **string** id of a game, created by another user.    
`vsn` is a **string** user's client version.  
`uid` is a **string** containing user id, by which this user can be found.  

Success reply:  

    {
        "result" : true,
        "code" : 0
    }
    
Error reply:

    {
        "result" : false,
        "code" : 500
    }
    
### Fast play
**/play** user wait for nearest game to play. He just registers and server start 
a search in created games. If no games found - server searches through fast play users.

POST request with body JSON:

    {
        "vsn" : "ClientVersion",
        "uid" : "UserId",
        "rules" : [...{"RuleKey" : "RuleValue"}...],
        "ttl" : "GamePublishingTTL"
    }
Where `vsn` is a **string** user's client version.  
`uid` is a **string** containing user id, by which this user can be found.  
`rules` is a list of objects with **string** key and **string** value. Is **optional**.  
`ttl` is a time, game is published. Is **optional**.    

Success reply:  

    {
        "result" : true,
        "game_id" : "GameId",
        "uid" : : "HostUId",
        "code" : 0
    }
    
Error reply:

    {
        "result" : false,
        "code" : 500
    }

### List games
**/list** get a list of all created (and supported by client) games.
POST request with body JSON:

    {
        "vsn" : "ClientVersion",
        "rules" : [...{"RuleKey" : "RuleValue"}...],
        "limit" : "LimitResults"
    }
Where `vsn` is a **string** user's client version.  
`limit` is an **integer** value, limiting fetched results.  
`rules` is a list of objects with **string** key and **string** value. Is **optional**.  

Success reply:  

    {
        "result" : true,
        "games" : [{"game_id" : "GameId", "uid" : "CreatedUserId"}],
        "next_list" : true
    }
Where `next_list` **boolean** means, that there are more, than limit games.
    
Error reply:

    {
        "result" : false,
        "code" : 500
    }