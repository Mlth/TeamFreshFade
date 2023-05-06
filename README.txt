The bot can build words both vertically and horizontally. 
It handles wildcards, but if a player has two wildcards, the second wildcard will be treated as a tile with the letter 'A'. 
The bot can build words through tiles already placed on the board. 
The bot cannot build words in parallel to words already placed next to it on the board. 
For some tiles placed in an order on the board, the bot will only find words that have a suffix or prefix that matches those tiles (Since we have not implemented a GADDAG).
In some cases, it can beat the Oxyphenbutazone bot ;)

Multi-player and dictionary (yes/no): yes, we have implemented a Trie which can be found in the ScrabbleBot folder. Our bot also supports multiplayer and keeps track of whose turn it is. Additionally, it can handle other players forfeiting.
Playing on all boards (yes/no): no
Parallelism (yes/no): no
Respect the timeout flag (yes/no): no