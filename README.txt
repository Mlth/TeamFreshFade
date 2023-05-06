Our bot can build words both vertically and horizontally. It handles wildcards, but if a player has two wildcards, the second wildcard will be treated as the letter 'A'. The bot can build words through tiles already placed on the board. The bot cannot build words in parallel to words already placed on the board. It can also not build words that contain some already placed tile in the middle of the word (i.e. it only builds words that start or end with letters placed on the board).

Multi-player and dictionary (yes/no): yes, we have implemented a Trie which can be found in the ScrabbleBot folder. Our bot also supports multiplayer and keeps track of whose turn it is.
Playing on all boards (yes/no): no
Parallelism (yes/no): no
Respect the timeout flag (yes/no): no