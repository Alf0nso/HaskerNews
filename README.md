# Hacker news extractor
Extract information from the Hacker news web site with Haskell!
This tiny project aims at using the library Scalpel to scrap the 
front page of the Hacker News website and allowing to do some 
filtering and ordering of the scraped posts.

Build the project:
```bash
$ stack build
```

Run the project:
```bash
$ stack run
```

This will give access to a prompt where some commands can be used to 
perform the filtering, ordering, and displaying the posts:
```bash
> Enter your commands: h 
All the commands: 
q | quit       -- quiting the program.
p | print      -- printing Hacker news posts to the
                  terminal.
c | clear      -- Cleans all the changes done to the data
                  returning to the original requested data.
f | filterWith -- Filter data with a specific filter function
                  which can be for example: filter > 5.
o | orderBy    -- Order data by Rank, Points or number of
                  comments, example: orderBy rank
h | help       -- prints this message.
```

Filter all posts with more than five words in the title ordered by the number of comments first:
```bash
> Enter your commands: filterWith > 5
> Enter your commands: orderBy comments
> Enter your commands: p
Title:     Intel tapes out chips on 1.8nm and 2nm production nodes (tomshardware.com)
Rank:      24
Points:    17
Comments:  1
----------------------------------
Title:     RJIT, a new JIT for Ruby (github.com/ruby)
Rank:      12
Points:    18
Comments:  4
----------------------------------
Title:     Effortless Performance Improvements in C++: std:vector (jorge.st)
Rank:      14
Points:    40
Comments:  4
----------------------------------
Title:     Launch HN: UpTrain (YC W23) – Open-source performance monitoring for ML models
Rank:      17
Points:    28
Comments:  7
----------------------------------
Title:     Code coverage for Go integration tests (go.dev)
Rank:      9
Points:    74
Comments:  8
----------------------------------
Title:     FTC bars GoodRx from sharing consumers’ sensitive health info for advertising (ftc.gov)
Rank:      28
Points:    24
Comments:  12
----------------------------------
(...)
```

A simple interaction where the user filters all posts with less than or 
equal to five words in the title ordered by points:
```bash
> Enter your commands: filterWith <= 5
> Enter your commands: p
Title:     Microphones (coutant.org)
Rank:      8
Points:    35
Comments:  8
----------------------------------
Title:     Videocard Virtual Museum (vgamuseum.ru)
Rank:      17
Points:    78
Comments:  26
----------------------------------
Title:     Plankalkül (wikipedia.org)
Rank:      20
Points:    59
Comments:  49
----------------------------------

> Enter your commands: orderBy points
> Enter your commands: p
Title:     Microphones (coutant.org)
Rank:      8
Points:    35
Comments:  8
----------------------------------
Title:     Plankalkül (wikipedia.org)
Rank:      20
Points:    59
Comments:  49
----------------------------------
Title:     Videocard Virtual Museum (vgamuseum.ru)
Rank:      17
Points:    78
Comments:  26
----------------------------------
```
