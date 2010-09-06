make
metafeed:addq("t1", {filter, contains, "Apps", ["description"], {fetch, "http://feeds.feedburner.com/readwriteweb"}}).
metafeed:addq("t2", {fetch, pipe, "t1"}).
metafeed:runq("t1").
metafeed:runq("t2").
