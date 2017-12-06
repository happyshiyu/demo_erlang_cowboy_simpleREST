-define(APP, sample).

-define(WORKER(I), #{id => I
                    ,start => {I, start_link, []}
                    ,restart => permanent
                    ,shutdown => 5 * 1000
                    ,type => worker
                    ,modules => [I]
                    }).
