-module(path_tree).
-author('Ryah Dahl <ry@tinyclouds.org>').

-export([create_subtree/2, store/3, find/2]).
-export([test/0]).

-record(branch, { name, data, subtree }).

find([], _) -> not_found;
find([Branch|RestOfTree], Path) ->
    [PathComponent|RestOfPath] = Path, 
    if Branch#branch.name == PathComponent ->
        if RestOfPath == [] ->
            {ok, Branch#branch.data};
        true ->
            find(Branch#branch.subtree, RestOfPath)
        end;
    true -> 
        find(RestOfTree, Path)
    end.

create_subtree([LastPathComponent], Data) -> 
  [#branch{ name = LastPathComponent, 
            subtree = [], 
            data = Data 
          }
  ];

create_subtree([PathComponent | RestOfPath], Data) ->
  [#branch{ name = PathComponent, 
            subtree = create_subtree(RestOfPath, Data), 
            data = nil 
          }
  ].

store(Tree, [], _) -> Tree;

store([], Path, Data) -> 
  create_subtree(Path, Data);

store([Branch | RestOfTree], Path, Data) ->
  %io:format("~nBranch: ~p~nPath: ~p~n~n", [Branch, Path]),
  [PathComponent | RestOfPath] = Path,
  case Branch#branch.name == PathComponent of
    true ->
      NewBranch = 
      case RestOfPath of 
        [] ->
          Branch#branch{ data = Data };
        _ ->
          NewSubTree = store(Branch#branch.subtree, RestOfPath, Data),
          Branch#branch{ subtree = NewSubTree }
      end,
      [NewBranch | RestOfTree];
    false -> 
      [Branch | store(RestOfTree, Path, Data)]
  end.

test() -> 
  ok = test_storage(),
  ok.

test_storage() -> 
    [ #branch{ name = "hello", data = "egg", subtree = [] } ] 
      = store([], ["hello"], "egg"),

    HelloWorldDB = 
      [ #branch{ name = "hello", data = nil, subtree = 
          [ #branch{ name = "world", data = "egg", subtree = [] }]
        }
      ],

    HelloWorldDB = store([], ["hello", "world"], "egg"),

    HelloWorldMotherDB =
      [ #branch{ name = "hello", data = nil, subtree = 
          [ #branch{ name = "world", data = "egg", subtree = [] },
            #branch{ name = "lisa", data = "salt", subtree = [] }
          ]
        }
      ],
    HelloWorldMotherDB  = store(HelloWorldDB, ["hello", "lisa"], "salt"),

    TabascoDB =
      [ #branch{ name = "hello", data = nil, subtree = 
          [ #branch{ name = "world", data = "egg", subtree = [] },
            #branch{ name = "lisa", data = "salt", subtree = [] }
          ]
        }, 
        #branch{ name = "another", data = nil, subtree = 
          [ #branch{ name = "world", data = "tabasco", subtree = [] } ]
        }
      ], 
    TabascoDB = store(HelloWorldMotherDB, ["another", "world"], "tabasco"),

    %io:format("~n~nDEEP~n", []),
    %io:format("deep: ~p~n",[ store(TabascoDB, ["hello", "lisa", "deep"], "milk")]), 

    DeepDB =
      [ #branch{ name = "hello", data = nil, subtree = 
          [ #branch{ name = "world", data = "egg", subtree = [] },
            #branch{ name = "lisa", data = "salt", subtree = 
              [ #branch{ name = "deep", data = "milk", subtree = [] } ]
            }
          ]
        }, 
        #branch{ name = "another", data = nil, subtree = 
          [ #branch{ name = "world", data = "tabasco", subtree = [] } ]
        }
      ], 
    DeepDB = store(TabascoDB, ["hello", "lisa", "deep"], "milk"),

    % do it again 
    TabascoDB = store(HelloWorldMotherDB, ["another", "world"], "tabasco"),



    [ #branch{ name = "hello", data = nil, subtree = 
        [ #branch{ name = "world", data = "egg", subtree = [] },
          #branch{ name = "lisa", data = "salt", subtree = [] }
        ]
      }, 
      #branch{ name = "another", data = nil, subtree = 
        [ #branch{ name = "world", data = "noodles", subtree = [] } ]
      }
    ]  =
    store(HelloWorldMotherDB, ["another", "world"], "noodles"),


    [ #branch{ name = "hello", data = nil, subtree = 
        [ #branch{ name = "world", data = "egg", subtree = [] },
          #branch{ name = "lisa", data = "salt", subtree = [] }
        ]
      }, 
      #branch{ name = "another", data = "noodles", subtree = 
        [ #branch{ name = "world", data = "tabasco", subtree = [] } ]
      }
    ] =
    store(TabascoDB, ["another"], "noodles"),

    ok.
