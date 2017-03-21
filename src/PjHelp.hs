module PjHelp
    ( pjhelp
    , printHelp
    ) where

pjhelp :: String -> [String]

pjhelp ""       =   [ "pj - filesystem path labeling program"
                    , "Usage:"
                    , "  pj <name>                         - navigates to a path labeled as <name>"
                    , "  pj add <name> <path>              - adds a label <name> for <path>"
                    , "  pj rm <name1> [, <name2> [, ...]] - removes labels with given names"
                    , "  pj list                           - lists all existing labels"
                    , "  pj --help                         - prints this message"
                    , "Type pj <cmd> --help for help about commands"
                    ]
pjhelp "add"    =   [ "pj add - create a new filesystem path label or update existing"
                    , "Usage:"
                    , "  pj add <name> <path>"
                    , "  - <name>: a unique label name (string, should not contain whitespace)"
                    , "  - <path>: filesystem path to label with <name>"
                    , "If a label with a given name already exists, its path will be changed."
                    , "If there is already a label for <path>, another label will be added for it."
                    , "Type pj --help for more common help message"
                    ]
pjhelp "rm"     =   [ "pj rm - remove a filesystem path label or labels"
                    , "Usage:"
                    , "  pj rm <name1> [, <name2> [, ...]]"
                    , "Command removes all labels with names exact <name1>, <name2>, etc."
                    , "If there are no labels with given names, nothing will happen."
                    , "Type pj --help for more common help message"
                    ]
pjhelp "list"   =   [ "pj list - list all existing filesystem path labels"
                    , "Usage:"
                    , "  pj list"
                    , "Prints all created labels and their count."
                    , "Type pj --help for more common help message"
                    ]

pjhelp _ = ["Cannot show help for such query"]

printHelp :: String -> IO ()
printHelp cmd = mapM_ putStrLn $ pjhelp cmd
