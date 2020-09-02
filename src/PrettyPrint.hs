module PrettyPrint where

import AST

-- prettyPrint :: Program -> String
-- prettyPrint rootNode =
--     case rootNode of
--         Procedure -> "[Procedure]"

-- prettyPrint :: Program -> IO ()
-- prettyPrint (Program records arrs procs) =
--   do
--     map printRecord records

-- printRecord :: Record -> IO ()
-- printRecord record =
--   firstRec:nextRecs
--   where firstRec:nextRecs = records

prettyPrint :: Program -> String
prettyPrint (Program recordDefs arrayDefs mainProc) =
    unlines [
        unlines recLines,
        unlines arrayLines,
        unlines mainProcLines
    ]
    where
        recLines = ["sample", "    rec", "    lines"]
        arrayLines = ["sample", "    array", "    lines"]
        mainProcLines = ["sample", "    main", "    lines"]
    -- case rootNode of
    --     Procedure -> "[Procedure]"