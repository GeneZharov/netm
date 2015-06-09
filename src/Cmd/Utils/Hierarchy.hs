module Utils.Hierarchy where

import Utils.Common


getChildren :: [Relation] -> Parent -> [Child]
getChildren hc parent = do
    (child, parent') <- hc
    [child | parent' == parent]


getDescendants :: [Relation] -> Parent -> [Child]
getDescendants hc parent = do
    child <- getChildren hc parent
    child : getDescendants hc child
