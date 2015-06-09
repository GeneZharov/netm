import Data.Maybe (isNothing)

import Utils.Common
import Utils.Hierarchy (getChildren)
import Utils.Misc (load)


data Record = Record Name [Record] deriving (Show)
  -- Запись для вывода на экран
  -- Содержит имя конфига и набор дочерних записей для порождённых конфигов


main :: IO ()
main = do
   st <- load statusFile    :: IO [Name]
   hc <- load hierarchyFile :: IO [Relation]
   let model = build hc st
   let view  = format model
   putStr view


build :: [Relation] -> [Name] -> [Record]
build = build' Nothing
   where build' :: Maybe Parent -> [Relation] -> [Name] -> [Record]
         build' parent hc ns =
            [ Record n sub
                | n <- ns
                , parent == (n `lookup` hc)
                , let sub = build' (Just n) hc (getChildren hc n)
            ]


format :: [Record] -> String
format model = unlines (format' 0 model)
   where format' :: Int -> [Record] -> [String]
         format' level model = do
            Record name children <- model
            let indent = (level * 3) `replicate` ' '
            (indent ++ name) : format' (level + 1) children
