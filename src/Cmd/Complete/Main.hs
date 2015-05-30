-- Формирует из сокращения имени конфига его полное имя.
-- Используется shell-скриптами для дополнения аргументов других команд.
--
-- $1 — сокращение имени конфига, если не задано, то выведет все доступные 
-- имена конфигов. Содержимое каталогов начинающихся с подчёркивания 
-- игнорируется. Выводит на stdout имена конфигов через \n
--
-- Для дополнения аргументов используется тот же алгоритм раскрытия сокращений 
-- имён конфигов, что и в других хаскельных программах, так как это во-первых 
-- проще, чем реализовывать его снова в каждом шэле, во-вторых надёжнее.


import Control.Monad
import Data.List (isPrefixOf)
import System.Environment (getArgs)

import Utils


main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [abbr] -> complete abbr
        _      -> complete "*"


complete :: String -> IO ()
complete abbr = do
    (_, confs) <- getFiles abbr
    let confs' = filter (not . isPrefixOf "_") confs
    mapM_ putStrLn confs'
