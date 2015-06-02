module Utils.Options where

import System.Console.GetOpt


data Option = Help
            | Quiet
            | Timeout Int
            | Suspend
            | Resume
            | NoCompletion
            deriving (Eq, Show)


commonOptions :: [OptDescr Option]
commonOptions =
  [ Option "q" ["quiet"]   (NoArg Quiet)
     "Подавить вывод stdout"
  , Option "t" ["timeout"] (ReqArg (Timeout . read) "INT")
     "Время ожидания пользовательского скрипта"
  , Option "C" ["--no-completion"] (NoArg NoCompletion)
     "Не выполнять дополнение имён конфигов"
  , Option "h" ["help"] (NoArg Help)
     "Распечатать API команды"
  ]


-- Формирует время ожидания на основе списка опций
getTimeout :: [Option] -> Int
getTimeout (Timeout t:os) = t
getTimeout (_:os)         = getTimeout os
getTimeout []             = 120 -- по умолчанию 2 минуты
