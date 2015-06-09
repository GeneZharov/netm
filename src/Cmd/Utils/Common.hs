module Utils.Common where


etcDir        = "/etc/netm/"              -- каталог конфигов пользователя
statusFile    = "/var/lib/netm/status"    -- имена работающих соединений
hierarchyFile = "/var/lib/netm/hierarchy" -- иерархния работающих соединений


type Abbr   = String -- сокращение имени конфига, например n/w
type Name   = String -- полное имя конфига, например nest/wlan

-- Иерархия конфигов
type Parent   = Name
type Child    = Name
type Relation = (Child, Parent)


-- Опции командной строки
data Option =

    -- common
      Help
    | Quiet
    | Timeout Int
    | NoCompletion

    -- netd
    | Suspend

    -- netu
    | Resume
    | Owner String

    deriving (Eq, Show)
