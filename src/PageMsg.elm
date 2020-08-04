module PageMsg exposing (PageMsg(..))


type PageMsg
    = None
    | ShowLogin
    | ShowRegister
    | Login String
