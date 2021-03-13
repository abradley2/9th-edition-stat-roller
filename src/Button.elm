module Button exposing (..)


baseButtonClass : String
baseButtonClass =
    "black-80 ba b--light-blue bg-light-blue f6 "
        ++ " hover-bg-transparent hover-white grow "
        ++ " pointer outline-0 br2 pa2 shadow-1 "


disabledButtonClass : String
disabledButtonClass =
        "white-40 ba b--white-50 b--dashed bg-transparent f6 "
        ++ " hover-bg-transparent "
        ++ " pointer outline-0 br2 pa2 shadow-1 "
