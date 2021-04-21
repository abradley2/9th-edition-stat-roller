module Button exposing (..)


baseButtonClass : String
baseButtonClass =
    "black-80 ba b--light-blue bg-light-blue f6 h2 border-box "
        ++ " hover-bg-transparent hover-white grow "
        ++ " pointer outline-0 br2 pa2 shadow-1 "


textButtonClass : String
textButtonClass =
    "light-blue f7 bn bg-transparent pointer "
      ++ " underline"


disabledButtonClass : String
disabledButtonClass =
        "white-40 ba b--white-50 b--dashed bg-transparent f6 h2 border-box "
        ++ " hover-bg-transparent "
        ++ " pointer outline-0 br2 pa2 shadow-1 "
