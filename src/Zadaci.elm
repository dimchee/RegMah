module Zadaci exposing (TestCase, Zadatak, isti, passAll, zadaci)

import Array
import Eval
import Lang


type alias TestCase =
    { input : Eval.Registers
    , output : Eval.Registers
    }


type alias Zadatak =
    { text : String
    , testCases : List TestCase
    }


isti : Eval.Registers -> Eval.Registers -> Bool
isti regs1 regs2 =
    List.map2 (==) (Array.toList regs1) (Array.toList regs2) |> List.all identity


pass : Lang.Program -> TestCase -> Bool
pass program { input, output } =
    Eval.eval (Array.map Just input) program |> isti output


passAll : Int -> Lang.Program -> Bool
passAll n program =
    case Array.get n zadaci of
        Just { testCases } ->
            testCases |> List.map (pass program) |> List.all identity

        Nothing ->
            False


zadaci : Array.Array Zadatak
zadaci =
    Array.fromList
        [ { text = "isprazniti registar 3"
          , testCases =
                [ { input = Array.fromList [ 0, 0, 4 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 22 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 51 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 75 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 125 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 251 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 621 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 721 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 851 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 900 ]
                  , output = Array.fromList [ 0, 0, 0 ]
                  }
                ]
          }
        , { text = "prebaciti vrednost iz registra 2 u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 1 ]
                  , output = Array.fromList [ 1, 0 ]
                  }
                , { input = Array.fromList [ 0, 22 ]
                  , output = Array.fromList [ 22, 0 ]
                  }
                , { input = Array.fromList [ 0, 51 ]
                  , output = Array.fromList [ 51, 0 ]
                  }
                , { input = Array.fromList [ 0, 75 ]
                  , output = Array.fromList [ 75, 0 ]
                  }
                , { input = Array.fromList [ 0, 125 ]
                  , output = Array.fromList [ 125, 0 ]
                  }
                , { input = Array.fromList [ 0, 251 ]
                  , output = Array.fromList [ 251, 0 ]
                  }
                , { input = Array.fromList [ 0, 621 ]
                  , output = Array.fromList [ 621, 0 ]
                  }
                , { input = Array.fromList [ 0, 721 ]
                  , output = Array.fromList [ 721, 0 ]
                  }
                , { input = Array.fromList [ 0, 851 ]
                  , output = Array.fromList [ 851, 0 ]
                  }
                , { input = Array.fromList [ 0, 900 ]
                  , output = Array.fromList [ 900, 0 ]
                  }
                ]
          }
        , { text = "prepisati vrednost iz registra 2 u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 1 ]
                  , output = Array.fromList [ 1, 1 ]
                  }
                , { input = Array.fromList [ 0, 22 ]
                  , output = Array.fromList [ 22, 22 ]
                  }
                , { input = Array.fromList [ 0, 51 ]
                  , output = Array.fromList [ 51, 51 ]
                  }
                , { input = Array.fromList [ 0, 75 ]
                  , output = Array.fromList [ 75, 75 ]
                  }
                , { input = Array.fromList [ 0, 125 ]
                  , output = Array.fromList [ 125, 125 ]
                  }
                , { input = Array.fromList [ 0, 251 ]
                  , output = Array.fromList [ 251, 251 ]
                  }
                , { input = Array.fromList [ 0, 621 ]
                  , output = Array.fromList [ 621, 621 ]
                  }
                , { input = Array.fromList [ 0, 721 ]
                  , output = Array.fromList [ 721, 721 ]
                  }
                , { input = Array.fromList [ 0, 851 ]
                  , output = Array.fromList [ 851, 851 ]
                  }
                , { input = Array.fromList [ 0, 900 ]
                  , output = Array.fromList [ 900, 900 ]
                  }
                ]
          }
        , { text = "sabrati brojeve u registrima 2 i 3 i rezultat upisati u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 12, 15 ]
                  , output = Array.fromList [ 27 ]
                  }
                , { input = Array.fromList [ 0, 23, 54 ]
                  , output = Array.fromList [ 77 ]
                  }
                , { input = Array.fromList [ 0, 18, 30 ]
                  , output = Array.fromList [ 48 ]
                  }
                , { input = Array.fromList [ 0, 13, 15 ]
                  , output = Array.fromList [ 28 ]
                  }
                , { input = Array.fromList [ 0, 3, 6 ]
                  , output = Array.fromList [ 9 ]
                  }
                ]
          }
        , { text = "pomnožiti brojeve u registrima 2 i 3 i rezultat upisati u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 12, 15 ]
                  , output = Array.fromList [ 180 ]
                  }
                , { input = Array.fromList [ 0, 23, 26 ]
                  , output = Array.fromList [ 598 ]
                  }
                , { input = Array.fromList [ 0, 63, 12 ]
                  , output = Array.fromList [ 756 ]
                  }
                , { input = Array.fromList [ 0, 27, 15 ]
                  , output = Array.fromList [ 405 ]
                  }
                , { input = Array.fromList [ 0, 3, 33 ]
                  , output = Array.fromList [ 99 ]
                  }
                ]
          }
        , { text = "oduzeti broj iz registra 3 od broja u registru 2 i apsolutnu vrednost razlike upisati u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 12, 15 ]
                  , output = Array.fromList [ 3 ]
                  }
                , { input = Array.fromList [ 0, 54, 23 ]
                  , output = Array.fromList [ 31 ]
                  }
                , { input = Array.fromList [ 0, 163, 312 ]
                  , output = Array.fromList [ 149 ]
                  }
                , { input = Array.fromList [ 0, 427, 315 ]
                  , output = Array.fromList [ 112 ]
                  }
                , { input = Array.fromList [ 0, 333, 642 ]
                  , output = Array.fromList [ 309 ]
                  }
                ]
          }
        , { text = "podeliti broj iz registra 3 brojem iz registra 2, i količnik upisati u registar 1"
          , testCases =
                [ { input = Array.fromList [ 0, 12, 15 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 54, 23 ]
                  , output = Array.fromList [ 2 ]
                  }
                , { input = Array.fromList [ 0, 163, 3 ]
                  , output = Array.fromList [ 54 ]
                  }
                , { input = Array.fromList [ 0, 427, 315 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 19, 4 ]
                  , output = Array.fromList [ 4 ]
                  }
                ]
          }
        , { text = "podeliti broj iz registra 4 brojem iz registra 3, količnik upisati u registar 1, a ostatak u registar 2"
          , testCases =
                [ { input = Array.fromList [ 0, 0, 12, 15 ]
                  , output = Array.fromList [ 0, 12 ]
                  }
                , { input = Array.fromList [ 0, 0, 54, 23 ]
                  , output = Array.fromList [ 2, 8 ]
                  }
                , { input = Array.fromList [ 0, 0, 163, 3 ]
                  , output = Array.fromList [ 54, 1 ]
                  }
                , { input = Array.fromList [ 0, 0, 427, 315 ]
                  , output = Array.fromList [ 1, 112 ]
                  }
                , { input = Array.fromList [ 0, 0, 19, 4 ]
                  , output = Array.fromList [ 4, 3 ]
                  }
                ]
          }
        , { text = "U registrima 1 2 3 upisati poslednje 3 cifre broja upisanog u registar 4"
          , testCases =
                [ { input = Array.fromList [ 0, 0, 0, 5212 ]
                  , output = Array.fromList [ 2, 1, 2 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 15631 ]
                  , output = Array.fromList [ 6, 3, 1 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 5324 ]
                  , output = Array.fromList [ 3, 2, 4 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 93243 ]
                  , output = Array.fromList [ 2, 4, 3 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 4125 ]
                  , output = Array.fromList [ 1, 2, 5 ]
                  }
                ]
          }
        , { text = "U registre 1, 2, 3, 4, 5, 6 upisati prvih 6 cifara binarnog zapisa broja upisanog u registar 7"
          , testCases =
                [ { input = Array.fromList [ 0, 0, 0, 0, 0, 0, 8 ]
                  , output = Array.fromList [ 0, 0, 1, 0, 0, 0 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 0, 0, 0, 15 ]
                  , output = Array.fromList [ 0, 0, 1, 1, 1, 1 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 0, 0, 0, 23 ]
                  , output = Array.fromList [ 0, 1, 0, 1, 1, 1 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 0, 0, 0, 31 ]
                  , output = Array.fromList [ 0, 1, 1, 1, 1, 1 ]
                  }
                , { input = Array.fromList [ 0, 0, 0, 0, 0, 0, 30 ]
                  , output = Array.fromList [ 0, 1, 1, 1, 1, 0 ]
                  }
                ]
          }
        , { text = "ispitati da li je broj u registru 2 prost, i ako jeste upisati 1 u registar 1, u suprotnom upisati 0"
          , testCases =
                [ { input = Array.fromList [ 0, 5 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 22 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 17 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 75 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 23 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 51 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 88 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 100 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 91 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 12 ]
                  , output = Array.fromList [ 0 ]
                  }
                ]
          }
        , { text = "ispitati da li brojevi u registrima 2, 3 i 4 čine pitagorinu trojku, i ako čine u registar 1 upisati 1, u suprotnom 0"
          , testCases =
                [ { input = Array.fromList [ 0, 3, 4, 5 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 15, 13, 12 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 8, 6, 10 ]
                  , output = Array.fromList [ 1 ]
                  }
                , { input = Array.fromList [ 0, 1, 5, 4 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 6, 2, 7 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 6, 2, 10 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 7, 5, 10 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 11, 2, 3 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 1, 6, 15 ]
                  , output = Array.fromList [ 0 ]
                  }
                , { input = Array.fromList [ 0, 5, 4, 8 ]
                  , output = Array.fromList [ 0 ]
                  }
                ]
          }
        ]
