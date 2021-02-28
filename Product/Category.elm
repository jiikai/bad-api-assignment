module Product.Category exposing
  ( Category
  , count, compare, listOf, other
  , fromInt, toInt
  , fromString, toString
  , decoder, parser
  )


import Json.Decode as Decode
import Url.Parser as Parser


import Array.NonEmpty as NEA exposing (NonEmptyArray) 
import Enum exposing (Enum)



{- An enum-like type for product categories.
  Encapsulates a string that has to match a category name in `namesOf`, and an
  int code for the category, suitable for use as an e.g. index.
-}
type Category
  = Category Int String



namesOf : List String
namesOf =
  [ "beanies"
  , "facemasks"
  , "gloves"
  ]


listOf : List Category
listOf = List.sort namesOf
    |> List.indexedMap (\idx name -> (Category (idx + 1) name))
    |> (::) other


enumOf : Enum Category
enumOf =
  Enum.define listOf (\(Category _ name) -> name)


arrayOf : NonEmptyArray Category
arrayOf =
  Maybe.withDefault (NEA.fromElement other)
    <| NEA.fromList listOf 


{-  Compare by encapsulated string.
-}
compare : Category -> Category -> Order
compare a b =
  Basics.compare (toInt a) (toInt b)


{-  Return the number of categories.
-}
count : Int
count =
  NEA.length arrayOf


{- The "other" default category that is always present.
-}
other : Category
other =
  (Category 0 "other")


{-  Returns the `Category` corresponding to the given integer `n`.
-}
fromInt : Int -> Maybe Category
fromInt n =
  NEA.get n arrayOf


{-  Returns the integer corresponding to the given Category. 
-}
toInt : Category -> Int
toInt (Category idx _) =
  idx
        

{-  Constructs a category from its name as a string.
-}
fromString : String -> Maybe Category
fromString str =
  Enum.build enumOf str


{-  Returns the name of the category as a string.
-}
toString : Category -> String
toString category =
  Enum.toString enumOf category


{-  Decodes JSON to a category.
-}
decoder : Decode.Decoder Category
decoder =
  Enum.decoder enumOf


{-  An url parser for Category. 
-}
parser : Parser.Parser (Category -> a) a
parser =
  Parser.custom "PRODUCT_CATEGORY" (\segment -> fromString segment)
