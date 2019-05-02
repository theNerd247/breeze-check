module Data.Aeson.Options 
{ customAesonOptions
} where

import Data.Aeson.Types

customAesonOptions = defaultOptions {fieldLabelModifier = removeUnderscorePrefix }

removeUnderscorePrefix ('_':xs) = xs
removeUnderscorePrefix xs = xs
