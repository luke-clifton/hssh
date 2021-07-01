{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Shh.Interpolate
  ( i
  , u
  ) where

import           Control.Applicative            ( liftA2 )
import           Data.Char
import           Data.List                      ( dropWhileEnd
                                                , intercalate
                                                )
import           GHC.TypeLits                   ( AppendSymbol )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.ParserCombinators.ReadP

-- | 'i' is a QuasiQuoter which performs very simple interpolation of Haskell
-- values into strings.
--
-- - Interpolated variables are prefixed with @$@
-- - To escape a @$@ use @\\$@
--
-- Values are concatenated with '<>' so this works with any type with an
-- 'IsString' instance.
--
-- It can be used as an expression, a type (using -XDataKinds) or a pattern
-- (matching the expression version using -XViewPatterns).
--
-- >>> let foo = "world" in [i|hello, $foo|]
-- "hello, world"
--
-- >>> let foo = "world" in [i|hello, \$foo|]
-- "hello, \\$foo"
--
-- >>> import Data.Functor.Identity -- Identity has an IsString instance
-- >>> :set -XOverloadedStrings
-- >>> let foo = Identity "world" in [i|hello, $foo|]
-- Identity "hello, world"
--
-- >>> type Foo = "world"
-- >>> :t (Proxy @[i|hello, $Foo|])
-- (Proxy @[i|hello, $Foo|]) :: Proxy "hello, world"
--
-- >>> foo = "world"
-- >>> case "hello, world" of [i|hello, $foo|] -> True
-- True
i :: QuasiQuoter
i = QuasiQuoter
  { quoteExp  = interpExp
  , quoteType = interpType
  , quoteDec  = error "No declaration quoting for interpolated strings"
  , quotePat  = (\e -> [p|((== $e) -> True)|]) . interpExp
  }

-- | 'u' performs the same function as 'i' except that:
--
-- - Empty lines at the beginning and end of the quote are removed
-- - A common prefix of space is removed from every line
--
-- This behaviour is similar to Nix's multiline strings
--
-- This makes it suitable for writing large blocks of text at a pleasing
-- indentation in Haskell code.
u :: QuasiQuoter
u = wrapQuasi unindent i

interpExp :: String -> Q Exp
interpExp =
  foldEither varOrConE (litE . stringL) (\e1 e2 -> [|$e1 <> $e2|]) . parse

interpType :: String -> Q Type
interpType =
  foldEither varOrConT (litT . strTyLit) (\t1 t2 -> [t|AppendSymbol $t1 $t2|])
    . parse

----------------------------------------------------------------
-- Unindenting
----------------------------------------------------------------

wrapQuasi :: (String -> String) -> QuasiQuoter -> QuasiQuoter
wrapQuasi f QuasiQuoter {..} =
  QuasiQuoter (quoteExp . f) (quotePat . f) (quoteType . f) (quoteDec . f)

-- | Strips empty lines from the beginning and end. Removes the common space
-- prefix from the nonempty lines
unindent :: String -> String
unindent s =
  let stripEmptyLines  = dropWhile (== "") . dropWhileEnd (== "")
      ls               = stripEmptyLines . lines $ s
      strippedLastLine = if onlySpace (last ls) then init ls else ls
      nonEmpties       = filter (/= "") strippedLastLine
      minIndent        = case nonEmpties of
        [] -> 0
        _  -> minimum (length . takeWhile (== ' ') <$> nonEmpties)
      unindented = drop minIndent <$> strippedLastLine
  in  intercalate "\n" unindented

onlySpace :: String -> Bool
onlySpace = all isSpace

----------------------------------------------------------------
-- The parser
----------------------------------------------------------------

type Var = String

-- >>> parse ""
-- []
--
-- >>> parse "hello $world"
-- [Right "hello ",Left "world"]
--
-- >>> parse "$hello$world"
-- [Left "hello",Left "world"]
--
-- >>> parse "$"
-- [Right "$"]
--
-- >>> parse "hi"
-- [Right "hi"]
--
-- >>> parse "h$hi"
-- [Right "h",Left "hi"]
--
-- >>> parse "$$hi"
-- [Right "$",Left "hi"]
--
-- >>> parse "$1"
-- [Right "$1"]
--
-- >>> parse "$$$"
-- [Right "$$$"]
--
-- >>> parse "\\"
-- [Right "\\"]
--
-- >>> parse "\\$"
-- [Right "$"]
--
-- >>> parse "\\$hi"
-- [Right "$hi"]
--
-- >>> parse "\\\\$hi"
-- [Right "\\$hi"]
--
-- >>> parse "\\hi"
-- [Right "\\hi"]
--
-- >>> parse "$hi\\$foo"
-- [Left "hi",Right "$foo"]
--
-- >>> parse "hello, \\$foo"
-- [Right "hello, \\$foo"]
parse :: String -> [Either Var String]
parse s =
  let -- A haskell var or con
      ident = (:) <$> satisfy (isLower <||> isUpper <||> (== '_')) <*> munch
        (isAlphaNum <||> (== '\'') <||> (== '_'))
      -- parse a var, if that doesn't work out return just the '$'
      var    = char '$' *> ((Left <$> ident) <++ pure (Right "$"))
      -- Everything up to a $ or \
      normal = Right <$> munch1 ((/= '$') <&&> (/= '\\'))
      -- One normal or var
      -- - Check escaped '$' first
      -- - variables, starting with $
      -- - normal string
      one    = escape <++ var <++ normal
      -- escape a $
      escape = char '\\' *> ((Right <$> string "$") <++ pure (Right "\\"))
      parser = many one <* eof
  in  case readP_to_S parser s of
        [(r, "")] -> foldr mergeRights [] r
        _         -> error "Failed to parse string"

mergeRights :: Either Var String -> [Either Var String] -> [Either Var String]
mergeRights = \case
  Left  v -> (Left v :)
  Right n -> \case
    (Right m : xs) -> Right (n <> m) : xs
    xs             -> Right n : xs

(<&&>), (<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
(<&&>) = liftA2 (&&)

----------------------------------------------------------------
-- Misc utilities
----------------------------------------------------------------

varOrConE :: String -> ExpQ
varOrConE n = (if isLower (head n) then varE else conE) . mkName $ n

varOrConT :: String -> TypeQ
varOrConT n = (if isLower (head n) then varT else conT) . mkName $ n

foldEither
  :: (Foldable t, Functor t)
  => (a -> c)
  -> (b -> c)
  -> (c -> c -> c)
  -> t (Either a b)
  -> c
foldEither l r f = foldr1 f . fmap (either l r)
