{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedLabels #-}

module Tools.Bib.Doi ( DOI(..)
                     , doiBaseURL
                     , doiPathParser
                     , doiUrlParser
                     , doiPath
                     , doiUrl
                     ) where
import Data.Map (Map)
import Control.Lens
import GHC.Generics hiding (to)
import Data.Generics.Sum
import Data.Generics.Product
import Data.Generics.Labels
import Data.Text.Lens (unpacked)
import Data.Void
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import Data.String (IsString, fromString)
import Control.Applicative ((<|>))
import Data.Data (Proxy)

data DOI = DOI { publisher :: String
               , item      :: String
               } deriving (Generic, Eq)

--------------------------------------------------------------------------------
-- CONSTANTS                                                                  --
--------------------------------------------------------------------------------

doiBaseURL :: IsString a => a
doiBaseURL = "https://doi.org/"

--------------------------------------------------------------------------------
-- PARSERS                                                                    --
--------------------------------------------------------------------------------

type CharStream s = (Stream s, Token s ~ Char, IsString (Tokens s))

doiPathParser :: (CharStream s) => Parsec Void s DOI
doiPathParser = DOI <$> P.manyTill a sep <*> P.manyTill a P.eof
  where a   = P.anySingle
        sep = P.char '/'

doiUrlParser :: (CharStream s) => Parsec Void s DOI
doiUrlParser = P.string doiBaseURL *> doiPathParser

--------------------------------------------------------------------------------
-- OPTICS                                                                     --
--------------------------------------------------------------------------------

-- pPrism :: (CharStream s, IsString t) =>  (b -> t) -> Prism s t a b

doiPath :: (CharStream s, IsString s) => Prism' s DOI
doiPath = prism' r (P.parseMaybe doiPathParser)
  where r x = fromString $ x ^. #publisher ++ "/" ++ x ^. #item

doiUrl :: (CharStream s, IsString s, Semigroup s) => Prism' s DOI
doiUrl = prism' r p
  where r x = doiBaseURL <> review doiPath x
        p x = P.parseMaybe doiUrlParser x <|> P.parseMaybe doiPathParser x

--------------------------------------------------------------------------------
-- INSTANCES                                                                  --
--------------------------------------------------------------------------------

instance Show DOI where
  show x = "DOI \"" ++ (x ^. re doiPath) ++ "\""

instance A.ToJSON DOI where
  toJSON     = A.String . view (re doiPath)
  toEncoding = A.text . view (re doiPath)

instance A.FromJSON DOI where
  parseJSON = A.withText "DOI" $ maybe (fail "Invalid") pure . preview doiUrl

-- toURL :: DOI -> URL
-- toURL = (++) "https://doi.org/"
--
-- fromURL :: URL -> Maybe DOI
--
-- getBibData :: DOI -> IO ( Response ByteString )
-- getBibData = getWith opts . toURL
--   where opts = defaults & header "Accept" .~ ["application/x-bibtex"]
--
-- getBib :: DOI -> IO ( Maybe BibTexEntry )
-- getBib = fmap (^? responseBody . utf8 . _BibTexEntry ) . getBibData
-- getBib = fmap BibParser.parseResponse . getBibData
