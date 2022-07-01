{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE FlexibleInstances         #-}

module Tools.Bib.BibTex where

import Tools.Bib.Doi

import Data.List (intercalate)
import Data.Semigroup (stimes)
import qualified Data.Map as M
import Data.Map.Lazy (Map)
import Control.Lens hiding (noneOf)
import GHC.Generics (Generic)
import Data.Generics.Sum
import Data.Generics.Product
import Data.Generics.Labels
import qualified Text.Megaparsec as P
import Text.Megaparsec hiding (parseMaybe
                              , parseMaybe
                              , parse
                              , parseTest
                              , runParser
                              , runParser')
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Network.Wreq as R
import Data.Void
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TIO
import Data.Aeson (ToJSON(..), FromJSON (..), Key(..), Value(..), fromJSON)
import qualified Data.Aeson.Types as A
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as ST
import qualified Data.Text.Lens as ST
import Data.Text.Lazy.Lens (utf8, packed, unpacked)
import Data.Aeson.Types (Result(..))
import qualified Data.Aeson.Key as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as A
import Control.Monad.State
import qualified Control.Monad.Trans as MT
import Data.Foldable (toList)
import qualified Data.Yaml as Y

type Text = T.Text
type EntryType = String
type BibTexFields = Map String String

data BibTexEntry = BibTexEntry { entryType :: EntryType,
                                 citekey   :: String,
                                 fields    :: BibTexFields
                               } deriving (Show, Generic)


defaultEntryType :: EntryType
defaultEntryType = "misc"

entryTypeAliases :: [String]
entryTypeAliases = ["entrytype", "etype", "type"]

citekeyAliases :: [String]
citekeyAliases = ["citekey", "ckey", "key", "id"]

--------------------------------------------------------------------------------
-- DOI                                                                        --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- JSON CONVERSION                                                            --
--------------------------------------------------------------------------------

-- KeyMap State Parser
type KMSParser  v = StateT (A.KeyMap v) A.Parser
type KMSParser' v = KMSParser Value v

--- Key Convertible
class IsKey k where
  _key :: Iso' k Key
  _key = iso toKey fromKey
  toKey :: k -> Key
  toKey = view _key
  fromKey :: Key -> k
  fromKey = view (from _key)
  showKey :: k -> String
  showKey = fromKey . toKey
  {-# MINIMAL _key | (toKey, fromKey) #-}

instance IsKey Key where
  _key    = id
  toKey   = id
  fromKey = id
  showKey = fromKey

instance IsKey String where
  toKey   = A.fromString
  fromKey = A.toString
  showKey = id

instance IsKey ST.Text where
  fromKey = A.toText
  toKey   = A.fromText
  showKey = ST.unpack

showKeys :: (Foldable t, IsKey k) => t k -> String
showKeys = intercalate "[" . fmap (\x -> "'" ++ showKey x ++ "'") . toList

-- | Gives a value from the KeyMapPicker without removing it from the map.
-- | If it does not exist, it gives an error.
kmsLookup :: IsKey k => k -> KMSParser v v
kmsLookup k = get >>= maybe err pure . A.lookup (toKey k)
  where err = fail ("key '" ++ showKey k ++ "' does not exist.")

-- | Picks the first match of the provided key.
kmsLookupFirstOf :: (Foldable t, IsKey k) => t k -> KMSParser v v
kmsLookupFirstOf ks = foldr (mplus . kmsLookup) err ks
  where err         = fail ("None of keys " ++ showKeys ks ++ " exists.")

-- | Picks the value at the key if it exists. Gives an error otherwise.
kmsPick :: IsKey k => k -> KMSParser v v
kmsPick k = do
    v <- kmsLookup k
    modify (A.delete (toKey k))
    return v

-- | Picks the first match of the provided key. Gives an error otherwise.
kmsPickFirstOf :: (Foldable t, IsKey k) => t k -> KMSParser v v
kmsPickFirstOf ks = foldr (mplus . kmsPick) err ks
  where err       = fail ("None of keys " ++ showKeys ks ++ " exists.")

-- | Pickes the cite key from the key value list.
kmsPickCiteKey :: KMSParser' String
kmsPickCiteKey = do
  v <- kmsPickFirstOf citekeyAliases
  t <- MT.lift $ A.withText "citekey" pure v
  return (ST.unpack t)

-- | Pickes the entry type from the key value list.
kmsPickEntryType :: KMSParser' EntryType
kmsPickEntryType = do
  v <- kmsPickFirstOf entryTypeAliases
  t <- MT.lift $ A.withText "entryType" pure v
  return (ST.unpack t)

-- | Pickes the entry type from the key value list.
kmsPickFields :: KMSParser' BibTexFields
kmsPickFields = do
  m <- get
  y <- MT.lift (parseJSON $ Object m)
  put mempty
  return y

kmsPickBibWithKey :: String -> KMSParser' BibTexEntry
kmsPickBibWithKey s = flip BibTexEntry s
    <$> (kmsPickEntryType <|> pure defaultEntryType)
    <*> kmsPickFields

kmsPickBib :: KMSParser' BibTexEntry
kmsPickBib = kmsPickCiteKey >>= kmsPickBibWithKey

keyToLower :: Key -> Key
keyToLower = over (from _key) ST.toLower

keysToLower :: KeyMap v -> KeyMap v
keysToLower = A.mapKeyVal keyToLower id

class ToJSONPair a where
  toJSONPair :: a -> (Key, Value)

class FromJSONPair a where
  parseJSONPair :: Key -> Value -> A.Parser a
  parseJSONObject :: A.Object -> A.Parser [a]
  parseJSONObject = traverse (uncurry parseJSONPair) . A.toList

instance ToJSONPair BibTexEntry where
  toJSONPair b = (A.fromString (b ^. #citekey), toJSON
      $ M.insert "type" (view #entryType b)
      $ view #fields b
    )

instance ToJSON   BibTexEntry where
  toJSON x   = toJSON
      $ M.insert "key" (view #citekey x)
      $ M.insert "type" (view #entryType x)
      $ view #fields x
  toJSONList = A.object . fmap toJSONPair

instance FromJSONPair BibTexEntry where
  parseJSONPair k = A.withObject "BibTexEntry" $ evalStateT m
    where m = modify keysToLower >> kmsPickBibWithKey (fromKey k)

__parseJSONList :: (FromJSONPair a, FromJSON a) => Value -> A.Parser [a]
__parseJSONList (Object o) = parseJSONObject o
__parseJSONList (Array l)  = traverse parseJSON (toList l)
__parseJSONList _          = fail "Array or Object expected."

instance FromJSON BibTexEntry where
  parseJSON = A.withObject "BibTexEntry" $ evalStateT m
    where m = modify keysToLower >> kmsPickBib
  parseJSONList = __parseJSONList

--------------------------------------------------------------------------------
-- BIB REQUESTS                                                               --
--------------------------------------------------------------------------------

data BibRequest = FromDOI  (Maybe String) DOI
                | FromJust BibTexEntry deriving (Show, Generic)

instance FromJSONPair BibRequest where
  parseJSONPair k v@(String _) = FromDOI (Just $ fromKey k) <$> parseJSON v
  parseJSONPair k v            = FromJust <$> parseJSONPair k v

instance FromJSON BibRequest where
  parseJSON v@(String _)   = FromDOI Nothing <$> parseJSON v
  parseJSON v              = FromJust <$> parseJSON v
  parseJSONList            = __parseJSONList


doiRequest :: DOI -> IO (Maybe BibTexEntry)
doiRequest d = preview (R.responseBody . utf8 . _BibTexEntry) <$> resp
  where resp = R.getWith opts (d ^. re doiUrl)
        opts = R.defaults & R.header "Accept" .~ ["application/x-bibtex"]

requestBibEntry :: BibRequest -> IO (Maybe BibTexEntry)
requestBibEntry (FromDOI a b) = fmap (f a) <$> doiRequest b
  where f Nothing  = id
        f (Just k) = #citekey .~ k
requestBibEntry (FromJust b) = pure $ Just b

requestBib :: [BibRequest] -> IO [BibTexEntry]
requestBib = fmap catMaybes . traverse requestBibEntry

decodeRequestFile :: FilePath -> IO [BibTexEntry]
decodeRequestFile = requestBib <=< Y.decodeFileThrow

--------------------------------------------------------------------------------
-- TEXT PARSER                                                                --
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser () -- whitespaces
sc = L.space space1 (L.skipLineComment "#") empty

-- Entry Parser
_entryParser :: Parser BibTexEntry
_entryParser = do
    t <- fmap toLower <$> typ
    br (BibTexEntry t <$> key <*> ((comma *> fields) <|> pure mempty))
  where
    xx :: Parser Char -- Special Characters
    xx      = satisfy (\x -> isSpace x || elem @[] x "@{}\"=,")
    l       = L.lexeme sc
    symb    = L.symbol sc
    strL :: Parser String -- string literals (like "abc")
    strL    = l (char '\"' *> manyTill anySingle (char '\"'))
    comma   = symb ","
    eq      = symb "="
    br      = between (symb "{") (symb "}")
    br'     = between (char '{') (char '}')
    noBrC :: Parser Char
    noBrC   = noneOf @[] "{}"
    brLit  :: Parser String
    brLit  = l.br' $ concat <$> many (
                    some (noneOf @[] "{}")
                <|> fmap (\c -> "{" ++ c ++ "}") brLit
              )
    typ    = l (char '@' *> many alphaNumChar) -- Entry types (prefixed with @)
    key    = l (manyTill anySingle (lookAhead xx)) -- Key values.
    term   = lookAhead (sc *> l (oneOf @[] ",}"))
    lit    = manyTill anySingle term -- Literal value
    val    = strL <|> brLit <|> lit
    field  :: Parser (String, String) -- One field assignment.
    field  = do
      k <- fmap toLower <$> key
      eq
      v <- val
      sc
      return (toLower <$> k, v)
    fields :: Parser BibTexFields -- Field assignments.
    fields = M.fromList <$> sepBy field comma

entryParser :: Parser BibTexEntry
entryParser = sc *> _entryParser <* eof

-- List parser
parser :: Parser [BibTexEntry]
parser = sc *> many _entryParser <* eof

-- Convenience aliases
parseEntry       = P.parse       entryParser
parseEntryMaybe  = P.parseMaybe  entryParser
parseEntryTest   = P.parseTest   entryParser
runEntryParser   = P.runParser   entryParser
runEntryParser'  = P.runParser'  entryParser
runEntryParserT  = P.runParserT  entryParser
runEntryParserT' = P.runParserT' entryParser
parse            = P.parse       parser
parseMaybe       = P.parseMaybe  parser
parseTest        = P.parseTest   parser
runParser        = P.runParser   parser
runParser'       = P.runParser'  parser
runParserT       = P.runParserT  parser
runParserT'      = P.runParserT' parser

--------------------------------------------------------------------------------
-- TEXT RENDERER                                                              --
--------------------------------------------------------------------------------

type Builder = TB.Builder

renderEntry :: BibTexEntry -> Builder
renderEntry e = typ <> br (ckey <> fields <> c '\n' )
  where c       = TB.singleton
        s       = TB.fromString
        typ     = "@" <> s (e ^. #entryType)
        br x    = "{" <> x <> "}"
        maxKeyL = maximum $ length <$> M.keys (e ^. #fields)
        ckey    = s (e ^. #citekey)
        fkey k  = s (k ++ replicate (maxKeyL - length k) ' ')
        fval v  = br $ s v
        f k v   = ",\n    " <> fkey k <> s " = " <> fval v
        fields  = M.foldMapWithKey f (e ^. #fields)

render :: [BibTexEntry] -> Builder
render = mconcat . fmap ((<> TB.fromString "\n\n") . renderEntry)

--------------------------------------------------------------------------------
-- OPTICS                                                                     --
--------------------------------------------------------------------------------


type instance IxValue BibTexEntry = String
type instance Index   BibTexEntry = String

defaultsTo :: Eq d => d -> Lens' d (Maybe d)
defaultsTo d f x = fromMaybe d <$> f v
  where v | x == d    = Nothing
          | otherwise = Just x

instance At BibTexEntry where
  at k | key `elem` entryTypeAliases = #entryType . defaultsTo defaultEntryType
       | otherwise                   = #fields . at key
    where key    = toLower <$> k

instance Ixed BibTexEntry where
  ix k | key `elem` entryTypeAliases = #entryType
       | key `elem` citekeyAliases   = #citekey
       | otherwise                   = #fields . ix key
    where key    = toLower <$> k

_BibTexEntry :: Prism' Text BibTexEntry
_BibTexEntry = prism' (TB.toLazyText . renderEntry) parseEntryMaybe

_BibTex :: Prism' Text [BibTexEntry]
_BibTex = prism' (TB.toLazyText . render) parseMaybe

--------------------------------------------------------------------------------
-- IO                                                                         --
--------------------------------------------------------------------------------

readBibFile :: FilePath -> IO [BibTexEntry]
readBibFile = fmap (fromMaybe [] . preview _BibTex) . TIO.readFile

writeBibFile :: FilePath -> [BibTexEntry] -> IO ()
writeBibFile p e = TIO.writeFile p (review _BibTex e)

appendBibFile :: FilePath -> [BibTexEntry] -> IO ()
appendBibFile p e = TIO.appendFile p (review _BibTex e)

putBibStr :: [BibTexEntry] -> IO ()
putBibStr = TIO.putStr . review _BibTex

putBibStrLn :: [BibTexEntry] -> IO ()
putBibStrLn = TIO.putStrLn . review _BibTex
