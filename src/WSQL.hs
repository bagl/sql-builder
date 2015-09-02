{-# LANGUAGE OverloadedStrings #-}
module WSQL where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as M
import           Data.Monoid ((<>))
import           Data.Scientific as S
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.ISO8601 (formatISO8601, parseISO8601)
import qualified Data.Vector as V
import           Prelude hiding (EQ, LT, GT)

data WSQL = WSQL String [SQLVal] -- TODO: WSQL [Field] String [SQLVal] or something similar where Field is a column name that can be present
            deriving (Show)

data WExpr = AndE [PairOr]
           deriving (Show)

type Key = T.Text

data PairOr = Pair Key ValConst
            | OrE [WExpr]
            deriving (Show)

data SQLVal = SQLString T.Text
            | SQLNumber Scientific
            | SQLTimestamp UTCTime
            deriving (Show)

data ValConst = Val SQLVal
              | Constrs [Constr]
              deriving (Show)

data Constr = EQ    SQLVal
            | NEQ   SQLVal
            | LT    SQLVal
            | LTE   SQLVal
            | GT    SQLVal
            | GTE   SQLVal
            | IN    [SQLVal]
            | NULL  Prelude.Bool
            deriving (Show)

instance FromJSON WExpr where
  parseJSON (Object o) = AndE <$> mapM parsePairOr (M.toList o)
  parseJSON _          = fail "WExpr not Object"

instance ToJSON WExpr where
  toJSON e = object [("sql", String $ wExprToSQL e)
                    ,("pars", Array $ V.fromList $ map toJSON $ wExprToPars e)]

instance FromJSON WSQL where
  parseJSON js = parseJSON js >>= \wexpr ->
    return $ WSQL (T.unpack $ wExprToSQL wexpr) (wExprToPars wexpr)

instance ToJSON SQLVal where
  toJSON (SQLString s) = String s
  toJSON (SQLNumber n) = Number n
  toJSON (SQLTimestamp ts) = String $ T.pack $ formatISO8601 ts

instance FromJSON ValConst where
  parseJSON o@Object{} = Constrs <$> parseConstrs o
  parseJSON v          = Val <$> parseJSON v

instance FromJSON SQLVal where
  parseJSON (String s) = pure
                         $ maybe (SQLString s) SQLTimestamp
                         $ parseISO8601 (T.unpack s)
  parseJSON (Number n) = pure $ SQLNumber n
  parseJSON _          = fail "SQLVal not String or Number"

jsonToWExprJson :: String -> String
jsonToWExprJson js = case eitherDecode (BS.pack js) :: Either String WExpr of
  Left msg -> msg
  Right e  -> BS.unpack $ encode e

qMarks :: [a] -> T.Text
qMarks = T.intersperse ',' . T.pack . map (const '?')

wExprToPars :: WExpr -> [SQLVal]
wExprToPars (AndE l) = concatMap pairOrToPars l

pairOrToPars :: PairOr -> [SQLVal]
pairOrToPars (Pair _ (Val v))      = [v]
pairOrToPars (Pair _ (Constrs cs)) = concatMap constrToPars cs
pairOrToPars (OrE exps)            = concatMap wExprToPars exps

constrToPars :: Constr -> [SQLVal]
constrToPars (EQ  v) = [v]
constrToPars (NEQ v) = [v]
constrToPars (LT  v) = [v]
constrToPars (LTE v) = [v]
constrToPars (GTE v) = [v]
constrToPars (GT  v) = [v]
constrToPars (IN vs) = vs
constrToPars _       = []

wExprToSQL :: WExpr -> T.Text
wExprToSQL (AndE l) = wrapInParens $ T.intercalate " AND " (map pairOrToSQL l)

pairOrToSQL :: PairOr -> T.Text
pairOrToSQL (Pair k (Val _))      = k <> " = ?"
pairOrToSQL (Pair k (Constrs cs)) = T.intercalate " AND " $ map ((k <>) . constrToSQL) cs
pairOrToSQL (OrE exps)            = wrapInParens $ T.intercalate " OR " $ map wExprToSQL exps

constrToSQL :: Constr -> T.Text
constrToSQL (EQ  _) = " = ?"
constrToSQL (NEQ _) = " <> ?"
constrToSQL (LT  _) = " < ?"
constrToSQL (LTE _) = " <= ?"
constrToSQL (GT  _) = " > ?"
constrToSQL (GTE _) = " >= ?"
constrToSQL (IN vs) = " IN " <> wrapInParens (qMarks vs)
constrToSQL (NULL True)  = " IS NULL"
constrToSQL (NULL False) = " IS NOT NULL"

wrapInParens :: T.Text -> T.Text
wrapInParens t = "(" <> t <> ")"

parseConstrs :: Value -> Parser [Constr]
parseConstrs (Object o) = mapM parseConstr $ M.toList o
parseConstrs _          = fail "Constraints not Object"

parseConstr :: (T.Text, Value) -> Parser Constr
parseConstr ("$eq",  v) = EQ  <$> parseJSON v
parseConstr ("$neq", v) = NEQ <$> parseJSON v
parseConstr ("$lt",  v) = LT  <$> parseJSON v
parseConstr ("$lte", v) = LTE <$> parseJSON v
parseConstr ("$gt",  v) = GT  <$> parseJSON v
parseConstr ("$gte", v) = GTE <$> parseJSON v
parseConstr ("$in", Array vs) = IN <$> mapM parseJSON (V.toList vs)
parseConstr ("$null", Bool b) = pure $ NULL b
parseConstr p = fail $ "Unknown keyword or keyword/value pair '"
                <> show p
                <> "' in constraint"

parsePairOr :: (T.Text, Value) -> Parser PairOr
parsePairOr ("$or", Array es) = OrE <$> mapM parseJSON (V.toList es)
parsePairOr (key, val)        = Pair key <$> parseJSON val
