{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Prelude hiding (EQ, LT, GT)
import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import           Data.Time.Clock (UTCTime)
import           Data.Time.ISO8601 (formatISO8601, parseISO8601)
import qualified Data.Scientific (Scientific)
import           Data.Scientific as S

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
  toJSON e = object [("sql", String $ toSQL e)
                    ,("pars", Array $ V.fromList $ map toJSON $ toPars e)]

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
jsonToWExprJson json = case eitherDecode (BS.pack json) :: Either String WExpr of
  Left msg -> msg
  Right e  -> BS.unpack $ encode e

qMarks :: [a] -> T.Text
qMarks = T.intersperse ',' . T.pack . map (const '?')

toPars :: WExpr -> [SQLVal]
toPars (AndE l) = concatMap toPars' l

toPars' :: PairOr -> [SQLVal]
toPars' (Pair _ (Val v))      = [v]
toPars' (Pair _ (Constrs cs)) = concatMap toPars'' cs
toPars' (OrE exps)            = concatMap toPars exps

toPars'' :: Constr -> [SQLVal]
toPars'' (EQ  v) = [v]
toPars'' (NEQ v) = [v]
toPars'' (LT  v) = [v]
toPars'' (LTE v) = [v]
toPars'' (GTE v) = [v]
toPars'' (GT  v) = [v]
toPars'' (IN vs) = vs
toPars'' _       = []

toSQL :: WExpr -> T.Text
toSQL (AndE l) = wrapInParens $ T.intercalate " AND " (map toSQL' l)

toSQL' :: PairOr -> T.Text
toSQL' (Pair k (Val _))      = k <> " = ?"
toSQL' (Pair k (Constrs cs)) = T.intercalate " AND " $ map ((k <>) . toSQL'') cs
toSQL' (OrE exps)            = wrapInParens $ T.intercalate " OR " $ map toSQL exps

toSQL'' :: Constr -> T.Text
toSQL'' (EQ  _) = " = ?"
toSQL'' (NEQ _) = " <> ?"
toSQL'' (LT  _) = " < ?"
toSQL'' (LTE _) = " <= ?"
toSQL'' (GT  _) = " > ?"
toSQL'' (GTE _) = " >= ?"
toSQL'' (IN vs) = " IN " <> wrapInParens (qMarks vs)
toSQL'' (NULL True)  = " IS NULL"
toSQL'' (NULL False) = " IS NOT NULL"

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
