{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Prelude hiding (EQ, LT, GT)
import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
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
            | NULL
            | NNULL
            deriving (Show)

instance FromJSON WExpr where
  parseJSON v = maybe mzero pure $ parseWExpr v

instance FromJSON ValConst where
  parseJSON v = maybe mzero pure $ parseValConst v

instance FromJSON SQLVal where
  parseJSON v = maybe mzero pure $ parseSQLVal v

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
toSQL'' NULL    = " IS NULL"
toSQL'' NNULL   = " IS NOT NULL"

toSQLVec :: WExpr -> (T.Text, [SQLVal])
toSQLVec e = (toSQL e, toPars e)

jsonToWExpr :: String -> String
jsonToWExpr json = show $ toSQLVec <$> decode (BS.pack json)

wrapInParens :: T.Text -> T.Text
wrapInParens t = "(" <> t <> ")"

parseSQLVal :: Value -> Maybe SQLVal
parseSQLVal (String s) = case parseISO8601 (T.unpack s) of
  Nothing -> Just $ SQLString s
  Just ts -> Just $ SQLTimestamp ts
parseSQLVal (Number n) = Just $ SQLNumber n
parseSQLVal _          = Nothing

parseValConst :: Value -> Maybe ValConst
parseValConst v@String{} = Val <$> parseSQLVal v
parseValConst o@Object{} = Constrs <$> parseConstrs o
parseValConst _          = Nothing

parseConstrs :: Value -> Maybe [Constr]
parseConstrs (Object o) = sequence $ map parseConstr $ M.toList o
parseConstrs _          = Nothing

parseConstr :: (T.Text, Value) -> Maybe Constr
parseConstr ("$eq",  v) = EQ  <$> parseSQLVal v
parseConstr ("$neq", v) = NEQ <$> parseSQLVal v
parseConstr ("$lt",  v) = LT  <$> parseSQLVal v
parseConstr ("$lte", v) = LTE <$> parseSQLVal v
parseConstr ("$gt",  v) = GT  <$> parseSQLVal v
parseConstr ("$gte", v) = GTE <$> parseSQLVal v
parseConstr ("$in", Array vs) = IN <$> sequence (map parseSQLVal $ V.toList vs)
parseConstr ("$null", Bool True)  = Just NULL
parseConstr ("$null", Bool False) = Just NNULL
parseConstr _           = Nothing

parseWExpr :: Value -> Maybe WExpr
parseWExpr (Object o) = AndE <$> sequence (map parsePairOr $ M.toList o)
parseWExpr _          = Nothing

parsePairOr :: (T.Text, Value) -> Maybe PairOr
parsePairOr ("$or", Array es) = OrE <$> sequence (map parseWExpr $ V.toList es)
parsePairOr (key, val) = Pair key <$> parseValConst val
