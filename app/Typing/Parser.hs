{-# LANGUAGE TypeFamilies #-}
module Typing.Parser where

import Typing.Typ
import Text.Parsec
import Text.Parsec.ByteString

data Parsed

type instance TySym Parsed = String

parseTyConSym :: Parser String
parseTyConSym = (:) <$> upper <*> many alphaNum

parseTyVarSym :: Parser String 
parseTyVarSym = (:) <$> lower <*> many alphaNum

parseTyCon :: Parser (Typ Parsed)
parseTyCon = TyCon <$> parseTyConSym

parseTyVar :: Parser (Typ Parsed)
parseTyVar = TyVar <$> parseTyVarSym

parseTyBracketed :: Parser (Typ Parsed)
parseTyBracketed = do 
    string "("
    spaces 
    ret <- parseType 
    spaces 
    string ")"
    return ret

parseTyAtom :: Parser (Typ Parsed)
parseTyAtom = parseTyCon <|> parseTyBracketed <|> parseTyVar

parseTyApp :: Parser (Typ Parsed)
parseTyApp = do 
    c1 <- parseTyAtom
    spaces 
    go c1
    where 
        parseApply c1 = do 
            c2 <- parseTyAtom 
            spaces 
            go (TyApp c1 c2)

        go a = parseApply a <|> return a

parseType :: Parser (Typ Parsed)
parseType = do
    t1 <- parseTyApp
    spaces
    (do
        string "->"
        spaces
        TyApp (TyApp (TyCon "->") t1) <$> parseType) <|> do 
            return t1
