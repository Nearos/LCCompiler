{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Parser where 

import Tree

import Text.Parsec
import Text.Parsec.ByteString


import Typing.Typ
import qualified Typing.Parser as TyParser

data Parsed

type instance Sym Parsed = String
type instance Const Parsed = String
type instance AppMeta Parsed = SourcePos
type instance VarMeta Parsed = SourcePos
type instance LambdaMeta Parsed = SourcePos
type instance LitMeta Parsed = SourcePos
type instance ExternMeta Parsed = SourcePos
type instance BindMeta Parsed = (SourcePos, Maybe (Typ TyParser.Parsed))
type instance ConsMeta Parsed = SourcePos
type instance CaseMeta Parsed = SourcePos

instance ShowablePass [Char] where
    passShow = id 

instance ShowablePass SourcePos where 
    passShow = show

instance ShowablePass (SourcePos, Maybe (Typ TyParser.Parsed)) where 
    passShow = show

parseSym :: Parser String 
parseSym = (:) <$> lower <*> many alphaNum

parseConstName :: Parser String
parseConstName = (:) <$> upper <*> many alphaNum

parseVar :: Parser (Expr Parsed)
parseVar = Var <$> getPosition <*> parseSym

parseConst :: Parser (Expr Parsed)
parseConst = Var <$> getPosition <*> parseConstName

parseLambda :: Parser (Expr Parsed)
parseLambda = do 
    location <- getPosition
    oneOf "\\λ" <|> ' ' <$ try (string "fn") <|> ' ' <$ try (string "lambda")
    spaces
    sym <- parseSym
    spaces
    oneOf "." <|> ' ' <$ try (string "->")
    spaces
    Lambda location sym <$> parseExpr

parseBracketed :: Parser (Expr Parsed)
parseBracketed = do 
    string "("
    ret <- parseExpr
    string ")"
    return ret 

parseExtern :: Parser (Expr Parsed) 
parseExtern = do 
    location <- getPosition
    string "["
    name <- parseSym
    string "]"
    return $ Extern location name 

parseStringLit :: Parser (Expr Parsed)
parseStringLit = do
    location <- getPosition
    string "\""
    value <- many $ noneOf "\""
    string "\""
    return $ SrcStringLit location value

parseIntLit :: Parser (Expr Parsed)
parseIntLit = do 
    location <- getPosition
    value <- read <$> many1 alphaNum
    return $ SrcIntLit location value

parseAtomic = parseBracketed <|> parseVar <|> parseConst <|> parseExtern <|> parseStringLit <|> parseIntLit

parseApp :: Parser (Expr Parsed)
parseApp = do  
    t1 <- parseAtomic
    spaces 
    paRec t1
    where 
        paRec t1 = (do 
            location <- getPosition 
            t2 <- parseAtomic
            spaces 
            paRec $ App location t1 t2)
            <|> return t1

parsePattern :: Parser (Pattern Parsed)
parsePattern = do 
    constant <- parseConstName
    spaces 
    vars <- many $ parseSym <* spaces 
    return $ Pattern constant vars

parseBranch :: Parser (CaseBranch Parsed)
parseBranch = do 
    pat <- parsePattern 
    spaces
    string "->"
    spaces
    inner <- parseExpr
    string ";"
    return $ CaseBranch pat inner


parseCase :: Parser (Expr Parsed)
parseCase = do 
    location <- getPosition
    try $ string "case"
    spaces
    scrut <- parseApp -- don't allow lambda definition to be here - there's no reason
    string ":"
    spaces
    branches <- many1 $ parseBranch <* spaces
    return $ Case location scrut branches

parseExpr :: Parser (Expr Parsed)
parseExpr = parseLambda <|> parseCase <|> parseApp

parseBinding :: Parser (Toplevel Parsed)
parseBinding = do 
    location <- getPosition
    name  <- parseSym 
    spaces 
    string "="
    spaces 
    value <- parseExpr
    spaces 
    string ";"
    return $ Binding (location, Nothing) name value

parseConstDef :: Parser (Toplevel Parsed)
parseConstDef = do 
    location <- getPosition
    name <- parseConstName
    spaces 
    string ":"
    spaces 
    --arity <- read <$> many digit 
    typeSig <- TyParser.parseType
    spaces 
    string ";"
    return $ ConstDef (location, Just typeSig) name $ arity "->" typeSig


parseToplevel :: Parser (Toplevel Parsed)
parseToplevel = parseBinding <|> parseConstDef

parseProgram :: Parser [Toplevel Parsed]
parseProgram = many $ spaces *> parseToplevel