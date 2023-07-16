{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Parser where 

import Tree

import Text.Parsec
import Text.Parsec.ByteString


data Parsed

type instance Sym Parsed = String
type instance AppMeta Parsed = SourcePos
type instance VarMeta Parsed = SourcePos
type instance LambdaMeta Parsed = SourcePos
type instance LitMeta Parsed = SourcePos
type instance ExternMeta Parsed = SourcePos
type instance BindMeta Parsed = SourcePos

instance ShowablePass [Char] where
    passShow = id 

instance ShowablePass SourcePos where 
    passShow = show

parseSym :: Parser String 
parseSym = (:) <$> letter <*> many alphaNum

parseVar :: Parser (Expr Parsed)
parseVar = Var <$> getPosition <*> parseSym

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

parseAtomic = parseBracketed <|> parseVar <|> parseExtern <|> parseStringLit

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

parseExpr :: Parser (Expr Parsed)
parseExpr = parseLambda <|> parseApp

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
    return $ Binding location name value

parseToplevel :: Parser (Toplevel Parsed)
parseToplevel = parseBinding

parseProgram :: Parser [Toplevel Parsed]
parseProgram = many $ spaces *> parseToplevel