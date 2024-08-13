module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe


-- Type definition
data JsonPrimitive
  = JsonWhitespace String
  | JsonString String
  | JsonNumber String -- TODO: Use numeric type?
  | JsonObject [(JsonPrimitive, JsonPrimitive)]
  | JsonArray [JsonPrimitive]
  | JsonBool Bool
  | JsonNull
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }


-- Class instancing
instance Functor Parser where
  fmap f (Parser p)
    = Parser $ \input -> do
      (x, inputRemainder) <- p input
      Just (f x, inputRemainder)


instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2)
    = Parser $ \input -> do
      (f, remainder)  <- p1 input
      (a, remainder') <- p2 remainder
      Just (f a, remainder')


instance Alternative Parser where
  empty =  Parser (\_ -> Nothing)
  (Parser p1) <|> (Parser p2)
    = Parser $ \input ->
      p1 input <|> p2 input


-- Parsers
-- Generic
charParser :: Char -> Parser Char
charParser c = Parser f
  where
    f (x:xs)
      | x == c = Just (x, xs)
      | otherwise = Nothing
    f _ = Nothing


escapedCharParser :: Parser Char
escapedCharParser = Parser f
  where
    f ('\\':x:xs)
      | x == '\'' = Just ('\\', xs)
      | x == '\\' = Just ('\\', xs)
      | x == '/'  = Just ('/', xs)
      | x == 'b'  = Just ('\b', xs)
      | x == 'n'  = Just ('\n', xs)
      | x == 'r'  = Just ('\r', xs)
      | x == 't'  = Just ('\t', xs) -- TODO: Handle "\u" 
      | otherwise = Nothing
    f _ = Nothing


elemParser :: String -> Parser Char
elemParser xs = Parser f
  where
    f (y:ys)
      | elem y xs = Just (y, ys)
      | otherwise = Nothing
    f _ = Nothing


notElemParser :: String -> Parser Char
notElemParser xs = Parser f
  where
    f (y:ys)
      | notElem y xs = Just (y, ys)
      | otherwise = Nothing
    f _ = Nothing


stringParser :: String -> Parser String
stringParser = sequenceA . map charParser


negationSignParser :: Parser Char
negationSignParser = charParser '-'


signParser :: Parser Char
signParser = elemParser "+-"


eParser :: Parser Char
eParser = elemParser "eE"


nonZeroDigitParser :: Parser Char
nonZeroDigitParser = elemParser "123456789"


digitParser :: Parser Char
digitParser = elemParser "0123456789"


zeroDigitParser :: Parser Char
zeroDigitParser = charParser '0'


pointParser :: Parser Char
pointParser = charParser '.'


nonZeroIntegerParser :: Parser String
nonZeroIntegerParser = (:) <$> nonZeroDigitParser <*> many digitParser


zeroParser :: Parser String
zeroParser = singleton <$> zeroDigitParser


integerParser :: Parser String
integerParser = zeroParser <|> nonZeroIntegerParser


fractionParser :: Parser String
fractionParser = (:) <$> pointParser <*> some digitParser


exponentParser :: Parser String
exponentParser = f <$> eParser <*> optional signParser <*> some digitParser
  where
    f e sign d = join [singleton e, maybeToList sign, d]


numberParser :: Parser String
numberParser = f <$> optional negationSignParser <*> integerParser <*> optional fractionParser <*> optional exponentParser
  where
    f signP intP fracP expP = join [maybeToList signP, intP, fromMaybe "" fracP, fromMaybe "" expP] 



-- Parsers
-- JSON
jsonNull :: Parser JsonPrimitive
jsonNull = const JsonNull <$> stringParser "null"


jsonTrue :: Parser JsonPrimitive
jsonTrue = const (JsonBool True) <$> stringParser "true"


jsonFalse :: Parser JsonPrimitive
jsonFalse = const (JsonBool False) <$> stringParser "false"


jsonBool :: Parser JsonPrimitive
jsonBool = jsonTrue <|> jsonFalse


jsonWhitespace :: Parser JsonPrimitive
jsonWhitespace = JsonWhitespace <$> some (elemParser " \n\r\t")


jsonNumber :: Parser JsonPrimitive
jsonNumber = JsonNumber <$> numberParser


jsonString :: Parser JsonPrimitive
jsonString = JsonString <$> (bound *> some (notElemParser "\"\\" <|> escapedCharParser) <* bound)
  where
    bound = charParser '"'


jsonArray :: Parser JsonPrimitive
jsonArray = JsonArray <$> (open *> optional jsonWhitespace *> elements <* optional jsonWhitespace <* close)
  where
    open = charParser '['
    close = charParser ']'
    separator = charParser ',' *> optional jsonWhitespace
    elements = (:) <$> element <*> many (separator *> element)
           <|> pure []
    element = jsonNull
          <|> jsonBool
          <|> jsonNumber
          <|> jsonString
          <|> jsonArray
          <|> jsonObject


jsonObject :: Parser JsonPrimitive
jsonObject = JsonObject <$> (open *> optional jsonWhitespace *> tuples <* optional jsonWhitespace <* close)
  where
    open  = charParser '{'
    close = charParser '}'

    assignment = optional jsonWhitespace *> charParser ':' <* optional jsonWhitespace
    separator  =                            charParser ',' <* optional jsonWhitespace

    tuples = (:) <$> tuple <*> many (separator *> tuple) <|> pure []
    tuple = (\k _ v -> (k, v)) <$> jsonString <*> assignment <*> jsonValue
    jsonValue = jsonNull
            <|> jsonBool
            <|> jsonNumber
            <|> jsonString
            <|> jsonArray
            <|> jsonObject

parseJson :: String -> Maybe JsonPrimitive
parseJson xs = case runParser jsonValue $ reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace xs of
    Just (jv, "") -> Just jv
    _             -> Nothing
  where
    jsonValue = optional jsonWhitespace
             *> jsonNull
            <|> jsonBool
            <|> jsonNumber
            <|> jsonString
            <|> jsonArray
            <|> jsonObject
             <* optional jsonWhitespace


-- Print utility
pretty' :: String -> Int -> JsonPrimitive -> String
pretty' p t x = padding ++ case x of
    JsonNull           -> "null"
    (JsonBool b)       -> map toLower $ show b
    (JsonNumber n)     -> n
    (JsonString s)     -> "\"" ++ s ++ "\""
    (JsonWhitespace w) -> w
    (JsonArray a)      -> "[\n" ++ (intercalate ",\n" $ map (pretty' p (t + 1)) a) ++ "\n" ++ padding ++ "]"
    (JsonObject o)     -> "{\n" ++ (intercalate ",\n" $ map tupleToStr o)      ++ "\n" ++ padding ++  "}"
  where
    tupleToStr (k, v) = pretty' p (t + 1) k ++ ": " ++ pretty' p 0 v
    padding = concat $ replicate t p


pretty :: JsonPrimitive -> String
pretty = pretty' "  " 0


-- main
main :: IO ()
main = do
  json <- readFile "test.json"
  putStrLn $ pretty $ fromMaybe JsonNull $ parseJson json
