module Parser where

import Data.Tuple
import Data.Char
import Control.Applicative
import Text.Regex

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonFloat Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(JsonValue, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f)  <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ (\_ -> Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = (\b -> JsonBool (b == "true")) <$> (stringP "true" <|> stringP "false")

jsonFloatRegex :: Regex
jsonFloatRegex = mkRegex "^-?(0|([1-9][0-9]*))(\\.[0-9]+([eE][+-]?[0-9]+)?)"

jsonIntRegex :: Regex
jsonIntRegex = mkRegex "^-?(0|([1-9][0-9]*))"

jsonInt :: Parser JsonValue
jsonInt = f <$> regexP jsonIntRegex
  where
    f x = JsonInt $ read x

jsonFloat :: Parser JsonValue
jsonFloat = f <$> regexP jsonFloatRegex
  where
    f x = JsonFloat $ read x

jsonNumber :: Parser JsonValue
jsonNumber = jsonFloat <|> jsonInt

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> nonQuoteP <* charP '"')

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> whitespaceP *> elements <* whitespaceP <* charP ']')
  where
    elements = sepByP (whitespaceP *> charP ',' <* whitespaceP) jsonValue 

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> whitespaceP *> pairs <* whitespaceP <* charP '}')
  where
    pairs = sepByP (whitespaceP *> charP ',' <* whitespaceP) pair
    pair = (\a _ b -> (a, b)) <$> jsonPrimitive <*> (whitespaceP *> charP ':' <* whitespaceP) <*> jsonValue

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sepP elemP = (:) <$> elemP <*> many (sepP *> elemP) <|> pure []

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | x == y = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

-- NOTE: Not so sure about this
nonQuoteP :: Parser String
nonQuoteP = regexP $ mkRegex "^((\\\\\")|([^\"]))*"

whitespaceP :: Parser String
whitespaceP = spanP isSpace

stringP :: String -> Parser String
stringP = sequenceA . map charP

regexP :: Regex -> Parser String
regexP p = Parser $ \input ->
  case matchRegexAll p input of
    Just ("", xs, ys, _) -> Just (ys, xs)
    _ -> Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ swap $ span f input

jsonPrimitive :: Parser JsonValue
jsonPrimitive = jsonNull
            <|> jsonBool
            <|> jsonNumber
            <|> jsonString

jsonValue :: Parser JsonValue
jsonValue = jsonNull
        <|> jsonBool
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject

parseJson :: String -> Maybe JsonValue
parseJson x = case runParser jsonValue x of
  Nothing -> Nothing
  Just ("", parsed) -> Just parsed
  _ -> Nothing

main :: IO ()
main = undefined
