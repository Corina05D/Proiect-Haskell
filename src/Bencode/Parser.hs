module Bencode.Parser where

import Bencode.Value
import qualified Data.List as L
import Parsec (Parser, andThen, orElse, pMap, pThen)
import qualified Parsec as P
import Result

-- | Parse a bencode value
--
-- >>> P.runParser value "i10e"
-- Success (BencodeInt 10, "")
--
-- >>> P.runParser value "3:abc"
-- Success (BencodeString "abc", "")
--
-- >>> P.runParser value "l3:abc4:abcde"
-- Success (BencodeList [BencodeString "abc",BencodeString "abcd"], "")
--
-- >>> P.runParser value "d3:abci10ee"
-- Success (BencodeDict [("abc",BencodeInt 10)], "")
value :: Parser BencodeValue
value =
  (pMap BencodeString string)
    `orElse` (pMap BencodeInt int)
    `orElse` (pMap BencodeList list)
    `orElse` (pMap BencodeDict dict)

-- | Parse a bencode integer
--
-- >>> P.runParser int "i10e"
-- Success (10, "")
int :: Parser Int
int = P.parser inner
 where 
  inner input =
   case P.runParser (P.char 'i') input of
     Success (c, r) -> case P.runParser P.number r of
                        Success (n, r2) -> case P.runParser (P.char 'e') r2 of
                                            Success (c2, r3) -> Success (n, r3)
                                            Error er -> Error (P.UnexpectedInput input "wrong")
                        Error er -> Error (P.UnexpectedInput input "wrong")
     Error er -> Error (P.UnexpectedInput input "wrong")

-- | Parse a bencode string
--
-- >>> P.runParser string "3:abc"
-- Success ("abc", "")
string :: Parser String
string = P.parser inner
 where
    inner input =
      case P.runParser P.number input of
        Success (n, r) -> case P.runParser (P.char ':') r of
                               Success (_, r2) -> case P.runParser (P.take n) r2 of 
                                                      Success (s, r3) -> Success (s, r3)
                                                      Error er -> Error (P.UnexpectedInput input "wrong")
                               Error er -> Error (P.UnexpectedInput input "wrong")
        Error er -> Error (P.UnexpectedInput input "wrong")

-- | Parse a bencode list
--
-- >>> P.runParser list "li1ei2ee"
-- Success ([BencodeInt 1,BencodeInt 2], "")
--
-- >>> P.runParser list "l1:a1:be"
-- Success ([BencodeString "a",BencodeString "b"], "")
list :: Parser [BencodeValue]
list = P.parser inner
 where
    inner input =
      case P.runParser (P.char 'l') input of
        Success (_, r) -> case P.runParser (P.many value) r of
                              Success (l, r2) -> case P.runParser (P.char 'e') r2 of
                                                        Success (_, r3) -> Success (l, r3)
                                                        Error er -> Error (P.UnexpectedInput input "wrong")
                              Error er -> Error (P.UnexpectedInput input "wrong")
        Error er -> Error (P.UnexpectedInput input "wrong")


-- | Parse a bencode dict
--
-- >>> P.runParser dict "d1:ai1e1:bi2ee"
-- Success ([(BencodeString "a", BencodeInt 1),(BencodeString "b",BencodeInt 2)], "")
dict :: Parser [BencodeKW]
dict = P.parser inner
  where
    inner input =
      case P.runParser (P.char 'd') input of
        Success (_, r) -> case P.runParser (P.many (P.andThen string value)) r of
                              Success (l, r2) -> case P.runParser (P.char 'e') r2 of
                                                        Success (_, r3) -> Success (l, r3)
                                                        Error er -> Error (P.UnexpectedInput input "wrong")
                              Error er -> Error (P.UnexpectedInput input "wrong")
        Error er -> Error (P.UnexpectedInput input "wrong")


-- | Convenience wrapper for `value`
--
-- >>> parse "i10e"
-- Success (BencodeInt 10)
parse :: String -> Result P.ParseError BencodeValue
parse input = fst <$> P.runParser value input
