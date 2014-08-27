import qualified Text.Parsec             as P
import qualified Text.Parsec.ByteString  as PB
import qualified Text.Parsec.Combinator  as PC
-- import qualified Text.Parsec.Indent      as PI
import qualified Text.Parsec.Char        as PChar
import qualified Text.Parsec.Prim        as PP
-- import qualified Data.Monoid             as DM
import           Control.Applicative     as CA

-- data MegaHaml = Empty | Node Element [MegaHaml]
-- type Element = String

main :: IO ()
main = do
  e <- inputHaml
  print ((show e ) !! 0)
  return ()

inputHaml :: IO (Either P.ParseError SoupOfTags)
inputHaml = PB.parseFromFile megaParser "input.haml"

megaParser :: PB.Parser SoupOfTags
megaParser = do
  tagList <- PC.many1 aTag'
  return (SoupOfTags tagList)

aTag' :: PB.Parser Tag
aTag' = Tag <$>
    (PChar.spaces *>
    PChar.char '%' *>
    (P.sourceLine <$> PP.getPosition))
    <*>
    (PC.many1 PChar.alphaNum <*
    PChar.spaces)

data SoupOfTags = SoupOfTags [Tag]        deriving (Show)
data Tag        = Tag {
              _line    :: Int,
              _tagName :: String }         deriving (Show)
