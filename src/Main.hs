import qualified Text.Parsec             as P
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Lazy.Char8   as BSLC
import qualified Text.Parsec.ByteString.Lazy  as PB
import qualified Text.Parsec.Combinator  as PC
-- import qualified Text.Parsec.Indent      as PI
import qualified Text.Parsec.Char        as PChar
import qualified Text.Parsec.Prim        as PP
-- import qualified Data.Monoid             as DM
import qualified Control.Applicative     as CA

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
  tagList <- PC.many1 aTag
  return (SoupOfTags tagList)

aTag :: PB.Parser Tag
aTag = do
  _ <- tabsOrWhitespacesOrNewLines
  _ <- PChar.char '%'
  tagName <- PC.many1 PChar.alphaNum
  position <- PP.getPosition
  _ <- tabsOrWhitespacesOrNewLines
  return (Tag (P.sourceLine position) (BSLC.pack tagName))

tabsOrWhitespacesOrNewLines :: PB.Parser [Char]
tabsOrWhitespacesOrNewLines = do
  CA.many (PC.choice [
      (PChar.char '\n'),
      (PChar.char ' ')
      ])

data SoupOfTags = SoupOfTags [Tag]        deriving (Show)
data Tag        = Tag Line BSL.ByteString deriving (Show)
type Line = Int

