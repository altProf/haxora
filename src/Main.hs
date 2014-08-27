import qualified Data.Attoparsec.ByteString.Lazy  as PB
import qualified Data.Attoparsec.ByteString.Char8 as PChar
import qualified Data.Attoparsec.Combinator       as PC
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Internal         as BSI
import qualified Control.Applicative              as CA

main :: IO ()
main = do
  e <- inputHaml
  print ((show e) !! 0)
  return ()

inputHaml :: IO (Either String SoupOfTags)
inputHaml = (BSL.readFile "input.haml") >>=
  (return . PB.eitherResult . (PB.parse megaParser))

megaParser :: PB.Parser SoupOfTags
megaParser = do
  tagList <- PC.many1 aTag
  return (SoupOfTags tagList)

aTag :: PB.Parser Tag
aTag = do
  _ <- tabsOrWhitespacesOrNewLines
  _ <- PChar.char '%'
  tagName <- PC.many1 $ PB.satisfy (PChar.isAlpha_ascii . BSI.w2c)
  -- position <- PP.getPosition
  _ <- tabsOrWhitespacesOrNewLines
  return (Tag (0) (BSL.pack tagName))
  -- return (Tag (P.sourceLine position) (BSLC.pack tagName))

tabsOrWhitespacesOrNewLines :: PB.Parser [Char]
tabsOrWhitespacesOrNewLines = do
  CA.many (PC.choice [
      (PChar.char '\n'),
      PChar.space
      -- (PChar.char ' ')
      ])

data SoupOfTags = SoupOfTags ![Tag]        deriving (Show)
data Tag        = Tag !Line !BSL.ByteString deriving (Show)
type Line = Int

