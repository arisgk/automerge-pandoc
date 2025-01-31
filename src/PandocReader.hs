module PandocReader (toPandoc) where

import Automerge (AutomergeSpan (..), BlockMarker (..), BlockType (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), TextSpan (..))
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Inlines, Many (..), blockQuote, bulletList, codeBlock, codeBlockWith, doc, emph, fromList, header, headerWith, link, orderedListWith, para, singleton, str, strong, toList)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Utils.Sequence (lastValue, withoutLast)

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
toPandoc spans = pure . doc $ convertAutomergeSpans spans

convertAutomergeSpans :: [AutomergeSpan] -> Blocks
convertAutomergeSpans = foldl' convertAutomergeSpan (Many Seq.Empty)

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan blockMarker parents) = addBlockToHierarchy acc (convertBlockSpan blockMarker) parents
convertAutomergeSpan acc (TextSpan textSpan) = case lastValue acc of
  Nothing -> acc <> convertAndWrapToParagraph textSpan
  Just block -> withoutLast acc <> convertAndAddTo (getLeafBlock block) textSpan

addBlockToHierarchy :: Blocks -> Blocks -> [BlockType] -> Blocks
addBlockToHierarchy previousBlocks currentBlock parents = case parents of
  [] -> previousBlocks <> currentBlock
  (rootAncestor : otherAncestors) ->
    case lastBlock of
      -- No block found before the current one; TODO: return a mapping error
      Nothing -> undefined
      Just block ->
        if isMatchingContainerBlock rootAncestor block
          then withoutLast previousBlocks <> (updateBlockChildren block $ addBlockToHierarchy (getBlockChildren block) currentBlock otherAncestors)
          -- The container block doesn't match the ancestor type. TODO: return a mapping error.
          else undefined
  where
    lastBlock = lastValue previousBlocks

updateBlockChildren :: Block -> Blocks -> Blocks
updateBlockChildren parent newChildren = case parent of
  BlockQuote _ -> blockQuote newChildren
  OrderedList attrs _ -> orderedListWith attrs $ toList (singleton newChildren)
  BulletList _ -> bulletList $ toList (singleton newChildren)
  _ -> undefined

getBlockChildren :: Block -> Blocks
getBlockChildren block = case block of
  BlockQuote children -> fromList children
  _ -> undefined

getLeafBlock :: Block -> Block
getLeafBlock block = case toList blockChildren of
  [] -> block
  _ -> getLeafBlock $ last $ toList blockChildren
  where
    blockChildren = getBlockChildren block

isMatchingContainerBlock :: BlockType -> Block -> Bool
isMatchingContainerBlock parentBlockType block = case block of
  BlockQuote _ -> if parentBlockType == BlockQuoteType then True else False
  OrderedList _ _ -> if parentBlockType == OrderedListItemType then True else False
  BulletList _ -> if parentBlockType == UnorderedListItemType then True else False
  _ -> False

convertBlockSpan :: BlockMarker -> Blocks
convertBlockSpan blockMarker = case blockMarker of
  ParagraphMarker -> para (Many Seq.Empty)
  HeadingMarker (Heading (HeadingLevel level)) -> header level $ Many Seq.Empty
  CodeBlockMarker -> codeBlock T.empty
  BlockQuoteMarker -> blockQuote $ Many Seq.Empty
  _ -> undefined -- more blocks to be implemented

convertAndWrapToParagraph :: TextSpan -> Blocks
convertAndWrapToParagraph = para . convertTextSpan

convertAndAddTo :: Block -> TextSpan -> Blocks
convertAndAddTo block textSpan = case block of
  Para inlines -> para $ addTextSpanToInlines (fromList inlines) textSpan
  Header level attr inlines -> headerWith attr level $ addTextSpanToInlines (fromList inlines) textSpan
  CodeBlock attr _ -> codeBlockWith attr $ value textSpan
  BlockQuote _ -> blockQuote $ convertAndWrapToParagraph textSpan
  _ -> undefined

addTextSpanToInlines :: Inlines -> TextSpan -> Inlines
addTextSpanToInlines inlines textSpan = inlines <> convertTextSpan textSpan

convertTextSpan :: TextSpan -> Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: TextSpan -> Inlines
convertTextToInlines = str . value

convertMarksToInlines :: TextSpan -> Inlines -> Inlines
convertMarksToInlines textSpan inlines = foldl' (flip markToInlines) inlines $ marks textSpan

markToInlines :: Mark -> Inlines -> Inlines
markToInlines mark = case mark of
  Automerge.Strong -> strong
  Automerge.Emphasis -> emph
  Automerge.LinkMark automergeLink -> link (url automergeLink) (title automergeLink)