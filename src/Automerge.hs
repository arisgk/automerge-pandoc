{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans, AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), TextSpan (..), Mark (..), Link (..), toJSONText) where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Bool, String), eitherDecode, encode, object, withObject, withScientific, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Utils.JSON (parseNonEmpty, parseStringifiedObject, stringifyObject)

data Link = Link {url :: T.Text, title :: T.Text} deriving (Show, Eq)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    linkUrl <- v .: "href" >>= parseNonEmpty "href"
    linkTitle <- v .: "title"
    pure Link {url = linkUrl, title = linkTitle}

instance ToJSON Link where
  toJSON link = object ["href" .= url link, "title" .= title link]

data Mark
  = Strong
  | Emphasis
  | LinkMark Link
  deriving (Show, Eq)

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype Heading = Heading HeadingLevel deriving (Show, Eq)

data BlockType
  = ParagraphType
  | HeadingType
  | CodeBlockType
  | BlockQuoteType
  | OrderedListItemType
  | UnorderedListItemType
  | ImageType
  deriving (Show, Eq)

instance FromJSON BlockType where
  parseJSON :: Value -> Parser BlockType
  parseJSON = withText "BlockType" $ \t -> case t of
    "paragraph" -> pure ParagraphType
    "heading" -> pure HeadingType
    "code-block" -> pure CodeBlockType
    "blockquote" -> pure BlockQuoteType
    "ordered-list-item" -> pure OrderedListItemType
    "unordered-list-item" -> pure UnorderedListItemType
    "image" -> pure ImageType
    _ -> fail "Invalid block type"

instance ToJSON BlockType where
  toJSON :: BlockType -> Value
  toJSON blockType = case blockType of
    ParagraphType -> String "paragraph"
    HeadingType -> String "heading"
    CodeBlockType -> String "code-block"
    BlockQuoteType -> String "blockquote"
    OrderedListItemType -> String "ordered-list-item"
    UnorderedListItemType -> String "unordered-list-item"
    ImageType -> String "image"

data BlockMarker
  = ParagraphMarker
  | HeadingMarker Heading
  | CodeBlockMarker
  | BlockQuoteMarker
  | OrderedListItemMarker
  | UnorderedListItemMarker
  | ImageBlockMarker
  deriving (Show, Eq)

data TextSpan = AutomergeText {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (AutomergeText value1 marks1) (AutomergeText value2 marks2) = AutomergeText (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = AutomergeText T.empty []

data AutomergeSpan
  = BlockSpan BlockMarker [BlockType]
  | TextSpan TextSpan
  deriving (Show)

instance FromJSON AutomergeSpan where
  parseJSON = withObject "AutomergeSpan" $ \v -> do
    elementType <- (v .: "type" :: Parser String)
    case elementType of
      "block" -> parseBlock v
      "text" -> parseInline v
      _ -> fail "Unknown span type"

parseBlock :: Object -> Parser AutomergeSpan
parseBlock v = do
  blockData <- v .: "value"
  blockType <- (blockData .: "type" :: Parser BlockType)
  parents <- (blockData .: "parents" :: Parser [BlockType])
  case blockType of
    ParagraphType -> pure $ BlockSpan ParagraphMarker parents
    HeadingType -> do
      attrs <- blockData .: "attrs"
      level <- attrs .: "level"
      pure $ BlockSpan (HeadingMarker $ Heading $ HeadingLevel level) parents
    CodeBlockType -> pure $ BlockSpan CodeBlockMarker parents
    BlockQuoteType -> pure $ BlockSpan BlockQuoteMarker parents
    OrderedListItemType -> pure $ BlockSpan OrderedListItemMarker parents
    UnorderedListItemType -> pure $ BlockSpan UnorderedListItemMarker parents
    ImageType -> pure $ BlockSpan ImageBlockMarker parents

parseInline :: Object -> Parser AutomergeSpan
parseInline v = do
  parsedValue <- v .: "value"
  marksKeyMap <- v .:? "marks" .!= KM.empty
  parsedMarks <- parseMarks marksKeyMap
  pure $ TextSpan $ AutomergeText parsedValue parsedMarks

parseMarks :: KM.KeyMap Value -> Parser [Mark]
parseMarks = mapM parseMark . KM.toList

parseMark :: (K.Key, Value) -> Parser Mark
parseMark (k, String txt)
  | K.toText k == "link" = parseStringifiedObject txt >>= (pure . LinkMark)
parseMark (k, Bool True) = case K.toText k of
  "strong" -> pure Strong
  "em" -> pure Emphasis
  _ -> fail $ "Unexpected mark with boolean value: " ++ T.unpack (K.toText k)
parseMark _ = fail "Invalid format in marks"

parseAutomergeSpans :: BL.ByteString -> Either String [AutomergeSpan]
parseAutomergeSpans = eitherDecode

instance ToJSON AutomergeSpan where
  toJSON (BlockSpan blockMarker parents) = case blockMarker of
    ParagraphMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
                "type" .= T.pack "paragraph",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    HeadingMarker (Heading (HeadingLevel level)) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
                "type" .= T.pack "heading",
                "attrs" .= object ["level" .= level]
              ]
        ]
    CodeBlockMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
                "type" .= T.pack "code-block",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    BlockQuoteMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "blockquote",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    OrderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool True,
                "parents" .= parents,
                "type" .= T.pack "ordered-list-item",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    UnorderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
                "type" .= T.pack "unordered-list-item",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    ImageBlockMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
                "type" .= T.pack "image",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
  toJSON (TextSpan (AutomergeText val extractedMarks)) =
    object $
      [ "type" .= T.pack "text",
        "value" .= val
      ]
        <> ["marks" .= KM.fromList (map markToKeyVal extractedMarks) | not (null extractedMarks)]
    where
      markToKeyVal mark = case mark of
        Strong -> (K.fromText "strong", Bool True)
        Emphasis -> (K.fromText "em", Bool True)
        LinkMark link -> (K.fromText "link", String $ stringifyObject link)

toJSONText :: [AutomergeSpan] -> T.Text
toJSONText = decodeUtf8 . BSL8.toStrict . encode
