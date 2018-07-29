{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens
import           Control.Monoid          ((<>))
import           Data.Maybe              (catMaybes)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Network.Wreq
import           Text.HTML.TagSoup

-- NOTE(dbp 2018-07-28): Allow us to use Text everywhere
instance TagRep Text where
  toTagRep t = toTagRep (T.unpack t)

-- NOTE(dbp 2018-07-28): Helper for grabbing contents of url as TagSoup tags
tagUrl :: Text -> IO [Tag Text]
tagUrl url =
  do r <- get (T.unpack url)
     let index = r ^. responseBody
     return $ parseTags (TL.toStrict . TL.decodeUtf8 $ index)

main :: IO ()
main = do peopleLinks <- getUrls
          emails <- catMaybes <$> mapM getEmail peopleLinks
          T.writeFile "emails.txt" (T.unlines emails)


getUrls :: IO [Text]
getUrls = do tags <- tagUrl "https://www.ccis.northeastern.edu/role/phd-students/"
             return $ filter (T.isPrefixOf "https://www.ccis.northeastern.edu/people/" ) $ map (fromAttrib "href" . head) $ partitions (~== ("<a>" :: Text)) $ dropWhile (~/= ("<article class=people>" :: Text)) tags

getEmail :: Text -> IO (Maybe Text)
getEmail url = do tags <- tagUrl url
                  let a = dropWhile (~/= ("<a>" :: Text)) $ dropWhile (~/= ("<p class=contact-email>" :: Text)) tags
                  case a of
                    (TagOpen _ _ : _) ->
                      return (Just $ T.drop 7{-mailto:-} $ fromAttrib "href" $ head $ a)
                    _ -> do print $ "Missing " <> url
                            return Nothing

