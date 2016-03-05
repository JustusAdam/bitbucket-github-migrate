{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           API.Bitbucket
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Foldable        (for_)
import           Text.Printf


testJsonParse = do
    file <- B.readFile "issues.json"
    let decoded = eitherDecode file :: Either String (PagedRequest Issue)

    either
        print
        (const $ return ())
        decoded
    
    commentsFile <- B.readFile "comments.json"
    
    either
        print
        (const $ return ())
        (eitherDecode commentsFile :: Either String (PagedRequest CommentShortInfo))
    
    commentFile <- B.readFile "comment.json"
    
    either
        print
        print
        (eitherDecode commentFile :: Either String Comment)
    

main = testJsonParse
