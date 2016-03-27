{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           API.Bitbucket
import           ClassyPrelude
import           Data.Aeson


testJsonParse :: IO ()
testJsonParse = do
    file <- readFile "issues.json"
    let decoded = eitherDecode file :: Either String (PagedRequest Issue)

    either
        print
        (const $ return ())
        decoded

    commentsFile <- readFile "comments.json"

    either
        print
        (const $ return ())
        (eitherDecode commentsFile :: Either String (PagedRequest CommentShortInfo))

    commentFile <- readFile "comment.json"

    either
        print
        print
        (eitherDecode commentFile :: Either String Comment)


main :: IO ()
main = testJsonParse
