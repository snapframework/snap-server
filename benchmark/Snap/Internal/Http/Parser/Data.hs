{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Http.Parser.Data
    ( parseGetData
    , parseChunkedData
    )
    where

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L

parseGetData :: S.ByteString
parseGetData = S.concat
               [ "GET /favicon.ico HTTP/1.1\r\n"
               , "Host: 0.0.0.0=5000\r\n"
               , "User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0\r\n"
               , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
               , "Accept-Language: en-us,en;q=0.5\r\n"
               , "Accept-Encoding: gzip,deflate\r\n"
               , "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
               , "Keep-Alive: 300\r\n"
               , "Connection: keep-alive\r\n"
               , "\r\n" ]

parseChunkedData :: L.ByteString
parseChunkedData = L.fromChunks ["In the beginning, everything was void, and J.H.W.H. Conway began to create numbers.", "Conway said, \"Let there be two rules which bring forth all numbers larege and small.", "This shall be the first rule: Every number corresponds to two sets of previously created numbers, such that no member of the left set is greater than or equal to any member of the right set.", "And the second rule shall be this: One number is less than or equal to another number if and only if no member of the first number \'s left set is greater than or equal to the second number, and no member of the second number\'s right set is less than or equal to the first number.\" And Conway examined these two rules he had made, and behold! They were very good.", "And the first number was created from the void left set and the void right set. Conway called this number \"zero,\" and said that it shall be a sign to separate positive numbers from negative numbers.", "Conway proved that zero was less than or equal to zero, end he saw that it was good.", "And the evening and the morning were the day of zero.", "On the next day, two more numbers were created, one with zero as its left set and one with zero as its right set. And Conway called the former number \"one,\" and the latter he called \"minus one.\" And he proved that minus one is less than but not equal to zero and zero is less than but not equal to one.", "And the evening day.", "And Conway said, \"Let the numbers be added to each other in this wise: The left set of the sum of two numbers shall be the sums of all left parts of each number with the other; and in like manner the right set shall be from the right parts, each according to its kind.\" Conway proved that every number plus zero is unchanged, and he saw that addition was good.", "And the evening and the morning were the third day."]
