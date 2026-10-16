module Main where

import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List

import Debug.Trace

helpMessage = "\
\Usage: base64 [OPTIONS] \n\
\ \n\
\A base64 encoder and decoder written in haskell \n\
\This silliness was inspired by Dave Eddy's bash implementation of the same program \n\
\ \n\
\Decode/Encodes data to/from base64 from stdin and outputs to stdout \n\
\ \n\
\Options \n\
\  -h, --help \n\
\          Show this help message \n\
\  -d         \n\
\          Decode from base64 \
\ "

to_char :: Word32 -> Char
to_char c
    | c >= 0 && c <= 63 = 
        let _c :: Int = fromIntegral c in
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !! _c
    | otherwise = '='

from_char :: Word8 -> Word32
from_char c
    | elem c ([65..90]++[97..122]++[48..57]++[43, 47]) =
        fromIntegral . fromJust $ elemIndex c ([65..90]++[97..122]++[48..57]++[43, 47])
    | otherwise = -1

do_chunk_encode :: [Word8] -> Integer -> String
do_chunk_encode [_o1, _o2, _o3] i = 
    let o1 = fromIntegral _o1 :: Word32 in
    let o2 = fromIntegral _o2 :: Word32 in
    let o3 = fromIntegral _o3 :: Word32 in

    let a :: Word32 = (o1 `shiftL` 16) .|. (o2 `shiftL` 8) .|. o3 in
    let s1 = (a `shiftR` 18) .&. 63 in
    let s2 = (a `shiftR` 12) .&. 63 in
    let s3 = if i > 1 then (a `shiftR` 6) .&. 63 else -1 in
    let s4 = if i > 2 then (a `shiftR` 0) .&. 63 else -1 in

    (to_char s1) : (to_char s2) : (to_char s3) : [to_char s4]

do_chunk_decode :: [Word8] -> Integer -> [Word8]
do_chunk_decode [_s1, _s2, _s3, _s4] i = 
    let s1 = from_char _s1 in
    let s2 = from_char _s2 in
    let s3 = from_char _s3 in
    let s4 = from_char _s4 in

    let a :: Word32 = (s1 `shiftL` 18) .|. (s2 `shiftL` 12) .|. (s3 `shiftL` 6) .|. s4 in

    let o1 = (a `shiftR` 16) .&. 255 in 
    let o2 = (a `shiftR`  8) .&. 255 in
    let o3 = (a `shiftR`  0) .&. 255 in

    fromIntegral o1 : 
    if i > 2 then fromIntegral o2 : 
    if i > 3 then [fromIntegral o3]
    else [] else []

to_chunks_encode :: [Word8] -> String
to_chunks_encode (o1:o2:o3:xs) = (do_chunk_encode [o1, o2, o3] 3) ++ to_chunks_encode xs
to_chunks_encode [o1, o2] = do_chunk_encode [o1, o2, 0] 2
to_chunks_encode [o1] = do_chunk_encode [o1, 0, 0] 1
to_chunks_encode [] = ""

to_chunks_decode :: [Word8] -> [Word8]
to_chunks_decode (s1:s2:s3:s4:xs) = (do_chunk_decode [s1, s2, s3, s4] 4) ++ to_chunks_decode xs
to_chunks_decode [s1, s2, s3] = do_chunk_decode [s1, s2, s3, 65] 3
to_chunks_decode [s1, s2] = do_chunk_decode [s1, s2, 65, 65] 2
to_chunks_decode [s1] = do_chunk_decode [s1, 65, 65, 65] 1
to_chunks_decode [] = []

newLine :: String -> String
newLine s
    | s == "" = ""
    | (last s) == '\n' = s ++ ""
    | otherwise = s ++ "\n"

encode :: BL.ByteString -> IO ()
encode = putStr . newLine . to_chunks_encode . BL.unpack

decode :: BL.ByteString -> IO ()
decode = BL.putStr . BL.pack . to_chunks_decode . (filter (\x -> from_char x /= -1)) . BL.unpack

data Arg = Help
         | Decode 
         | Unknown String 
    deriving ( Eq, Show )

findUnknown :: [Arg] -> Maybe String
findUnknown [] = Nothing
findUnknown ((Unknown s):_) = Just s
findUnknown (_:args) = findUnknown args

base64 :: [Arg] -> BL.ByteString -> IO ()
base64 args bytes =
    let isArg = (\x -> elem x args) in

    let unknown = findUnknown args in
    if isJust unknown then do
        putStrLn $ "Unknown argument: " ++ fromJust unknown
        putStrLn helpMessage
    else if isArg Help then
        putStrLn helpMessage
    else if isArg Decode then
        decode bytes
    else
        encode bytes

parseArg :: String -> Arg
parseArg s = case s of
    "-d" -> Decode
    "-h" -> Help
    "--help" -> Help
    _ -> Unknown s

parseArgs :: [String] -> [Arg]
parseArgs = map parseArg

main :: IO ()
main = do
    _args <- getArgs
    let args = parseArgs _args
    contents <- BL.getContents
    base64 args contents
