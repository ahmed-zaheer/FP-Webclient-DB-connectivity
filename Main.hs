module Main where

import WebHTTP
import Parser
import DataAccessLayer
import System.Environment

{-
There are four modules as per project specification
 1. Database
 2. Parser
 3. HTTP (Got from crawler example and then unnecessary code is removed)
 4. Main (Execute the commands)

The implementation can work on any web page, it extract the images and then further crop the image path. 
Similary it also extract hyper links from given url and crop the href attribute.
Then save the extracted information in respective tables

-}

main = do args <- getArgs
          case args of
             ["create"] -> createDatabase	
             ["download", url] ->
             	do urlText <- downloadURL url
             	   let imgs = extractPageImages url urlText
             	   let links = extractPageLinks url urlText
             	   saveImage imgs
             	   saveLinks links
             ["loadSavedImgs"] -> loadImages	
             ["loadSavedLinks"] -> loadLinks
             ["show", url] ->
             	do urlText <- downloadURL url
		   print $  extractPageImages url urlText
		   print $  extractPageLinks url urlText
             _ -> syntaxError

syntaxError = putStrLn 
   "Usage: pod command [args]\n\
  \\n\
  \create           Initialize database Project.db\n\
  \download url     Download give url page and store images and links in database\n\
  \loadSavedImgs    Load images with their pages from database\n\
  \loadSavedLinks   Similary load links with their pages from database\n\
  \show url         Extract images and hyper links from give urls and print as list of PageImage and list of PageLink\n"
