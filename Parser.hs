module Parser where

-- Represent the Page location
type Page = String

-- Represent the Image Path
type IMG = String

-- Represent the Hyper Link location
type Link = String

-- Record contains Page path and image path
data PageImage = PageImage
	      {
		  page :: Page,
		  img :: IMG
	      }
	      deriving (Eq, Show, Read)

-- Record contains Page path and hyper link path
data PageLink = PageLink
	      {
		  pageUrl :: Page,
		  link :: Link
	      }
	      deriving (Eq, Show, Read)


extractPageImages :: Page -> String -> [PageImage]
extractPageImages p xs = getPageImage p (extractImgPath xs)

getPageImage :: Page -> [IMG] -> [PageImage]
getPageImage p xs =
              map makePageImage xs
              where makePageImage x = PageImage {page = p, img = x}


extractImgPath :: String -> [IMG]
extractImgPath [] = []
extractImgPath xs = map parseImgSrc (parseIMGs xs)


parseIMGs :: String -> [IMG]
parseIMGs [] = []
parseIMGs ('<':'i':'m':'g':xs) = ("<img" ++ img ++ ">") : (parseIMGs $ tail rest)
     where (img, rest) = break space xs
           space c = elem c ['>']
parseIMGs (_:xs) = parseIMGs xs


parseImgSrc :: String -> String
parseImgSrc [] = []
parseImgSrc ('<':'i':'m':'g':xs) = parseImgSrc (tail $ dropWhile notSpace xs)
parseImgSrc ('s':'r':'c':'=':xs) =  takeWhile notQuots (tail $ dropWhile notQuots xs)
parseImgSrc (_:xs) = parseImgSrc (dropWhile empty xs)

-------------------------------------------------------------------------------------------

extractPageLinks :: Page -> String -> [PageLink]
extractPageLinks p xs = getPageLink p (extractLinkPath xs)

getPageLink :: Page -> [Link] -> [PageLink]
getPageLink p xs =
              map makePageLink xs
              where makePageLink x = PageLink {pageUrl = p, link = x}

extractLinkPath :: String -> [Link]
extractLinkPath [] = []
extractLinkPath xs = map parseHref (parseHyperlinks xs)

parseHyperlinks :: String -> [Link]
parseHyperlinks [] = []
parseHyperlinks ('<':'a':' ':xs) = ("<a " ++ img ++ ">") : (parseHyperlinks $ tail rest)
     where (img, rest) = break space xs
           space c = elem c ['>']
parseHyperlinks (_:xs) = parseHyperlinks xs


parseHref :: String -> String
parseHref [] = []
parseHref ('<':'a':xs) = parseHref (tail $ dropWhile notSpace xs)
parseHref ('h':'r':'e':'f':'=':xs) =  takeWhile notQuots (tail $ dropWhile notQuots xs)
parseHref (_:xs) = parseHref (dropWhile empty xs)


empty c = c ==' '
notSpace c = not (c ==' ')
notQuots c = not (c =='"' || c == '\'')
notSlash c = not (c =='/')




















