module I18n (titleMap, descriptionMap) where

import qualified Data.Map as M (Map)
import System.FilePath (</>, takeBaseName, takeDirectory)

import Hakyll

type Language = String

type LanguageMap = M.Map Language String

descriptionMap :: LanguageMap
descriptionMap =
  M.fromList
  [ ("en", "On software in general, with focus on Elm, Elixir")
  , ("ko", "소프트웨어 전반에 대한 개인 블로그. Elm, Elixir")
  ]

url: "https://harfangk.github.io"
github_username:  harfangk
twitter_username: harfangk

getRoute :: Identifier -> Routes
getRoute i =
  let path = toFilePath i
