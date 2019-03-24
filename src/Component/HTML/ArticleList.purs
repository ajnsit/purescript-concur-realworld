-- | This module exports a pure HTML function to render lists of articles in various formats,
-- | included paginated lists.
module Conduit.Component.HTML.ArticleList where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Conduit.Component.HTML.Utils (safeHref, whenElem)
import Conduit.Component.Part.FavoriteButton (favoriteButton, ButtonSize(..))
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Array (mapWithIndex)
import Data.Enum (enumFromTo)
import Data.Foldable (length)
import Data.Monoid (guard)
import Network.RemoteData (RemoteData(..))
import React.SyntheticEvent (SyntheticMouseEvent)


articleList
  :: forall p
   . (Int -> p)
  -> (Int -> p)
  -> RemoteData String (PaginatedArray ArticleWithMetadata)
  -> Widget HTML p
articleList favoriteQuery unfavoriteQuery = case _ of
  NotAsked ->
    text "Articles not yet loaded"
  Loading ->
    text "Loading..."
  Failure err ->
    text ("Error loading articles: " <> err)
  Success { body } | length body == 0 ->
    text "No articles are here...yet!"
  Success articles ->
    D.div'
      (articlePreview favoriteQuery unfavoriteQuery `mapWithIndex` articles.body)
  where
  text str =
    D.div
      [ P.className "article-preview" ]
      [ D.text str ]

articlePreview
  :: forall p
   . (Int -> p)
  -> (Int -> p)
  -> Int
  -> ArticleWithMetadata
  -> Widget HTML p
articlePreview favoriteQuery unfavoriteQuery ix article =
  D.div
  [ P.className "article-preview" ]
  [ D.div
    [ P.className "article-meta" ]
    [ D.a
      [ safeHref $ Profile username ]
      [ D.img
        [ P.src $ Avatar.toStringWithDefault avatar
        , P.alt $ Username.toString username
        ]
      ]
    , D.div
      [ P.className "info" ]
      [ D.a
        [ P.className "author", safeHref $ Profile username ]
        [ D.text $ Username.toString username ]
      , D.span
        [ P.className "date" ]
        [ D.text $ PDT.toDisplayWeekName article.createdAt ]
      ]
    , D.div
      [ P.className "pull-xs-right" ]
      [ favoriteButton Icon (favoriteQuery ix) (unfavoriteQuery ix) article ]
    ]
  , D.a
    [ P.className "preview-link"
    , safeHref $ ViewArticle article.slug
    ]
    [ D.h1'
        [ D.text article.title ]
    , D.p'
        [ D.text article.description ]
    , D.span'
        [ D.text "Read more..." ]
    , D.ul
        [ P.className "tag-list" ]
        (article.tagList <#> renderTag)
    ]
  ]
  where
    username = article.author.username
    avatar = article.author.image

renderTag :: forall p. String -> Widget HTML p
renderTag tag =
  D.li
  [ P.className "tag-default tag-pill tag-outline" ]
  [ D.text tag ]

-- Pagination

renderPagination :: forall p. (Int -> SyntheticMouseEvent -> p) -> Int -> PaginatedArray ArticleWithMetadata -> Widget HTML p
renderPagination query currentIndex { body, total } =
  whenElem (total > 20) \_ ->
    D.ul
      [ P.className "pagination" ]
      (map (renderPageLink query currentIndex) (enumFromTo 1 (total / 20)))

renderPageLink :: forall p. (Int -> SyntheticMouseEvent -> p) -> Int -> Int -> Widget HTML p
renderPageLink query activeIndex index =
  D.li
    [ P.className $ "page-item" <> guard (activeIndex == index) " active" ]
    [ D.a
      [ P.className "page-link"
      , P.href "" -- needed for realworld css; remember to prevent default!
      , query index <$> P.onClick
      ]
      [ D.text $ show index ]
    ]
