
load('ref_tracks.RData')

tracks.from.csv <- read.tracks.csv('ref_tracks.csv', sep = ',')
tracks.from.csv.blank.sep <- read.tracks.csv('ref_tracks_blank_line_sep.csv', sep = ',',
                                             track.sep.blankline = T,
                                             time.column = 1, pos.columns = c(2:4))
tracks.from.df <- as.tracks.data.frame(read.csv('ref_tracks.csv'))

test_that("Tracks are loaded correctly", {
  expect_equivalent(tracks.from.csv, ref)
  expect_equivalent(tracks.from.df, ref)
  expect_equivalent(tracks.from.csv.blank.sep, ref)
} )

test_that("Tracks have correct structure", {
  expect_equal(class(tracks.from.df[[1]]), "matrix")
})

test_that("scale.t argument works", {
  scale.t <- 0.1
  tracks.from.csv.scale.t <-
    read.tracks.csv('ref_tracks_blank_line_sep.csv', sep = ',',
                    track.sep.blankline = T,
                    time.column = 1, pos.columns = c(2:4), scale.t = scale.t)
  ref.df <- as.data.frame(ref)
  ref.df$t <- ref.df$t * scale.t
  expect_equivalent(tracks.from.csv.scale.t, as.tracks(ref.df))
})

test_that("scale.pos argument works", {
  scale.pos <- 0.1
  tracks.from.csv.scale.pos <-
    read.tracks.csv('ref_tracks_blank_line_sep.csv', sep = ',',
                    track.sep.blankline = T,
                    time.column = 1, pos.columns = c(2:4), scale.pos = scale.pos)
  ref.df <- as.data.frame(ref)
  ref.df[,-c(1,2)] <- scale.pos * ref.df[,-c(1,2)]
  expect_equivalent(tracks.from.csv.scale.pos, as.tracks(ref.df))

test_that("Can choose strings or factors when converting to data frame", {
	expect_equal(class(as.data.frame.tracks(TCells,idsAsFactors=TRUE)[,"id"]),"factor")
	expect_equal(class(as.data.frame.tracks(TCells,idsAsFactors=FALSE)[,"id"]),"character")
})
