{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Sokoban.Resources where

import Data.Maybe     (fromJust)
import Helper         (str)
import Sokoban.Level  (LevelCollection(..))
import Sokoban.Parser (parseLevels)

import qualified Data.Text as T

rawLevel :: String
rawLevel =
  [str|
      # # # # #
    # #   .   #
    #   .   $ #
  # # $ # . . #
  # @ $   *   #
  #   $       #
  # # #       #
      # # # # #
  ; The maze
; number 1
|]

yoshiroAutoCollection :: LevelCollection
yoshiroAutoCollection =
  LevelCollection
    { _title = "Yoshio Murase's Auto-Generated"
    , _description =
        [str|These sokoban screens were made automatically by computer. Yosio Murase wrote the program that
             created these levels and then made the levels available on his sokoban web pages.
             For information about the theory behind the creation of these levels, see Yoshio Murase,
             Hitoshi Matsubara, and Yuzuru Hiraga Automatic Making of Sokoban Problems, which was
             presented at the Pacific Rim Conference on AI in 1996.
             These levels are small (no more than 8x8), and contain only 3 stones to push, but can be delightfully
             tricky and surprisingly difficult to solve.|]
    , _email = "yoshio@asahi.email.ne.jp"
    , _url = "http://www.ne.jp/asahi/ai/yoshio/sokoban/auto52/auto52.htm"
    , _copyright = "Yoshio Murase"
    , _levels = fromJust $ parseLevels yoshiroAuto
    }

dh1Collection :: LevelCollection
dh1Collection =
  LevelCollection
    { _title = "gh1"
    , _description =
        [str|Some of the small puzzles had starting stone positions and solution generated by the
             computer program sokgen, but maze and goal positions designed by David Holland, the
             author of sokgen. These puzzles are generally pretty hard (David Holland has trouble
             solving them :-) and the non-computer-generated puzzles should be light relief.
             The puzzles are arranged in roughly ascending order of difficulty.|]
    , _email = "david@noether.freeserve.co.uk"
    , _url = "http://www.clickfest88.freeserve.co.uk/dh1/index.html"
    , _copyright = "David Holland"
    , _levels = fromJust $ parseLevels dh1
    }

testCollection :: LevelCollection
testCollection =
  LevelCollection
    { _title = "test"
    , _description = ""
    , _email = ""
    , _url = "http://sokobano.de/wiki/index.php?title=DHolland_S3"
    , _copyright = ""
    , _levels = fromJust $ parseLevels testRaw
    }

testRaw :: T.Text
testRaw = 
  T.pack
    [str|
; 1 Long
 #########
 #  ##   #
 #   @   #
 #  ### ##
### ### #
#..  ## #
####$## #
 #  $ # #
 #    # #
 # #  # ##
## ####  #
#        #
#   ###  #
##### ####

; 0 Simplest
#######
#@$ . #
#######

; 2 Solvable
    #####
    # @ ######
    # $      #
    ## ##### # #####
#####  #  ## ###   #
#   #$    #   #  # ##
# $    $ .. $    #  #
## ##### ## ###    .#
#  ####   #  ## ##  #
#       .    #   ####
############## . #
             #####

; 3 Unsolvable
    #####
    # @ ######
    # $      #
    ## ##### # #####
#####  #  ## ###   #
#   #     #   #  # ##
# $      ** $    #  #
## ##### ## ###    .#
#  ####   #  ## ##  #
#       .    #   ####
############## . #
             #####
|]  

yoshiroAuto :: T.Text
yoshiroAuto =
  T.pack
    [str|
; 1
 ######
##  . #
# * # #
# .$  #
#  #$##
## @ #
 #####

; 2
#######
#  .@ #
# #.# #
#   $ #
#.$$ ##
#  ###
####

; 3
   ####
#### @#
#  *$ #
#     #
## .###
 #$ #
 # .#
 ####

; 4
### ###
#.###.#
# #  .#
# $$ @#
#  $  #
#  #  #
#  ####
####

; 5
   ####
   # @##
####   #
#. #$$ #
#     ##
#.  $##
##.  #
 #####

; 6
#####
# ..####
# $    #
#  #$# #
# @ .$ #
########

; 7
  #####
###  .#
# $ # #
# *$  #
# .#@ #
#    ##
#   ##
#####

; 8
#######
#.  @.#
#  $# ##
# # $. #
#   $# #
####   #
   #####

; 9
#####
#. .###
#.#$$ #
#   @ #
# $#  #
##   ##
 #####

; 10
#####
#.  ###
# #   #
# . # #
# $*$ #
##@ ###
 #  #
 ####

; 11
########
#.   . #
# # #  #
#@$  $.#
##### $#
    #  #
    ####

; 12
####
#  #
#  #####
# .*   #
##$    #
 # #$###
 #. @#
 #####

; 13
 #####
 # @ ###
## .   #
#. $.$ #
##$# ###
 #   #
 #####

; 14
 #####
##   #
# $# #
# . @##
# *   #
## #$ #
 #.  ##
 #####

; 15
 ####
##  ####
#..$  .#
# #$ $ #
#@  #  #
#####  #
    ####

; 16
 ######
 #  .@##
 #   $.#
 ###*# #
##     #
#  $  ##
#   ###
#####

; 17
 ####
 #@ #
 #  #
##. ####
# $$. .#
#  $ ###
###  #
  ####

; 18
#####
#.  #
# # ###
# *$  #
#  $. #
#  @###
#####

; 19
  #####
  #   #
  # #.#
###  .#
#@ $$ #
#  .$ #
#######

; 20
######
#   @#
# $# ###
# * $  #
#   ## #
##.  . #
 ##   ##
  #####

; 21
######
#   @##
#  #  #
#.  $ #
# $$#.#
###  .#
  #####

; 22
  ####
###. #
# .  ###
#   $$ #
## . $@#
 #######

; 23
 ######
##@.  #
# $$* #
#  #  ##
#  #  .#
#### # #
   #   #
   #####

; 24
    ####
    #  #
  ###$.#
  #  . #
###  #.#
# $  $ #
#   #@ #
########

; 25
#####
#  .###
# $.. #
#  ##$##
##  #  #
 #$   @#
 #  ####
 ####

; 26
  ####
  #  #
  #  ###
### .. #
#  $#  #
#  .$$ #
#### @ #
   #####

; 27
#####
#   ###
# # *@##
#  *   #
###$   #
  #   .#
  ######

; 28
  ######
### .  #
# $@#. #
#  $# ##
#  *  #
##  # #
 ##   #
  #####

; 29
 ####
##  ###
#     ##
#  #$$@#
#  . *.#
########

; 30
 #######
##@    #
#. #   #
# $$$.##
# .#  #
#  ####
####

; 31
########
#      #
# # ##*#
# #@ $ #
#.$ .  #
#####  #
    #  #
    ####

; 32
 ######
 #@   ##
 ##$   #
### .  #
# $ #$##
# .  .#
####  #
   ####

; 33
#####
#   ###
#  $  #
##$$ .#
 #@ . #
 ## # #
  #  .#
  #####

; 34
#####
#   ####
# $$   #
# .#.  #
#  ## ##
#  ##$#
# @  .#
#######

; 35
######
# .  #
# .# ###
# @$$  #
# $.   #
########

; 36
########
# @.#  #
# .$ . #
#  #$  #
#  $  ##
###  ##
  #  #
  ####

; 37
 #######
##   . #
# $  $@#
#.$.####
#  ##
#  #
#  #
####

; 38
######
# .  #
#  #@#
#  $ ##
##$#  #
#   # #
#. *  #
#######

; 39
   #####
#### . #
# *@ . #
# $ #  #
# #  $ #
#   ####
#####

; 40
  ####
###  ###
# .. $.#
#  $$ @#
####   #
   #####

; 41
    ####
    #@ #
##### .#
# $ $ $#
#   .  #
### .  #
  ######

; 42
########
#   #  #
# #.$ $#
#   $  #
#####. #
  #   @#
  #   .#
  ######

; 43
   ####
  ##@ ##
 ##  ..#
## $#$##
#   $. #
#  #   #
#    ###
######

; 44
######
#   @#
# $$####
# $ .  #
## #.# #
#.   # #
#      #
########

; 45
   ####
   #  #
#### $##
# @$.  #
# ##   #
#   ## #
#   * .#
########

; 46
   #####
   # @ #
 ###   #
 # $ $##
## $  #
#.  # #
#..   #
#######

; 47
   #####
####. @#
#  .$  #
# #  ###
# $ $ .#
####   #
   #####

; 48
########
#  .# @#
# # $  #
# $.#$ #
## .   #
 #  ####
 ####

; 49
#######
#     #
#.## .#
#*  $@#
#  #$ #
#  #  #
#######

; 50
####
#. #
# $#
#  #####
# .$ @ #
# .$ # #
###    #
  ######

; 51
########
#      #
# #$   #
# $ @#.#
##$#.  #
 #    .#
 #######

; 52
######
#  . #
#    ###
# #$$. #
#.  ## #
#@$ ## #
###    #
  ######
|]

dh1 :: T.Text
dh1 =
  T.pack
    [str|
;1 Spade
      ####
    ###  ####
  ### $ $   #
  #   #..#$ #
  # $$#*.#  #
  #   ....$ #
 ## # .#.#  #
 # $##$#.#$ #
 # @$    .$ #
 ##  #  ##  #
  ###########

;2 Ninevah
   #####
####   #
#@$  # #
# $$$  #
##.#.  #
#  ..# #
#  #.* #
# $.$ ##
###   #
  #####

;3 CrissCross
   #####
  ##  @####
  #  #$$  #
###.#   # #
#  $#.#$  #
#   ...  ##
###$#.#$ #
  #     ##
  ###   #
    #####

;4 Carousel
   ####  #####
####  ####   #
# $  $     $ ####
# #   # ### #   #
#  $   $ # $$ $ #
###  ## $@$     ##
  #$ ##  # $$ $$ #
  #    $  ## ##  #
 ### #$##  ...# ##
 #  ..*.*..##.# #
 # #..*.....*.# #
 #   ########   #
 #####      #####

;5 Pattern-oster
  ####
  #  #
  #  # #######
  #  # #..   #
  # ####..##$###
  #  $ $.*     #
  # #$ #*.# $# #
###$#  #..#  # #
#   #$ #*.# $# #
# # #  $..#  # ##
# #$#$ #..$  #  #
#   #  #..#   # #
# #@# $#*.#$# # #
#  $#  #..#   # #
##   $ $ $ ###  #
 ####   #      ##
    ############

;6 Saturnine
##########
#   ##   ######
# $   $ $ #   #
# $ #  .$ #   #
###  ##.#  $ ##
# ##$##.###. #
#   *.......$#
#    ##.##   #
##$####.######
#  $ # . #   #
#  #  **$    #
#  $ # @ # $ #
# $   #####  #
###   #   ####
  #####

;7 briefcase alt 23
  #####
###   #
# $*@.###
# # *$  #
# #. .# #
# #$* $ #
#    .###
###   #
  #####

;8 Salamander v3
      #####
#######   ###
#   ## $ $  #
# # ##    # #
# #..$ $ ## #
#  .$# $#   #
###..# $ @$ #
  #**# $ # ##
  #..#   # #
  #..##### ##
  #.*#   $  #
  #..# $    #
  ##.### #####
  # $      $ #
  #   ####   #
  #####  #####

;9 Abstract
 ######
 #    #
##. * ##
# . $  #
# #**# #
#  *+* #
## $$ ##
 #    #
 ######

;10 Storm (in a teacup) v6
####
#  #######
#        #
#  ###$# #
##.# # . #
 # # #$* #
 #.### + #
 # $ #$* #
 # #   . #
##.####* #
#  $ $ * #
#       ##
#########
|]
