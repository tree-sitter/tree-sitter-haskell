module TSH.Test.GapsTest where

import qualified Data.List.NonEmpty as NonEmpty
import Hedgehog ((===))

import qualified TSH.Data.Unicode
import TSH.Data.Unicode (Codepoint (Codepoint), CodepointBlock (CodepointBlock))
import TSH.Test.Utils (UnitTest)
import TSH.Unicode (pointBlocks)

point :: Word64 -> Codepoint
point num = Codepoint {num, cat = "Cc", name = "esc"}

points1_1 :: NonEmpty Codepoint
points1_1 = point <$> [5..7]

points1_2 :: NonEmpty Codepoint
points1_2 = point <$> [9..11]

points1_3 :: NonEmpty Codepoint
points1_3 = point <$> [34..38]

points1_4 :: NonEmpty Codepoint
points1_4 = point <$> [86..90]

points1_5 :: NonEmpty Codepoint
points1_5 = point <$> [93..95]

block :: NonEmpty Codepoint -> CodepointBlock
block points =
  CodepointBlock {points, minPoint = (NonEmpty.head points).num, maxPoint = (NonEmpty.last points).num}

blocks1 :: NonEmpty CodepointBlock
blocks1 = pointBlocks 10 (points1_1 <> points1_2 <> points1_3 <> points1_4 <> points1_5)

target1 :: NonEmpty CodepointBlock
target1 =
  [
    block (points1_1 <> points1_2),
    block points1_3,
    block (points1_4 <> points1_5)
  ]

points2_1 :: NonEmpty Codepoint
points2_1 = point <$> [0..5]

points2_2 :: NonEmpty Codepoint
points2_2 = point <$> [100..105]

blocks2 :: NonEmpty CodepointBlock
blocks2 = pointBlocks 10 (points2_1 <> points2_2)

target2 :: NonEmpty CodepointBlock
target2 =
  [
    block points2_1,
    block points2_2
  ]

test_gaps :: UnitTest
test_gaps = do
  target1 === blocks1
  target2 === blocks2
