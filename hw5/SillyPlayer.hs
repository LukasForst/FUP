module SillyPlayer where
import SedmaBase
import Player


sillyPlayer :: AIPlayer State
sillyPlayer trick (State inHand _) = inHand !! 0
