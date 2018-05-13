module SillyPlayer where
import SedmaBase
import Player


sillyPlayer :: AIPlayer MState
sillyPlayer trick (MState inHand _) = inHand !! 0
