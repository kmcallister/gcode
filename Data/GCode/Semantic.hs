{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving #-}
module Data.GCode.Semantic
    ( -- * G-code commands
      Command(..)
    , File(..)
      -- * Converting
    , render
    , toRaw
      -- * Parameter types
    , Position(..), PathTolerance(..)
    , Axis(..), Plane(..), ArcDirection(..), LatheMode(..), Units(..)
    , DistanceMode(..), Coolant(..)
    , Seconds, Tolerance
    ) where

import Data.Typeable ( Typeable )
import Data.Data     ( Data     )
import Data.Monoid
import Text.PrettyPrint ( Doc )

import qualified Data.GCode.Raw as R

import Data.GCode.Axes ( Axis(..), Position(..) )

-- | A plane for arc motion.
data Plane = XY | ZX | YZ | UV | WU | VW
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Direction of arc.
data ArcDirection
    = CW   -- ^ Clockwise
    | CCW  -- ^ Counter-clockwise
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Position mode on a lathe.
data LatheMode = Diameter | Radius
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Length units.
data Units = Inches | Millimeters
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Distance mode.
data DistanceMode
    = Absolute     -- ^ Absolute in current coordinate system
    | Incremental  -- ^ Relative to the current coordinate
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Coolant mode.
data Coolant
    = Mist
    | Flood
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Tolerance of deviation from programmed path.
data PathTolerance

    -- | Motion must be exact.
    = Exact

    -- | Motion must be exact, and the controlled point must have zero speed
    -- at the start and end of a move.
    | ExactWithStop

    -- | Keep best possible speed, regardless of deviation.
    | BestSpeed

    -- | Allow a certain deviation from the programmed path.
    | Blend    Tolerance

    -- | Allow a certain deviation, with a separate tolerance for the
    -- \"naive cam detector\".
    | BlendCam Tolerance Tolerance
    deriving (Eq, Ord, Read, Show, Typeable, Data)

type Seconds   = Rational
type Tolerance = Rational

data Command

    -- Motion commands
    = Rapid            [Position]     -- ^ Rapid linear motion (G0)
    | Linear           [Position]     -- ^ Linear motion at feed rate (G1)
    | Arc              ArcDirection [Position]  -- ^ Arc motion (G2, G3)
    | QuadBSpline      [Position]     -- ^ Quadratic B-spline motion (G5.1)
    | SpindleSync      [Position]     -- ^ Spindle-synchronized motion (G33)
    | RigidTapping     [Position]     -- ^ Rigid tapping motion (G33.1)
    | Dwell            Seconds        -- ^ Dwell (G4)
    | CancelMotion                    -- ^ Cancel modal motion (G80)
    | SpindleOn        ArcDirection   -- ^ Start spindle (M3, M4)
    | SpindleOff                      -- ^ Stop spindle (M5)
    | CoolantOn        Coolant        -- ^ Coolant on (M7, M8)
    | CoolantOff                      -- ^ Coolant off (M9)

    -- Modes and settings
    | FeedRate         Rational       -- ^ Set feed rate (F)
    | SpindleSpeed     Rational       -- ^ Set spindle speed (S)
    | LatheMode        LatheMode      -- ^ Select lathe unit mode (G7, G8)
    | Plane            Plane          -- ^ Select arc plane (G17 - G19.1)
    | Units            Units          -- ^ Select unit system (G20, G21)
    | PathTolerance    PathTolerance  -- ^ Set path tolerance (G61, G61.1, G64)
    | DistanceMode     DistanceMode   -- ^ Select distance mode (G90, G91)
    | ArcDistanceMode  DistanceMode   -- ^ Select distance mode for arc offsets (G90.1, G91.1)

    -- Program control
    | Pause                           -- ^ Pause program (M0)
    | ConditionalPause                -- ^ Pause program if stop switch is enabled (M1)
    | End                             -- ^ End program (M2)

    -- | Invoke a user command named \"@M100@\" through \"@M199@\", given the corresponding
    -- number and two command-line parameters.
    | UserCommand      Int Rational Rational

    -- | Raw G-code.
    | Other            R.Line

    -- | A G-code comment.  Should not contain '@)@'.  Certain comments may be interpreted
    -- specially by the implementation.
    | Comment          String
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | An entire file.
newtype File = File [Command]
    deriving (Eq, Ord, Read, Show, Typeable, Data, Monoid)

-- | Convert semantic commands to raw G-code words.
toRaw :: File -> R.File
toRaw (File cmds) = R.File (map cmd cmds) where
    w = R.Word

    cmd :: Command -> R.Line
    cmd (Other   ln) = ln
    cmd (Comment xs) = R.Line [R.TComment xs]
    cmd c = R.Line $ map R.TWord (cw c)

    g :: Rational -> [Position] -> [R.Word]
    g n ps = w 'G' n : map f ps where
        f (Pos axis value) = w (head $ show axis) value

    cw :: Command -> [R.Word]

    cw (Rapid        xs) = g 0    xs
    cw (Linear       xs) = g 1    xs
    cw (Arc      CW  xs) = g 2    xs
    cw (Arc      CCW xs) = g 3    xs
    cw (QuadBSpline  xs) = g 5.1  xs
    cw (SpindleSync  xs) = g 33   xs
    cw (RigidTapping xs) = g 33.1 xs

    cw (Dwell n)         = [w 'G' 4, w 'P' n]
    cw CancelMotion      = [w 'G' 80]
    cw (SpindleOn CW )   = [w 'M' 3]
    cw (SpindleOn CCW)   = [w 'M' 4]
    cw SpindleOff        = [w 'M' 5]
    cw (CoolantOn Mist ) = [w 'M' 7]
    cw (CoolantOn Flood) = [w 'M' 8]
    cw CoolantOff        = [w 'M' 9]
    cw (FeedRate     n)  = [w 'F' n]
    cw (SpindleSpeed n)  = [w 'S' n]

    cw (Plane XY) = [w 'G' 17]
    cw (Plane ZX) = [w 'G' 18]
    cw (Plane YZ) = [w 'G' 19]
    cw (Plane UV) = [w 'G' 17.1]
    cw (Plane WU) = [w 'G' 18.1]
    cw (Plane VW) = [w 'G' 19.1]

    cw (LatheMode Diameter)          = [w 'G' 7]
    cw (LatheMode Radius  )          = [w 'G' 8]
    cw (Units Inches     )           = [w 'G' 20]
    cw (Units Millimeters)           = [w 'G' 21]
    cw (DistanceMode    Absolute   ) = [w 'G' 90]
    cw (DistanceMode    Incremental) = [w 'G' 91]
    cw (ArcDistanceMode Absolute   ) = [w 'G' 90.1]
    cw (ArcDistanceMode Incremental) = [w 'G' 91.1]

    cw (PathTolerance Exact)              = [w 'G' 61]
    cw (PathTolerance ExactWithStop)      = [w 'G' 61.1]
    cw (PathTolerance BestSpeed)          = [w 'G' 64]
    cw (PathTolerance (Blend tol))        = [w 'G' 64, w 'P' tol]
    cw (PathTolerance (BlendCam tol cam)) = [w 'G' 64, w 'P' tol, w 'Q' cam]

    cw Pause            = [w 'M' 0]
    cw ConditionalPause = [w 'M' 1]
    cw End              = [w 'M' 2]

    cw (UserCommand n p q)
        | (100 <= n) && (n <= 199) = [w 'M' (fromIntegral n), w 'P' p, w 'Q' q]
        | otherwise = error ("Data.GCode: bad user command number " ++ show n)

    cw (Other   _) = error "not possible"
    cw (Comment _) = error "not possible"


-- | Convert semantic commands to concrete G-code syntax.
--
-- Calling @'show'@ provides the @'String'@ syntax.
render :: File -> Doc
render = R.render . toRaw
