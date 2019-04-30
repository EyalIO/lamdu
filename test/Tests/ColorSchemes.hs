module Tests.ColorSchemes (test) where

import           Data.Aeson.Config (load)
import           Data.Data.Lens (template)
import qualified Data.Map as Map
import           GUI.Momentu.Draw (Color(..))
import qualified Lamdu.Config.Folder as ConfigFolder
import           Lamdu.Config.Theme (Theme)
import           System.FilePath (takeFileName)

import           Test.Lamdu.Prelude

test :: Test
test =
    ConfigFolder.getFiles ConfigFolder.themes >>= traverse_ verifyTheme
    & testCase "color-scheme"

verifyTheme :: FilePath -> IO ()
verifyTheme filename =
    load filename >>= verify
    where
        verify :: Theme -> IO ()
        verify theme
            | "retro.json" == takeFileName filename = traverse_ verifyRetroColor colors
            | Map.size saturations <= 3 = pure ()
            | otherwise =
                assertString
                ("Too many saturation options in theme " ++ filename ++ ":\n" ++
                prettyShow (Map.toList saturations))
            where
                saturations =
                    colors <&> (\c -> (roundIn 0.001 (colorSat c), [c]))
                    & Map.fromListWith (++)
                colors = theme ^.. template
        verifyRetroColor col@(Color r g b a)
            | all (`elem` [0, 0.5, 1.0]) [r, g, b]
                && elem a [0, 0.05, 0.1, 0.5, 1.0] = pure ()
            | otherwise =
                assertString ("Bad retro color in theme " ++ filename ++ ": " ++ show col)

colorSat :: Color -> Double
colorSat = fst . colorSV

colorSV :: Color -> (Double, Double)
colorSV (Color r g b _a) =
    (if v == 0 then 0 else (v - m) / v, v)
    where
        v = maximum [r, g, b]
        m = minimum [r, g, b]

roundIn :: RealFrac a => a -> a -> a
roundIn unit x = fromIntegral (round (x / unit) :: Integer) * unit
