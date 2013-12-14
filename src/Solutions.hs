module Solutions where

import qualified Data.Map as M

import Problem1 (solution1)
import Problem2 (solution2)
import Problem3 (solution3)
import Problem4 (solution4)
import Problem5 (solution5)
import Problem6 (solution6)
import Problem7 (solution7)
import Problem8 (solution8)
import Problem9 (solution9)
import Problem10 (solution10)
import Problem11 (solution11)
import Problem12 (solution12)
import Problem13 (solution13)
import Problem14 (solution14)
import Problem15 (solution15)
import Problem16 (solution16)
import Problem17 (solution17)
import Problem18 (solution18)
import Problem19 (solution19)
import Problem20 (solution20)
import Problem21 (solution21)
import Problem22 (solution22)
import Problem23 (solution23)
import Problem24 (solution24)
import Problem25 (solution25)
import Problem26 (solution26)
import Problem27 (solution27)
import Problem28 (solution28)
import Problem29 (solution29)
import Problem30 (solution30)
import Problem31 (solution31)
import Problem32 (solution32)
import Problem33 (solution33)
import Problem34 (solution34)
import Problem35 (solution35)
import Problem36 (solution36)
import Problem37 (solution37)
import Problem38 (solution38)
import Problem39 (solution39)
import Problem40 (solution40)
import Problem41 (solution41)
import Problem42 (solution42)
import Problem43 (solution43)
import Problem44 (solution44)
import Problem45 (solution45)
import Problem46 (solution46)
import Problem47 (solution47)
import Problem48 (solution48)
import Problem49 (solution49)
import Problem50 (solution50)
import Problem51 (solution51)
import Problem52 (solution52)
import Problem53 (solution53)
import Problem54 (solution54)
import Problem55 (solution55)
import Problem56 (solution56)
import Problem57 (solution57)
import Problem58 (solution58)
import Problem59 (solution59)
import Problem60 (solution60)
import Problem61 (solution61)
import Problem62 (solution62)
import Problem63 (solution63)
import Problem65 (solution65)
import Problem67 (solution67)
import Problem69 (solution69)
import Problem70 (solution70)
import Problem71 (solution71)
import Problem72 (solution72)
import Problem73 (solution73)
import Problem74 (solution74)
import Problem75 (solution75)
import Problem76 (solution76)
import Problem77 (solution77)
import Problem78 (solution78)
import Problem80 (solution80)
import Problem87 (solution87)
import Problem89 (solution89)
import Problem92 (solution92)
import Problem97 (solution97)
import Problem99 (solution99)
import Problem102 (solution102)
import Problem104 (solution104)
import Problem112 (solution112)
import Problem124 (solution124)
import Problem145 (solution145)
import Problem179 (solution179)
import Problem187 (solution187)
import Problem200 (solution200)
import Problem203 (solution203)
import Problem204 (solution204)
import Problem206 (solution206)
import Problem303 (solution303)
import Problem357 (solution357)
import Problem381 (solution381)


solutions :: M.Map String (IO ())
solutions = M.fromList [("1", solution1),
                        ("2", solution2),
                        ("3", solution3),
                        ("4", solution4),
                        ("5", solution5),
                        ("6", solution6),
                        ("7", solution7),
                        ("8", solution8),
                        ("9", solution9),
                        ("10", solution10),
                        ("11", solution11),
                        ("12", solution12),
                        ("13", solution13),
                        ("14", solution14),
                        ("15", solution15),
                        ("16", solution16),
                        ("17", solution17),
                        ("18", solution18),
                        ("19", solution19),
                        ("20", solution20),
                        ("21", solution21),
                        ("22", solution22),
                        ("23", solution23),
                        ("24", solution24),
                        ("25", solution25),
                        ("26", solution26),
                        ("27", solution27),
                        ("28", solution28),
                        ("29", solution29),
                        ("30", solution30),
                        ("31", solution31),
                        ("32", solution32),
                        ("33", solution33),
                        ("34", solution34),
                        ("35", solution35),
                        ("36", solution36),
                        ("37", solution37),
                        ("38", solution38),
                        ("39", solution39),
                        ("40", solution40),
                        ("41", solution41),
                        ("42", solution42),
                        ("43", solution43),
                        ("44", solution44),
                        ("45", solution45),
                        ("46", solution46),
                        ("47", solution47),
                        ("48", solution48),
                        ("49", solution49),
                        ("50", solution50),
                        ("51", solution51),
                        ("52", solution52),
                        ("53", solution53),
                        ("54", solution54),
                        ("55", solution55),
                        ("56", solution56),
                        ("57", solution57),
                        ("58", solution58),
                        ("59", solution59),
                        ("60", solution60),
                        ("61", solution61),
                        ("62", solution62),
                        ("63", solution63),
                        ("65", solution65),
                        ("67", solution67),
                        ("69", solution69),
                        ("70", solution70),
                        ("71", solution71),
                        ("72", solution72),
                        ("73", solution73),
                        ("74", solution74),
                        ("75", solution75),
                        ("76", solution76),
                        ("77", solution77),
                        ("78", solution78),
                        ("80", solution80),
                        ("87", solution87),
                        ("89", solution89),
                        ("92", solution92),
                        ("97", solution97),
                        ("99", solution99),
                        ("102", solution102),
                        ("104", solution104),
                        ("112", solution112),
                        ("124", solution124),
                        ("145", solution145),
                        ("179", solution179),
                        ("187", solution187),
                        ("200", solution200),
                        ("203", solution203),
                        ("204", solution204),
                        ("206", solution206),
                        ("303", solution303),
                        ("357", solution357),
                        ("381", solution381)
                       ]


runSolution :: String -> IO ()
runSolution problem = do
    putStrLn $ "Running solution for problem #" ++ problem
    case M.lookup problem solutions of
        Just solution -> solution
        Nothing -> putStrLn "No solution for that problem"
