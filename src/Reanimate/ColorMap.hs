{-# LANGUAGE OverloadedStrings #-}
module Reanimate.ColorMap
  ( turbo
  , viridis
  , magma
  , inferno
  , plasma
  , sinebow
  , cividis
  , jet
  , hsv
  , greyscale
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Vector as V
import Codec.Picture
import Data.Char
import Data.Bits
import qualified Data.Colour.RGBSpace.HSV as HSV
import           Data.Colour.RGBSpace

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “turbo” color scheme by Anton Mikhailov.
turbo :: Double -> PixelRGB8
turbo t = PixelRGB8 red green blue
  where
    red   = trunc (round (34.61 + t * (1172.33 - t * (10793.56 - t * (33300.12 - t * (38394.49 - t * 14825.05))))))
    green = trunc (round (23.31 + t * (557.33 + t * (1225.33 - t * (3574.96 - t * (1073.77 + t * 707.56))))))
    blue  = trunc (round (27.2 + t * (3211.1 - t * (15327.97 - t * (27814 - t * (22569.18 - t * 6838.66))))))
    trunc :: Integer -> Pixel8
    trunc = fromIntegral . min 255 . max 0

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “viridis” perceptually-uniform color scheme designed by van der Walt,
--   Smith and Firing for matplotlib, represented as an RGB string.
viridis :: Double -> PixelRGB8
viridis = ramp (colors
  "44015444025645045745055946075a46085c460a5d460b5e470d60470e614710634711644713\
  \6548146748166848176948186a481a6c481b6d481c6e481d6f481f7048207148217348237448\
  \2475482576482677482878482979472a7a472c7a472d7b472e7c472f7d46307e46327e46337f\
  \463480453581453781453882443983443a83443b84433d84433e85423f854240864241864142\
  \874144874045884046883f47883f48893e49893e4a893e4c8a3d4d8a3d4e8a3c4f8a3c508b3b\
  \518b3b528b3a538b3a548c39558c39568c38588c38598c375a8c375b8d365c8d365d8d355e8d\
  \355f8d34608d34618d33628d33638d32648e32658e31668e31678e31688e30698e306a8e2f6b\
  \8e2f6c8e2e6d8e2e6e8e2e6f8e2d708e2d718e2c718e2c728e2c738e2b748e2b758e2a768e2a\
  \778e2a788e29798e297a8e297b8e287c8e287d8e277e8e277f8e27808e26818e26828e26828e\
  \25838e25848e25858e24868e24878e23888e23898e238a8d228b8d228c8d228d8d218e8d218f\
  \8d21908d21918c20928c20928c20938c1f948c1f958b1f968b1f978b1f988b1f998a1f9a8a1e\
  \9b8a1e9c891e9d891f9e891f9f881fa0881fa1881fa1871fa28720a38620a48621a58521a685\
  \22a78522a88423a98324aa8325ab8225ac8226ad8127ad8128ae8029af7f2ab07f2cb17e2db2\
  \7d2eb37c2fb47c31b57b32b67a34b67935b77937b87838b9773aba763bbb753dbc743fbc7340\
  \bd7242be7144bf7046c06f48c16e4ac16d4cc26c4ec36b50c46a52c56954c56856c66758c765\
  \5ac8645cc8635ec96260ca6063cb5f65cb5e67cc5c69cd5b6ccd5a6ece5870cf5773d05675d0\
  \5477d1537ad1517cd2507fd34e81d34d84d44b86d54989d5488bd6468ed64590d74393d74195\
  \d84098d83e9bd93c9dd93ba0da39a2da37a5db36a8db34aadc32addc30b0dd2fb2dd2db5de2b\
  \b8de29bade28bddf26c0df25c2df23c5e021c8e020cae11fcde11dd0e11cd2e21bd5e21ad8e2\
  \19dae319dde318dfe318e2e418e5e419e7e419eae51aece51befe51cf1e51df4e61ef6e620f8\
  \e621fbe723fde725")

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “magma” perceptually-uniform color scheme designed by van der Walt and
--   Smith for matplotlib, represented as an RGB string.
magma :: Double -> PixelRGB8
magma = ramp (colors
  "00000401000501010601010802010902020b02020d03030f0303120404140504160605180605\
  \1a07061c08071e0907200a08220b09240c09260d0a290e0b2b100b2d110c2f120d31130d3414\
  \0e36150e38160f3b180f3d19103f1a10421c10441d11471e114920114b21114e221150241253\
  \25125527125829115a2a115c2c115f2d11612f116331116533106734106936106b38106c390f\
  \6e3b0f703d0f713f0f72400f74420f75440f764510774710784910784a10794c117a4e117b4f\
  \127b51127c52137c54137d56147d57157e59157e5a167e5c167f5d177f5f187f601880621980\
  \641a80651a80671b80681c816a1c816b1d816d1d816e1e81701f81721f817320817521817621\
  \817822817922827b23827c23827e24828025828125818326818426818627818827818928818b\
  \29818c29818e2a81902a81912b81932b80942c80962c80982d80992d809b2e7f9c2e7f9e2f7f\
  \a02f7fa1307ea3307ea5317ea6317da8327daa337dab337cad347cae347bb0357bb2357bb336\
  \7ab5367ab73779b83779ba3878bc3978bd3977bf3a77c03a76c23b75c43c75c53c74c73d73c8\
  \3e73ca3e72cc3f71cd4071cf4070d0416fd2426fd3436ed5446dd6456cd8456cd9466bdb476a\
  \dc4869de4968df4a68e04c67e24d66e34e65e44f64e55064e75263e85362e95462ea5661eb57\
  \60ec5860ed5a5fee5b5eef5d5ef05f5ef1605df2625df2645cf3655cf4675cf4695cf56b5cf6\
  \6c5cf66e5cf7705cf7725cf8745cf8765cf9785df9795df97b5dfa7d5efa7f5efa815ffb835f\
  \fb8560fb8761fc8961fc8a62fc8c63fc8e64fc9065fd9266fd9467fd9668fd9869fd9a6afd9b\
  \6bfe9d6cfe9f6dfea16efea36ffea571fea772fea973feaa74feac76feae77feb078feb27afe\
  \b47bfeb67cfeb77efeb97ffebb81febd82febf84fec185fec287fec488fec68afec88cfeca8d\
  \fecc8ffecd90fecf92fed194fed395fed597fed799fed89afdda9cfddc9efddea0fde0a1fde2\
  \a3fde3a5fde5a7fde7a9fde9aafdebacfcecaefceeb0fcf0b2fcf2b4fcf4b6fcf6b8fcf7b9fc\
  \f9bbfcfbbdfcfdbf")

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “inferno” perceptually-uniform color scheme designed by van der Walt
--   and Smith for matplotlib, represented as an RGB string.
inferno :: Double -> PixelRGB8
inferno = ramp (colors
  "00000401000501010601010802010a02020c02020e0302100403120403140504170604190705\
  \1b08051d09061f0a07220b07240c08260d08290e092b10092d110a30120a32140b34150b3716\
  \0b39180c3c190c3e1b0c411c0c431e0c451f0c48210c4a230c4c240c4f260c51280b53290b55\
  \2b0b572d0b592f0a5b310a5c320a5e340a5f3609613809623909633b09643d09653e0966400a\
  \67420a68440a68450a69470b6a490b6a4a0c6b4c0c6b4d0d6c4f0d6c510e6c520e6d540f6d55\
  \0f6d57106e59106e5a116e5c126e5d126e5f136e61136e62146e64156e65156e67166e69166e\
  \6a176e6c186e6d186e6f196e71196e721a6e741a6e751b6e771c6d781c6d7a1d6d7c1d6d7d1e\
  \6d7f1e6c801f6c82206c84206b85216b87216b88226a8a226a8c23698d23698f246990256892\
  \25689326679526679727669827669a28659b29649d29649f2a63a02a63a22b62a32c61a52c60\
  \a62d60a82e5fa92e5eab2f5ead305dae305cb0315bb1325ab3325ab43359b63458b73557b935\
  \56ba3655bc3754bd3853bf3952c03a51c13a50c33b4fc43c4ec63d4dc73e4cc83f4bca404acb\
  \4149cc4248ce4347cf4446d04545d24644d34743d44842d54a41d74b3fd84c3ed94d3dda4e3c\
  \db503bdd513ade5238df5337e05536e15635e25734e35933e45a31e55c30e65d2fe75e2ee860\
  \2de9612bea632aeb6429eb6628ec6726ed6925ee6a24ef6c23ef6e21f06f20f1711ff1731df2\
  \741cf3761bf37819f47918f57b17f57d15f67e14f68013f78212f78410f8850ff8870ef8890c\
  \f98b0bf98c0af98e09fa9008fa9207fa9407fb9606fb9706fb9906fb9b06fb9d07fc9f07fca1\
  \08fca309fca50afca60cfca80dfcaa0ffcac11fcae12fcb014fcb216fcb418fbb61afbb81dfb\
  \ba1ffbbc21fbbe23fac026fac228fac42afac62df9c72ff9c932f9cb35f8cd37f8cf3af7d13d\
  \f7d340f6d543f6d746f5d949f5db4cf4dd4ff4df53f4e156f3e35af3e55df2e661f2e865f2ea\
  \69f1ec6df1ed71f1ef75f1f179f2f27df2f482f3f586f3f68af4f88ef5f992f6fa96f8fb9af9\
  \fc9dfafda1fcffa4")

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “plasma” perceptually-uniform color scheme designed by van der Walt and
--   Smith for matplotlib, represented as an RGB string.
plasma :: Double -> PixelRGB8
plasma = ramp (colors
  "0d088710078813078916078a19068c1b068d1d068e20068f2206902406912605912805922a05\
  \932c05942e05952f059631059733059735049837049938049a3a049a3c049b3e049c3f049c41\
  \049d43039e44039e46039f48039f4903a04b03a14c02a14e02a25002a25102a35302a35502a4\
  \5601a45801a45901a55b01a55c01a65e01a66001a66100a76300a76400a76600a76700a86900\
  \a86a00a86c00a86e00a86f00a87100a87201a87401a87501a87701a87801a87a02a87b02a87d\
  \03a87e03a88004a88104a78305a78405a78606a68707a68808a68a09a58b0aa58d0ba58e0ca4\
  \8f0da4910ea3920fa39410a29511a19613a19814a099159f9a169f9c179e9d189d9e199da01a\
  \9ca11b9ba21d9aa31e9aa51f99a62098a72197a82296aa2395ab2494ac2694ad2793ae2892b0\
  \2991b12a90b22b8fb32c8eb42e8db52f8cb6308bb7318ab83289ba3388bb3488bc3587bd3786\
  \be3885bf3984c03a83c13b82c23c81c33d80c43e7fc5407ec6417dc7427cc8437bc9447aca45\
  \7acb4679cc4778cc4977cd4a76ce4b75cf4c74d04d73d14e72d24f71d35171d45270d5536fd5\
  \546ed6556dd7566cd8576bd9586ada5a6ada5b69db5c68dc5d67dd5e66de5f65de6164df6263\
  \e06363e16462e26561e26660e3685fe4695ee56a5de56b5de66c5ce76e5be76f5ae87059e971\
  \58e97257ea7457eb7556eb7655ec7754ed7953ed7a52ee7b51ef7c51ef7e50f07f4ff0804ef1\
  \814df1834cf2844bf3854bf3874af48849f48948f58b47f58c46f68d45f68f44f79044f79143\
  \f79342f89441f89540f9973ff9983ef99a3efa9b3dfa9c3cfa9e3bfb9f3afba139fba238fca3\
  \38fca537fca636fca835fca934fdab33fdac33fdae32fdaf31fdb130fdb22ffdb42ffdb52efe\
  \b72dfeb82cfeba2cfebb2bfebd2afebe2afec029fdc229fdc328fdc527fdc627fdc827fdca26\
  \fdcb26fccd25fcce25fcd025fcd225fbd324fbd524fbd724fad824fada24f9dc24f9dd25f8df\
  \25f8e125f7e225f7e425f6e626f6e826f5e926f5eb27f4ed27f3ee27f3f027f2f227f1f426f1\
  \f525f0f724f0f921")

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “sinebow” color scheme by Jim Bumgardner and Charlie Loyd.
sinebow :: Double -> PixelRGB8
sinebow t = PixelRGB8 r g b
  where
    pi_1_3 = pi / 3
    pi_2_3 = pi * 2 / 3
    x = (0.5 - t) * pi
    r = round $ 255 * sin(x)^2
    g = round $ 255 * sin(x+pi_1_3)^2
    b = round $ 255 * sin(x+pi_2_3)^2

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “cividis” color vision deficiency-optimized color scheme designed by
--   Nuñez, Anderton, and Renslow, represented as an RGB string.
cividis :: Double -> PixelRGB8
cividis t = PixelRGB8 red green blue
  where
    red   = trunc $ round(-4.54 - t * (35.34 - t * (2381.73 - t * (6402.7 - t * (7024.72 - t * 2710.57)))))
    green = trunc $ round(32.49 + t * (170.73 + t * (52.82 - t * (131.46 - t * (176.58 - t * 67.37)))))
    blue  = trunc $ round(81.24 + t * (442.36 - t * (2482.43 - t * (6167.24 - t * (6614.94 - t * 2475.67)))))
    trunc :: Integer -> Pixel8
    trunc = fromIntegral . min 255 . max 0

-- | Jet colormap. Used to be the default in matlab. Obsolete.
jet :: Double -> PixelRGB8
jet t = PixelRGB8 red green blue
  where
    red   = trunc $ min (4*t - 1.5) (-4*t + 4.5)
    green = trunc $ min (4*t - 0.5) (-4*t + 3.5)
    blue  = trunc $ min (4*t + 0.5) (-4*t + 2.5)
    trunc :: Double -> Pixel8
    trunc = fromIntegral . min 255 . max 0 . round . (*) 255

-- | hsv colormap. Goes from 0 degrees to 360 degrees.
hsv :: Double -> PixelRGB8
hsv t = PixelRGB8 (round $ r*255) (round $ g*255) (round $ b*255)
  where
    RGB r g b = HSV.hsv (t * 360) 1 1

-- | Matlab hsv colormap. Goes from 0 degrees to 330 degrees.
hsvMatlab :: Double -> PixelRGB8
hsvMatlab t = PixelRGB8 (round $ r*255) (round $ g*255) (round $ b*255)
  where
    RGB r g b = HSV.hsv (t * 330) 1 1

greyscale :: Double -> PixelRGB8
greyscale t = PixelRGB8 v v v
  where
    v = round $ t * 255

--------------------------------------------------------------------------------
-- Helpers

colors :: Text -> Vector PixelRGB8
colors = V.fromList . map (toColor . map (fromIntegral . digitToInt) . T.unpack) . T.chunksOf 6
  where
    toColor [r1,r2,g1,g2,b1,b2] =
      PixelRGB8 (r1 `shiftL` 4 + r2) (g1 `shiftL` 4 + g2) (b1 `shiftL` 4+b2)

ramp :: Vector PixelRGB8 -> Double -> PixelRGB8
ramp v = \t -> v V.! (max 0 $ min (len-1) $ round $ t * (len'-1))
  where
    len = V.length v
    len' = fromIntegral len
