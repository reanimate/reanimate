{-# LANGUAGE OverloadedStrings #-}
{-|
  A colormap takes a number between 0 and 1 (inclusive) and spits out a color.
  The colors do not have an alpha component but one can be added with
  `Codec.Picture.Types.promotePixel`.
-}
module Reanimate.ColorMap
  ( turbo
  , viridis
  , magma
  , inferno
  , plasma
  , sinebow
  , parula
  , cividis
  , jet
  , hsv
  , hsvMatlab
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
--
--   <<docs/gifs/doc_turbo.gif>>
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
--
--   <<docs/gifs/doc_viridis.gif>>
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
--
--   <<docs/gifs/doc_magma.gif>>
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
--
--   <<docs/gifs/doc_inferno.gif>>
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
--
--   <<docs/gifs/doc_plasma.gif>>
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
--
--   <<docs/gifs/doc_sinebow.gif>>
sinebow :: Double -> PixelRGB8
sinebow t = PixelRGB8 r g b
  where
    pi_1_3 = pi / 3
    pi_2_3 = pi * 2 / 3
    x = (0.5 - t) * pi
    r = round $ 255 * sin x**2
    g = round $ 255 * sin (x+pi_1_3)**2
    b = round $ 255 * sin (x+pi_2_3)**2

-- | Given a number t in the range [0,1], returns the corresponding color from
--   the “cividis” color vision deficiency-optimized color scheme designed by
--   Nuñez, Anderton, and Renslow, represented as an RGB string.
--
--   <<docs/gifs/doc_cividis.gif>>
cividis :: Double -> PixelRGB8
cividis t = PixelRGB8 red green blue
  where
    red   = trunc $ round(-4.54 - t * (35.34 - t * (2381.73 - t * (6402.7 - t * (7024.72 - t * 2710.57)))))
    green = trunc $ round(32.49 + t * (170.73 + t * (52.82 - t * (131.46 - t * (176.58 - t * 67.37)))))
    blue  = trunc $ round(81.24 + t * (442.36 - t * (2482.43 - t * (6167.24 - t * (6614.94 - t * 2475.67)))))
    trunc :: Integer -> Pixel8
    trunc = fromIntegral . min 255 . max 0

-- | Jet colormap. Used to be the default in matlab. Obsolete.
--
--   <<docs/gifs/doc_jet.gif>>
jet :: Double -> PixelRGB8
jet t = PixelRGB8 red green blue
  where
    red   = trunc $ min (4*t - 1.5) (-4*t + 4.5)
    green = trunc $ min (4*t - 0.5) (-4*t + 3.5)
    blue  = trunc $ min (4*t + 0.5) (-4*t + 2.5)
    trunc :: Double -> Pixel8
    trunc = round . min 255 . max 0 . (*) 255

-- | hsv colormap. Goes from 0 degrees to 360 degrees.
--
--   <<docs/gifs/doc_hsv.gif>>
hsv :: Double -> PixelRGB8
hsv t = PixelRGB8 (round $ r*255) (round $ g*255) (round $ b*255)
  where
    RGB r g b = HSV.hsv (t * 360) 1 1

-- | Matlab hsv colormap. Goes from 0 degrees to 330 degrees.
--
--   <<docs/gifs/doc_hsvMatlab.gif>>
hsvMatlab :: Double -> PixelRGB8
hsvMatlab t = PixelRGB8 (round $ r*255) (round $ g*255) (round $ b*255)
  where
    RGB r g b = HSV.hsv (t * 330) 1 1

-- | Greyscale colormap.
--
--   <<docs/gifs/doc_greyscale.gif>>
greyscale :: Double -> PixelRGB8
greyscale t = PixelRGB8 v v v
  where
    v = round $ t * 255

-- | Parula is the default colormap for matlab.
--
--   <<docs/gifs/doc_parula.gif>>
parula :: Double -> PixelRGB8
parula = ramp vec
  where
    vec = V.fromList $ pixels colorList
    pixels [] = []
    pixels (r:g:b:xs) =
      PixelRGB8 (round $ r*255) (round $ g*255) (round $ b*255) :
      pixels xs
    pixels _ = error "Reanimate.ColorMap.parula: Broken data"
    colorList :: [Double]
    colorList =
       [0.2081, 0.1663, 0.5292
       ,0.2091, 0.1721, 0.5411
       ,0.2101, 0.1779, 0.5530
       ,0.2109, 0.1837, 0.5650
       ,0.2116, 0.1895, 0.5771
       ,0.2121, 0.1954, 0.5892
       ,0.2124, 0.2013, 0.6013
       ,0.2125, 0.2072, 0.6135
       ,0.2123, 0.2132, 0.6258
       ,0.2118, 0.2192, 0.6381
       ,0.2111, 0.2253, 0.6505
       ,0.2099, 0.2315, 0.6629
       ,0.2084, 0.2377, 0.6753
       ,0.2063, 0.2440, 0.6878
       ,0.2038, 0.2503, 0.7003
       ,0.2006, 0.2568, 0.7129
       ,0.1968, 0.2632, 0.7255
       ,0.1921, 0.2698, 0.7381
       ,0.1867, 0.2764, 0.7507
       ,0.1802, 0.2832, 0.7634
       ,0.1728, 0.2902, 0.7762
       ,0.1641, 0.2975, 0.7890
       ,0.1541, 0.3052, 0.8017
       ,0.1427, 0.3132, 0.8145
       ,0.1295, 0.3217, 0.8269
       ,0.1147, 0.3306, 0.8387
       ,0.0986, 0.3397, 0.8495
       ,0.0816, 0.3486, 0.8588
       ,0.0646, 0.3572, 0.8664
       ,0.0482, 0.3651, 0.8722
       ,0.0329, 0.3724, 0.8765
       ,0.0213, 0.3792, 0.8796
       ,0.0136, 0.3853, 0.8815
       ,0.0086, 0.3911, 0.8827
       ,0.0060, 0.3965, 0.8833
       ,0.0051, 0.4017, 0.8834
       ,0.0054, 0.4066, 0.8831
       ,0.0067, 0.4113, 0.8825
       ,0.0089, 0.4159, 0.8816
       ,0.0116, 0.4203, 0.8805
       ,0.0148, 0.4246, 0.8793
       ,0.0184, 0.4288, 0.8779
       ,0.0223, 0.4329, 0.8763
       ,0.0264, 0.4370, 0.8747
       ,0.0306, 0.4410, 0.8729
       ,0.0349, 0.4449, 0.8711
       ,0.0394, 0.4488, 0.8692
       ,0.0437, 0.4526, 0.8672
       ,0.0477, 0.4564, 0.8652
       ,0.0514, 0.4602, 0.8632
       ,0.0549, 0.4640, 0.8611
       ,0.0582, 0.4677, 0.8589
       ,0.0612, 0.4714, 0.8568
       ,0.0640, 0.4751, 0.8546
       ,0.0666, 0.4788, 0.8525
       ,0.0689, 0.4825, 0.8503
       ,0.0710, 0.4862, 0.8481
       ,0.0729, 0.4899, 0.8460
       ,0.0746, 0.4937, 0.8439
       ,0.0761, 0.4974, 0.8418
       ,0.0773, 0.5012, 0.8398
       ,0.0782, 0.5051, 0.8378
       ,0.0789, 0.5089, 0.8359
       ,0.0794, 0.5129, 0.8341
       ,0.0795, 0.5169, 0.8324
       ,0.0793, 0.5210, 0.8308
       ,0.0788, 0.5251, 0.8293
       ,0.0778, 0.5295, 0.8280
       ,0.0764, 0.5339, 0.8270
       ,0.0746, 0.5384, 0.8261
       ,0.0724, 0.5431, 0.8253
       ,0.0698, 0.5479, 0.8247
       ,0.0668, 0.5527, 0.8243
       ,0.0636, 0.5577, 0.8239
       ,0.0600, 0.5627, 0.8237
       ,0.0562, 0.5677, 0.8234
       ,0.0523, 0.5727, 0.8231
       ,0.0484, 0.5777, 0.8228
       ,0.0445, 0.5826, 0.8223
       ,0.0408, 0.5874, 0.8217
       ,0.0372, 0.5922, 0.8209
       ,0.0342, 0.5968, 0.8198
       ,0.0317, 0.6012, 0.8186
       ,0.0296, 0.6055, 0.8171
       ,0.0279, 0.6097, 0.8154
       ,0.0265, 0.6137, 0.8135
       ,0.0255, 0.6176, 0.8114
       ,0.0248, 0.6214, 0.8091
       ,0.0243, 0.6250, 0.8066
       ,0.0239, 0.6285, 0.8039
       ,0.0237, 0.6319, 0.8010
       ,0.0235, 0.6352, 0.7980
       ,0.0233, 0.6384, 0.7948
       ,0.0231, 0.6415, 0.7916
       ,0.0230, 0.6445, 0.7881
       ,0.0229, 0.6474, 0.7846
       ,0.0227, 0.6503, 0.7810
       ,0.0227, 0.6531, 0.7773
       ,0.0232, 0.6558, 0.7735
       ,0.0238, 0.6585, 0.7696
       ,0.0246, 0.6611, 0.7656
       ,0.0263, 0.6637, 0.7615
       ,0.0282, 0.6663, 0.7574
       ,0.0306, 0.6688, 0.7532
       ,0.0338, 0.6712, 0.7490
       ,0.0373, 0.6737, 0.7446
       ,0.0418, 0.6761, 0.7402
       ,0.0467, 0.6784, 0.7358
       ,0.0516, 0.6808, 0.7313
       ,0.0574, 0.6831, 0.7267
       ,0.0629, 0.6854, 0.7221
       ,0.0692, 0.6877, 0.7173
       ,0.0755, 0.6899, 0.7126
       ,0.0820, 0.6921, 0.7078
       ,0.0889, 0.6943, 0.7029
       ,0.0956, 0.6965, 0.6979
       ,0.1031, 0.6986, 0.6929
       ,0.1104, 0.7007, 0.6878
       ,0.1180, 0.7028, 0.6827
       ,0.1258, 0.7049, 0.6775
       ,0.1335, 0.7069, 0.6723
       ,0.1418, 0.7089, 0.6669
       ,0.1499, 0.7109, 0.6616
       ,0.1585, 0.7129, 0.6561
       ,0.1671, 0.7148, 0.6507
       ,0.1758, 0.7168, 0.6451
       ,0.1849, 0.7186, 0.6395
       ,0.1938, 0.7205, 0.6338
       ,0.2033, 0.7223, 0.6281
       ,0.2128, 0.7241, 0.6223
       ,0.2224, 0.7259, 0.6165
       ,0.2324, 0.7275, 0.6107
       ,0.2423, 0.7292, 0.6048
       ,0.2527, 0.7308, 0.5988
       ,0.2631, 0.7324, 0.5929
       ,0.2735, 0.7339, 0.5869
       ,0.2845, 0.7354, 0.5809
       ,0.2953, 0.7368, 0.5749
       ,0.3064, 0.7381, 0.5689
       ,0.3177, 0.7394, 0.5630
       ,0.3289, 0.7406, 0.5570
       ,0.3405, 0.7417, 0.5512
       ,0.3520, 0.7428, 0.5453
       ,0.3635, 0.7438, 0.5396
       ,0.3753, 0.7446, 0.5339
       ,0.3869, 0.7454, 0.5283
       ,0.3986, 0.7461, 0.5229
       ,0.4103, 0.7467, 0.5175
       ,0.4218, 0.7473, 0.5123
       ,0.4334, 0.7477, 0.5072
       ,0.4447, 0.7482, 0.5021
       ,0.4561, 0.7485, 0.4972
       ,0.4672, 0.7487, 0.4924
       ,0.4783, 0.7489, 0.4877
       ,0.4892, 0.7491, 0.4831
       ,0.5000, 0.7491, 0.4786
       ,0.5106, 0.7492, 0.4741
       ,0.5212, 0.7492, 0.4698
       ,0.5315, 0.7491, 0.4655
       ,0.5418, 0.7490, 0.4613
       ,0.5519, 0.7489, 0.4571
       ,0.5619, 0.7487, 0.4531
       ,0.5718, 0.7485, 0.4490
       ,0.5816, 0.7482, 0.4451
       ,0.5913, 0.7479, 0.4412
       ,0.6009, 0.7476, 0.4374
       ,0.6103, 0.7473, 0.4335
       ,0.6197, 0.7469, 0.4298
       ,0.6290, 0.7465, 0.4261
       ,0.6382, 0.7460, 0.4224
       ,0.6473, 0.7456, 0.4188
       ,0.6564, 0.7451, 0.4152
       ,0.6653, 0.7446, 0.4116
       ,0.6742, 0.7441, 0.4081
       ,0.6830, 0.7435, 0.4046
       ,0.6918, 0.7430, 0.4011
       ,0.7004, 0.7424, 0.3976
       ,0.7091, 0.7418, 0.3942
       ,0.7176, 0.7412, 0.3908
       ,0.7261, 0.7405, 0.3874
       ,0.7346, 0.7399, 0.3840
       ,0.7430, 0.7392, 0.3806
       ,0.7513, 0.7385, 0.3773
       ,0.7596, 0.7378, 0.3739
       ,0.7679, 0.7372, 0.3706
       ,0.7761, 0.7364, 0.3673
       ,0.7843, 0.7357, 0.3639
       ,0.7924, 0.7350, 0.3606
       ,0.8005, 0.7343, 0.3573
       ,0.8085, 0.7336, 0.3539
       ,0.8166, 0.7329, 0.3506
       ,0.8246, 0.7322, 0.3472
       ,0.8325, 0.7315, 0.3438
       ,0.8405, 0.7308, 0.3404
       ,0.8484, 0.7301, 0.3370
       ,0.8563, 0.7294, 0.3336
       ,0.8642, 0.7288, 0.3300
       ,0.8720, 0.7282, 0.3265
       ,0.8798, 0.7276, 0.3229
       ,0.8877, 0.7271, 0.3193
       ,0.8954, 0.7266, 0.3156
       ,0.9032, 0.7262, 0.3117
       ,0.9110, 0.7259, 0.3078
       ,0.9187, 0.7256, 0.3038
       ,0.9264, 0.7256, 0.2996
       ,0.9341, 0.7256, 0.2953
       ,0.9417, 0.7259, 0.2907
       ,0.9493, 0.7264, 0.2859
       ,0.9567, 0.7273, 0.2808
       ,0.9639, 0.7285, 0.2754
       ,0.9708, 0.7303, 0.2696
       ,0.9773, 0.7326, 0.2634
       ,0.9831, 0.7355, 0.2570
       ,0.9882, 0.7390, 0.2504
       ,0.9922, 0.7431, 0.2437
       ,0.9952, 0.7476, 0.2373
       ,0.9973, 0.7524, 0.2310
       ,0.9986, 0.7573, 0.2251
       ,0.9991, 0.7624, 0.2195
       ,0.9990, 0.7675, 0.2141
       ,0.9985, 0.7726, 0.2090
       ,0.9976, 0.7778, 0.2042
       ,0.9964, 0.7829, 0.1995
       ,0.9950, 0.7880, 0.1949
       ,0.9933, 0.7931, 0.1905
       ,0.9914, 0.7981, 0.1863
       ,0.9894, 0.8032, 0.1821
       ,0.9873, 0.8083, 0.1780
       ,0.9851, 0.8133, 0.1740
       ,0.9828, 0.8184, 0.1700
       ,0.9805, 0.8235, 0.1661
       ,0.9782, 0.8286, 0.1622
       ,0.9759, 0.8337, 0.1583
       ,0.9736, 0.8389, 0.1544
       ,0.9713, 0.8441, 0.1505
       ,0.9692, 0.8494, 0.1465
       ,0.9672, 0.8548, 0.1425
       ,0.9654, 0.8603, 0.1385
       ,0.9638, 0.8659, 0.1343
       ,0.9623, 0.8716, 0.1301
       ,0.9611, 0.8774, 0.1258
       ,0.9600, 0.8834, 0.1215
       ,0.9593, 0.8895, 0.1171
       ,0.9588, 0.8958, 0.1126
       ,0.9586, 0.9022, 0.1082
       ,0.9587, 0.9088, 0.1036
       ,0.9591, 0.9155, 0.0990
       ,0.9599, 0.9225, 0.0944
       ,0.9610, 0.9296, 0.0897
       ,0.9624, 0.9368, 0.0850
       ,0.9641, 0.9443, 0.0802
       ,0.9662, 0.9518, 0.0753
       ,0.9685, 0.9595, 0.0703
       ,0.9710, 0.9673, 0.0651
       ,0.9736, 0.9752, 0.0597
       ,0.9763, 0.9831, 0.0538]

--------------------------------------------------------------------------------
-- Helpers

colors :: Text -> Vector PixelRGB8
colors = V.fromList . map (toColor . map (fromIntegral . digitToInt) . T.unpack) . T.chunksOf 6
  where
    toColor [r1,r2,g1,g2,b1,b2] =
      PixelRGB8 (r1 `shiftL` 4 + r2) (g1 `shiftL` 4 + g2) (b1 `shiftL` 4+b2)
    toColor _ = error "Reanimate.ColorMap.colors: Broken data"

ramp :: Vector PixelRGB8 -> Double -> PixelRGB8
ramp v = \t -> v V.! max 0 (min (len-1) $ round $ t * (len'-1))
  where
    len = V.length v
    len' = fromIntegral len
