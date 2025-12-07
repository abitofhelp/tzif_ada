pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Iana_Releases
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Iana Releases value object - immutable domain data.
--
--  Responsibilities:
--    - Define Iana Releases type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    SHA256_Hex_Type
--    IANA_Release_Record
--    Release_Array
--
--  Dependencies:
--    TZif.Domain.Value_Object.Source_Info
--    Elaborate_Body
--
--  ===========================================================================

with TZif.Domain.Value_Object.Source_Info;

package TZif.Domain.Value_Object.IANA_Releases with
  Elaborate_Body
is

   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  SHA256 Digest Type
   --  ========================================================================

   subtype SHA256_Hex_Type is String (1 .. 64);  -- Hex-encoded SHA256

   --  ========================================================================
   --  IANA Release Record
   --  ========================================================================

   type IANA_Release_Record is record
      Version : String (1 .. 6);      -- e.g., "2024a " (padded with spaces)
      Zone_Count   : Natural;              -- Number of zones in release
      Tarball_Size : Natural;              -- Compressed .tar.gz size (bytes)
      SHA256       : SHA256_Hex_Type;      -- Checksum for integrity validation
   end record;

   --  ========================================================================
   --  Release Table (2015-2025)
   --  ========================================================================

   type Release_Array is array (Positive range <>) of IANA_Release_Record;

   IANA_Releases : constant Release_Array :=
     [
   --  2015 Releases (7 releases)

("2015a ", 416, 292_131,
      "c52490917d00a8e7fc9b5f0b1b65ef6ec76d612b5b20c81bf86a04147af18e4c"),

     ("2015b ", 416, 293_482,
      "556ac1a5b3a371adc1ad4e77138f78ddd7f8ddd7bc2b52545924598c7dc8ad62"),

     ("2015c ", 416, 294_353,
      "860fac5f5f57f7a2dfc1ba682dbd1f5c0f0b597f761571277b6ed6561e22365a"),

     ("2015d ", 416, 296_131,
      "8b9f5008277f09e251e97dba7813f56168d691115bda90ade4638d72f296d531"),

     ("2015e ", 416, 296_773,
      "ffc9b5d38abda8277aa479e3f75aa7668819d0977cd1a0c8ef3b09128334ba6f"),

     ("2015f ", 416, 298_815,
      "959f81b541e042ecb13c50097d264ae92ff03a57979c478dbcf24d5da242531d"),

     ("2015g ", 417, 300_363,
      "b923cdbf078491696b17bc8d069c74bce73fabc5774629da2f410c9b31576161"),

     ("2016a ", 416, 302_817,
      "5efa6b324e64ef921ef700ac3273a51895f672684a30e342f68e47871c6a8cd1"),

     ("2016b ", 419, 305_437,
      "6392091d92556a32de488ea06a055c51bc46b7d8046c8a677f0ccfe286b3dbdc"),

     ("2016c ", 419, 308_625,
      "8700d981e6f2007ac037dabb5d2b12f390e8629bbc30e564bc21cf0c069a2d48"),

     ("2016d ", 421, 312_481,
      "d9554dfba0efd76053582bd89e8c7036ef12eee14fdd506675b08a5b59f0a1b4"),

     ("2016e ", 421, 313_684,
      "ba00f899f18dc4048d7fa21f5e1fdef434496084eedc06f6caa15e5ecdb6bd81"),

     ("2016f ", 421, 313_286,
      "ed8c951008d12f1db55a11e96fc055718c6571233327d9de16a7f8475e2502b0"),

     ("2016g ", 421, 316_669,
      "3c7137b2bc47323b0de47b77786bacf81ed503d4b2c693ff8ada2fbd1281ebd1"),

     ("2016h ", 421, 318_714,
      "da1b74fc2dec2ce8b64948dafb0bfc2f923c830d421a7ae4d016226135697a64"),

     ("2016i ", 422, 320_352,
      "b6966ec982ef64fe48cebec437096b4f57f4287519ed32dde59c86d3a1853845"),

     ("2016j ", 424, 321_185,
      "f5ee4e0f115f6c2faee1c4b16193a97338cbd1b503f2cea6c5a768c82ff39dc8"),

     ("2017a ", 424, 323_983,
      "df3a5c4d0a2cf0cde0b3f35796ccf6c9acfd598b8e70f8dece5404cd7626bbd6"),

     ("2017b ", 424, 324_317,
      "f8242a522ea3496b0ce4ff4f2e75a049178da21001a08b8e666d8cbe07d18086"),

     ("2017c ", 424, 335_571,
      "d6543f92a929826318e2f44ff3a7611ce5f565a43e10250b42599d0ba4cbd90b"),

     ("2018a ", 424, 339_626,
      "d2ea23a06783603a42c1a2665392e65958b0c8a341b2f2e7342d43343d7080dc"),

     ("2018b ", 424, 340_314,
      "f87540b9050e1b0d772d19058fcb62f54baa505ecebf392ea825713c33bd9848"),

     ("2018c ", 424, 341_074,
      "2825c3e4b7ef520f24d393bcc02942f9762ffd3e7fc9b23850789ed8f22933f6"),

     ("2018d ", 424, 351_271,
      "5106eddceb5f1ae3a91dbd3960e1b8b11ba0dc08579a31cf0724a7691b10c054"),

     ("2018e ", 424, 353_953,
      "6b288e5926841a4cb490909fe822d85c36ae75538ad69baf20da9628b63b692e"),

     ("2018f ", 424, 366_046,
      "0af6a85fc4ea95832f76524f35696a61abb3992fd3f8db33e5a1f95653e043f2"),

     ("2018g ", 424, 366_408,
      "02dfde534872f6513ae4553a3388fdae579441e31b862ea99170dfc447f46a16"),

     ("2018h ", 425, 376_711,
      "b8cbcf3e46aed30ad27c40d5bed68c16cf759384d14cc6411b84460c88bdd91f"),

     ("2018i ", 425, 377_009,
      "82c45ef84ca3bc01d0a4a397ba8adeb8f7f199c6550740587c6ac5a7108c00d9"),

     ("2019a ", 425, 378_961,
      "90366ddf4aa03e37a16cd49255af77f801822310b213f195e2206ead48c59772"),

     ("2019b ", 425, 384_667,
      "05d9092c90dcf9ec4f3ccfdea80c7dcea5e882b3b105c3422da172aaa9a50c64"),

     ("2019c ", 425, 392_087,
      "79c7806dab09072308da0e3d22c37d3b245015a591891ea147d3b133b60ffc7c"),

     ("2020a ", 425, 397_245,
      "547161eca24d344e0b5f96aff6a76b454da295dc14ed4ca50c2355043fb899a2"),

     ("2020b ", 425, 400_017,
      "9b053f951d245ce89d850b96ee4711d82d833559b1fc96ba19f90bc4d745e809"),

     ("2020c ", 425, 400_801,
      "7890ac105f1aa4a5d15c5be2409580af401ee2f3fffe2a1e4748af589e194bd9"),

     ("2020d ", 425, 401_479,
      "8d813957de363387696f05af8a8889afa282ab5016a764c701a20758d39cbaf3"),

     ("2020e ", 424, 411_619,
      "0be1ba329eae29ae1b54057c3547b3e672f73b3ae7643aa87dac85122bec037e"),

     ("2020f ", 424, 411_739,
      "121131918c3ae6dc5d40f0eb87563a2be920b71a76e2392c09519a5e4a666881"),

     ("2021a ", 424, 411_892,
      "39e7d2ba08c68cbaefc8de3227aab0dec2521be8042cf56855f7dc3a9fb14e08"),

     ("2021b ", 424, 420_593,
      "53d9e6dbdb59dffe2b7bff59d140148181386c06e175fa69eaeb4cc83bc3deb7"),

     ("2021c ", 424, 421_791,
      "b4f1d1c8cb11c3500276dac862d8c7e6f88c69b1e8ee4c5e9d1daad17fbe3542"),

     ("2021d ", 424, 422_399,
      "d7c188a2b33d4a3c25ee4a9fdc68c1ff462bfdb302cf41343d84ca5942dbddf6"),

     ("2021e ", 424, 422_509,
      "07ec42b737d0d3c6be9c337f8abb5f00554a0f9cc4fcf01a703d69403b6bb2b1"),

     ("2022a ", 424, 425_833,
      "ef7fffd9f4f50f4f58328b35022a32a5a056b245c5cb3d6791dddb342f871664"),

     ("2022b ", 424, 432_594,
      "f590eaf04a395245426c2be4fae71c143aea5cebc11088b7a0a5704461df397d"),

     ("2022c ", 424, 432_721,
      "6974f4e348bf2323274b56dff9e7500247e3159eaa4b485dfa0cd66e75c14bfe"),

     ("2022d ", 422, 433_425,
      "6ecdbee27fa43dcfa49f3d4fd8bb1dfef54c90da1abcd82c9abcf2dc4f321de0"),

     ("2022e ", 422, 433_785,
      "8de4c2686dce3d1aae9030719e6814931c216a2d5e891ec3d332e6f6516aeccd"),

     ("2022f ", 419, 436_352,
      "9990d71f675d212567b931fe8aae1cab7027f89fefb8a79d808a6933a67af000"),

     ("2022g ", 419, 439_731,
      "4491db8281ae94a84d939e427bdd83dc389f26764d27d9a5c52d782c16764478"),

     ("2023a ", 418, 442_875,
      "a2a27edb7af5a384cfcefdae9defad6a7ed23f3f2cfdaf3d5394c1e8299710bc"),

     ("2023b ", 418, 443_019,
      "9b78fd264f95611fe987a95721456cd5aac9f55e6915be4a9ca2997bc7ce0e6c"),

     ("2023c ", 418, 443_902,
      "3f510b5d1b4ae9bb38e485aa302a776b317fb3637bdb6404c4adf7b6cadd965c"),

     ("2023d ", 418, 449_767,
      "dbca21970b0a8b8c0ceceec1d7b91fa903be0f6eca5ae732b5329672232a08f3"),

     ("2024a ", 418, 451_270,
      "0d0434459acbd2059a7a8da1f3304a84a86591f6ed69c6248fffa502b6edffe3"),

     ("2024b ", 417, 459_393,
      "70e754db126a8d0db3d16d6b4cb5f7ec1e04d5f261255e4558a67fe92d39e550"),

     ("2025a ", 417, 462_943,
      "4d5fcbc72c7c450ebfe0b659bd0f1c02fbf52fd7f517a9ea13fe71c21eb5f0d0"),

     ("2025b ", 418, 464_295,
      "11810413345fc7805017e27ea9fa4885fd74cd61b2911711ad038f5d28d71474")];

   --  ========================================================================
   --  Lookup Functions
   --  ========================================================================

   --  Check if a version is in the official IANA release table
   function Is_Known_Version (Version : Version_String_Type) return Boolean;

   --  Get expected zone count for a known version (returns 0 if unknown)
   function Get_Expected_Zone_Count
     (Version : Version_String_Type) return Natural;

   --  Get tarball SHA256 for a known version (returns empty string if unknown)
   function Get_Expected_SHA256 (Version : Version_String_Type) return String;

end TZif.Domain.Value_Object.IANA_Releases;
