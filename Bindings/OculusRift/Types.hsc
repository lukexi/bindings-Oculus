--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
#include "OVR_CAPI_0_5_0.h"
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Bindings.OculusRift.Types where

import Foreign.C
import Foreign.Ptr
--import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Word
import Data.Bits 

data VoidPtr = VoidPtr
  deriving (Show,Eq)
type HWND = Ptr VoidPtr
type HDC = Ptr VoidPtr

type OvrBool = CChar

data OvrVector2i = OvrVector2i
  { v2i_x :: Int
  , v2i_y :: Int
  } deriving (Show)
instance Storable OvrVector2i where
  sizeOf _ = (#size ovrVector2i)
  alignment = sizeOf 
  peek ptr = do
    x <- fmap fromIntegral ((#peek ovrVector2i, x) ptr :: IO CInt)
    y <- fmap fromIntegral ((#peek ovrVector2i, y) ptr :: IO CInt)
    return $ OvrVector2i x y
  poke ptr (OvrVector2i x y) = do
    (#poke ovrVector2i, x) ptr (fromIntegral x :: CInt)
    (#poke ovrVector2i, y) ptr (fromIntegral y :: CInt)

data OvrSizei = OvrSizei
  { si_w :: Int
  , si_h :: Int
  } deriving (Show)
instance Storable OvrSizei where
  sizeOf _ = (#size ovrSizei)
  alignment = sizeOf 
  peek ptr = do
    w <- fmap fromIntegral ((#peek ovrSizei, w) ptr :: IO CInt)
    h <- fmap fromIntegral ((#peek ovrSizei, h) ptr :: IO CInt)
    return $ OvrSizei w h 
  poke ptr (OvrSizei w h) = do
    (#poke ovrSizei, w) ptr (fromIntegral w :: CInt)
    (#poke ovrSizei, h) ptr (fromIntegral h :: CInt)

data OvrRecti = OvrRecti
  { ri_pos :: OvrVector2i
  , ri_size :: OvrSizei
  } deriving (Show)

instance Storable OvrRecti where
  sizeOf _ = (#size ovrRecti)
  alignment = sizeOf 
  peek ptr = do
    p <- (#peek ovrRecti, Pos) ptr 
    s <- (#peek ovrRecti, Size) ptr 
    return $ OvrRecti p s 
  poke ptr (OvrRecti p s) = do
    (#poke ovrRecti, Pos) ptr p 
    (#poke ovrRecti, Size) ptr s 

data OvrQuatf = OvrQuatf
  { qf_x :: CFloat
  , qf_y :: CFloat
  , qf_z :: CFloat
  , qf_w :: CFloat
  }deriving(Show)
instance Storable OvrQuatf where
  sizeOf _ = (#size ovrQuatf)
  alignment = sizeOf 
  peek ptr = do
    x <- (#peek ovrQuatf, x) ptr 
    y <- (#peek ovrQuatf, y) ptr 
    z <- (#peek ovrQuatf, z) ptr 
    w <- (#peek ovrQuatf, w) ptr 
    return $ OvrQuatf x y z w 
  poke ptr (OvrQuatf x y z w) = do
    (#poke ovrQuatf, x) ptr x 
    (#poke ovrQuatf, y) ptr y 
    (#poke ovrQuatf, z) ptr z 
    (#poke ovrQuatf, w) ptr w 

data OvrVector2f = OvrVector2f
  { v2f_x :: CFloat
  , v2f_y :: CFloat
  } deriving (Show)

instance Storable OvrVector2f where
  sizeOf _ = (#size ovrVector2f)
  alignment = sizeOf 
  peek ptr = do
    x <- (#peek ovrVector2f, x) ptr 
    y <- (#peek ovrVector2f, y) ptr 
    return $ OvrVector2f x y 
  poke ptr (OvrVector2f x y) = do
    (#poke ovrVector2f, x) ptr x 
    (#poke ovrVector2f, y) ptr y 

data OvrVector3f = OvrVector3f
  { v3f_x :: CFloat
  , v3f_y :: CFloat
  , v3f_z :: CFloat
  } deriving (Show)

instance Storable OvrVector3f where
  sizeOf _ = (#size ovrVector3f)
  alignment = sizeOf 
  peek ptr = do
    x <- (#peek ovrVector3f, x) ptr 
    y <- (#peek ovrVector3f, y) ptr 
    z <- (#peek ovrVector3f, z) ptr 
    return $ OvrVector3f x y z
  poke ptr (OvrVector3f x y z) = do
    (#poke ovrVector3f, x) ptr x 
    (#poke ovrVector3f, y) ptr y 
    (#poke ovrVector3f, z) ptr z 

data OvrMatrix4f = OvrMatrix4f
  { mf_m :: [[CFloat]] }

instance Storable OvrMatrix4f where
  sizeOf _ = (#size ovrMatrix4f)
  alignment = sizeOf 
  peek ptr = do
    v1 <- peekArray 4 (ptr `plusPtr` (#offset ovrMatrix4f, M[0]) )
    v2 <- peekArray 4 (ptr `plusPtr` (#offset ovrMatrix4f, M[1]) )
    v3 <- peekArray 4 (ptr `plusPtr` (#offset ovrMatrix4f, M[2]) )
    v4 <- peekArray 4 (ptr `plusPtr` (#offset ovrMatrix4f, M[3]) )
    return $ OvrMatrix4f [v1,v2,v3,v4] 
  poke ptr (OvrMatrix4f [v1,v2,v3,v4]) = do
    pokeArray (ptr `plusPtr` (#offset ovrMatrix4f, M[0]) ) v1
    pokeArray (ptr `plusPtr` (#offset ovrMatrix4f, M[1]) ) v2
    pokeArray (ptr `plusPtr` (#offset ovrMatrix4f, M[2]) ) v3
    pokeArray (ptr `plusPtr` (#offset ovrMatrix4f, M[3]) ) v4
  poke _ (OvrMatrix4f _) = return ()

data OvrPosef = OvrPosef
  { orientation :: OvrQuatf
  , position :: OvrVector3f
  } deriving (Show)

instance Storable OvrPosef where
  sizeOf _ = (#size ovrPosef)
  alignment = sizeOf 
  peek ptr = do
    o <- (#peek ovrPosef, Orientation) ptr 
    p <- (#peek ovrPosef, Position) ptr 
    return $ OvrPosef o p 
  poke ptr (OvrPosef o p) = do
    (#poke ovrPosef, Orientation) ptr o 
    (#poke ovrPosef, Position) ptr p 

data OvrPoseStatef = OvrPoseStatef
  { thePose :: OvrPosef
  , angularVelocity :: OvrVector3f
  , linearVelocity :: OvrVector3f
  , angularAcceleration :: OvrVector3f
  , linearAcceleration :: OvrVector3f
  , timeInSeconds :: CDouble -- Absolute time of this state sample.
  } deriving (Show)

instance Storable OvrPoseStatef where
  sizeOf _ = (#size ovrPoseStatef)
  alignment = sizeOf 
  peek ptr = do
    tp <- (#peek ovrPoseStatef, ThePose) ptr 
    av <- (#peek ovrPoseStatef, AngularVelocity) ptr 
    lv <- (#peek ovrPoseStatef, LinearVelocity) ptr
    aa <- (#peek ovrPoseStatef, AngularAcceleration) ptr
    la <- (#peek ovrPoseStatef, LinearAcceleration) ptr
    tis <- (#peek ovrPoseStatef, TimeInSeconds) ptr 
    return $ OvrPoseStatef tp av lv aa la tis 
  poke _ _ = return ()

data OvrFovPort = OvrFovPort
  { upTan :: Float
  , downTan :: Float
  , leftTan :: Float
  , rightTan :: Float
  } deriving (Eq,Show)

instance Storable OvrFovPort where
  sizeOf _ = (#size ovrFovPort)
  alignment = sizeOf 
  peek ptr = do
    u <- toFloat ((#peek ovrFovPort, UpTan) ptr :: IO CFloat)
    d <- toFloat ((#peek ovrFovPort, DownTan) ptr :: IO CFloat)
    l <- toFloat ((#peek ovrFovPort, LeftTan) ptr :: IO CFloat)
    r <- toFloat ((#peek ovrFovPort, RightTan) ptr :: IO CFloat)
    return $ OvrFovPort u d l r
    where toFloat = fmap realToFrac
  poke ptr (OvrFovPort u d l r) = do
    (#poke ovrFovPort, UpTan) ptr $ fromFloat u 
    (#poke ovrFovPort, DownTan) ptr $ fromFloat  d 
    (#poke ovrFovPort, LeftTan) ptr $ fromFloat l 
    (#poke ovrFovPort, RightTan) ptr $ fromFloat r 
    where fromFloat x = realToFrac x :: CFloat

newtype OvrHmdType = OvrHmdType { _hmdType :: CInt }
  deriving (Eq,Show)
#{enum OvrHmdType , OvrHmdType,
    ovrHmd_None     = ovrHmd_None,    
    ovrHmd_DK1      = ovrHmd_DK1,
    ovrHmd_DKHD     = ovrHmd_DKHD,    
    ovrHmd_DK2      = ovrHmd_DK2,
    ovrHmd_Other    = ovrHmd_Other
 }

instance Storable OvrHmdType where
  sizeOf _ = (#size ovrHmdType)
  alignment = sizeOf 
  peek ptr = do
    h <- peek (castPtr ptr) :: IO CInt 
    return $ OvrHmdType h 
  poke ptr (OvrHmdType h) = do
    poke (castPtr ptr :: Ptr CInt) h 

newtype OvrHmdCaps = OvrHmdCaps { _hmdCaps :: Word32 }
  deriving (Show,Eq)
#{ enum OvrHmdCaps , OvrHmdCaps,
    ovrHmdCap_Present           = ovrHmdCap_Present,
    ovrHmdCap_Available         = ovrHmdCap_Available,
    ovrHmdCap_Captured          = ovrHmdCap_Captured,
    ovrHmdCap_ExtendDesktop     = ovrHmdCap_ExtendDesktop, 
    ovrHmdCap_NoMirrorToWindow  = ovrHmdCap_NoMirrorToWindow,
    ovrHmdCap_DisplayOff        = ovrHmdCap_DisplayOff,
    ovrHmdCap_LowPersistence    = ovrHmdCap_LowPersistence,
    ovrHmdCap_DynamicPrediction = ovrHmdCap_DynamicPrediction,
    ovrHmdCap_NoVSync           = ovrHmdCap_NoVSync,

    ovrHmdCap_Writable_Mask     = ovrHmdCap_Writable_Mask,
    ovrHmdCap_Service_Mask      = ovrHmdCap_Service_Mask
 }
instance Bits OvrHmdCaps where
  (OvrHmdCaps a) .|. (OvrHmdCaps b) = OvrHmdCaps (a .|. b)
  (OvrHmdCaps a) .&. (OvrHmdCaps b) = OvrHmdCaps (a .&. b)
  xor (OvrHmdCaps a) (OvrHmdCaps b) = OvrHmdCaps (xor a b)
  complement (OvrHmdCaps a) = OvrHmdCaps (complement a)
  shift (OvrHmdCaps a) i = OvrHmdCaps (shift a i)
  shiftL (OvrHmdCaps a) i = OvrHmdCaps (shiftL a i)
  shiftR (OvrHmdCaps a) i = OvrHmdCaps (shiftR a i)
  rotate (OvrHmdCaps a) i = OvrHmdCaps (rotate a i)
  rotateL (OvrHmdCaps a) i = OvrHmdCaps (rotateL a i)
  rotateR (OvrHmdCaps a) i = OvrHmdCaps (rotateR a i)
  bitSizeMaybe (OvrHmdCaps a) = bitSizeMaybe a
  bitSize (OvrHmdCaps a) = finiteBitSize a
  isSigned (OvrHmdCaps a) = isSigned a
  testBit (OvrHmdCaps a) i = testBit a i
  bit i = OvrHmdCaps (bit i) 
  popCount (OvrHmdCaps a) = popCount a 

newtype OvrTrackingCaps = OvrTrackingCaps { _trackingCaps :: Word32 }
  deriving (Eq,Show)
#{ enum OvrTrackingCaps , OvrTrackingCaps,
    ovrTrackingCap_Orientation      = ovrTrackingCap_Orientation,
    ovrTrackingCap_MagYawCorrection = ovrTrackingCap_MagYawCorrection,
    ovrTrackingCap_Position         = ovrTrackingCap_Position,
    ovrTrackingCap_Idle             = ovrTrackingCap_Idle
 }
ovrTrackingCap_None :: OvrTrackingCaps
ovrTrackingCap_None = OvrTrackingCaps 0
instance Bits OvrTrackingCaps where
  (OvrTrackingCaps a) .|. (OvrTrackingCaps b) = OvrTrackingCaps (a .|. b)
  (OvrTrackingCaps a) .&. (OvrTrackingCaps b) = OvrTrackingCaps (a .&. b)
  xor (OvrTrackingCaps a) (OvrTrackingCaps b) = OvrTrackingCaps (xor a b)
  complement (OvrTrackingCaps a) = OvrTrackingCaps (complement a)
  shift (OvrTrackingCaps a) i = OvrTrackingCaps (shift a i)
  shiftL (OvrTrackingCaps a) i = OvrTrackingCaps (shiftL a i)
  shiftR (OvrTrackingCaps a) i = OvrTrackingCaps (shiftR a i)
  rotate (OvrTrackingCaps a) i = OvrTrackingCaps (rotate a i)
  rotateL (OvrTrackingCaps a) i = OvrTrackingCaps (rotateL a i)
  rotateR (OvrTrackingCaps a) i = OvrTrackingCaps (rotateR a i)
  bitSizeMaybe (OvrTrackingCaps a) = bitSizeMaybe a
  bitSize (OvrTrackingCaps a) = finiteBitSize a
  isSigned (OvrTrackingCaps a) = isSigned a
  testBit (OvrTrackingCaps a) i = testBit a i
  bit i = OvrTrackingCaps (bit i) 
  popCount (OvrTrackingCaps a) = popCount a 

newtype OvrDistortionCaps = OvrDistortionCaps { _distortion :: Word32 }
  deriving (Show,Eq)
#{ enum OvrDistortionCaps , OvrDistortionCaps,
    ovrDistortionCap_TimeWarp   = ovrDistortionCap_TimeWarp,
    ovrDistortionCap_Vignette   = ovrDistortionCap_Vignette,
    ovrDistortionCap_NoRestore  = ovrDistortionCap_NoRestore,
    ovrDistortionCap_FlipInput  = ovrDistortionCap_FlipInput,
    ovrDistortionCap_SRGB       = ovrDistortionCap_SRGB,
    ovrDistortionCap_Overdrive  = ovrDistortionCap_Overdrive,
    ovrDistortionCap_HqDistortion = ovrDistortionCap_HqDistortion,
    ovrDistortionCap_LinuxDevFullscreen = ovrDistortionCap_LinuxDevFullscreen,
    ovrDistortionCap_ComputeShader = ovrDistortionCap_ComputeShader,
    ovrDistortionCap_TimewarpJitDelay = ovrDistortionCap_TimewarpJitDelay,
    ovrDistortionCap_ProfileNoSpinWaits = ovrDistortionCap_ProfileNoSpinWaits
 }
instance Bits OvrDistortionCaps where
  (OvrDistortionCaps a) .|. (OvrDistortionCaps b) = OvrDistortionCaps (a .|. b)
  (OvrDistortionCaps a) .&. (OvrDistortionCaps b) = OvrDistortionCaps (a .&. b)
  xor (OvrDistortionCaps a) (OvrDistortionCaps b) = OvrDistortionCaps (xor a b)
  complement (OvrDistortionCaps a) = OvrDistortionCaps (complement a)
  shift (OvrDistortionCaps a) i = OvrDistortionCaps (shift a i)
  shiftL (OvrDistortionCaps a) i = OvrDistortionCaps (shiftL a i)
  shiftR (OvrDistortionCaps a) i = OvrDistortionCaps (shiftR a i)
  rotate (OvrDistortionCaps a) i = OvrDistortionCaps (rotate a i)
  rotateL (OvrDistortionCaps a) i = OvrDistortionCaps (rotateL a i)
  rotateR (OvrDistortionCaps a) i = OvrDistortionCaps (rotateR a i)
  bitSizeMaybe (OvrDistortionCaps a) = bitSizeMaybe a
  bitSize (OvrDistortionCaps a) = finiteBitSize a
  isSigned (OvrDistortionCaps a) = isSigned a
  testBit (OvrDistortionCaps a) i = testBit a i
  bit i = OvrDistortionCaps (bit i) 
  popCount (OvrDistortionCaps a) = popCount a 

newtype OvrEyeType = OvrEyeType { _ovrEyeType :: CInt }
  deriving (Eq,Show)
#{enum OvrEyeType , OvrEyeType, 
        ovrEye_Left  = ovrEye_Left,
        ovrEye_Right = ovrEye_Right,
        ovrEye_Count = ovrEye_Count
  } 
instance Storable OvrEyeType where
  sizeOf _ = (#size ovrEyeType)
  alignment = sizeOf 
  peek ptr = do
    e <- peek (castPtr ptr :: Ptr CInt)
    return $ OvrEyeType e
  poke ptr (OvrEyeType e) = do
    poke (castPtr ptr :: Ptr CInt) e 

data OvrHmdStruct = OvrHmdStruct

data OvrHmdDesc = OvrHmdDesc
  { hmdtype :: OvrHmdType
  , productName :: String  
  , manufacturer :: String
    
  , vendorId :: Int
  , productId :: Int

  , serialNumber :: String
  , firmwareMajor :: Int
  , firmwareMinor :: Int

  , cameraFrustumHFovInRadians :: Float
  , cameraFrustumVFovInRadians :: Float
  , cameraFrustumNearZInMeters :: Float
  , cameraFrustumFarZInMeters :: Float

  , hmdCaps :: Word 
  , trackingCaps :: Word
  , distortionCaps :: Word 

  , defaultEyeFov :: [OvrFovPort] -- ovrEye_Count
  , maxEyeFov :: [OvrFovPort] -- ovrEye_Count

  , eyeRenderOrder :: [OvrEyeType] -- ovrEye_Count
  , resolution :: OvrSizei 
  , windowsPos :: OvrVector2i
  , displayDeviceName :: String 
  , displayId :: Int 
  }

instance Storable OvrHmdDesc where
  sizeOf _ = (#size ovrHmdDesc)
  alignment = sizeOf 
  peek ptr = do
    t  <- (#peek ovrHmdDesc, Type) ptr 
    pn <- peekCString =<< ((#peek ovrHmdDesc, ProductName) ptr)
    m <- peekCString =<< ((#peek ovrHmdDesc, Manufacturer) ptr)
    vi <- toInt ((#peek ovrHmdDesc, VendorId) ptr :: IO CShort)
    pid <- toInt ((#peek ovrHmdDesc, ProductId) ptr :: IO CShort)
    sn <- fmap ((takeWhile (/= '\0')) . (map castCCharToChar)) $
            peekArray 24 (ptr `plusPtr` (#offset ovrHmdDesc, SerialNumber))
    fm <- toInt ((#peek ovrHmdDesc, FirmwareMajor) ptr :: IO CShort)
    fn <- toInt ((#peek ovrHmdDesc, FirmwareMinor) ptr :: IO CShort)

    hfir <- toFloat ((#peek ovrHmdDesc, CameraFrustumHFovInRadians) ptr :: IO CFloat)
    vfir <- toFloat ((#peek ovrHmdDesc, CameraFrustumVFovInRadians) ptr :: IO CFloat)
    nzim <- toFloat ((#peek ovrHmdDesc, CameraFrustumNearZInMeters) ptr :: IO CFloat)
    fzim <- toFloat ((#peek ovrHmdDesc, CameraFrustumFarZInMeters) ptr :: IO CFloat)

    hc <- toWord ((#peek ovrHmdDesc, HmdCaps) ptr :: IO CUInt)
    tc <- toWord ((#peek ovrHmdDesc, TrackingCaps) ptr :: IO CUInt)
    dic <- toWord ((#peek ovrHmdDesc, DistortionCaps) ptr :: IO CUInt)

    def <- peekArray oec (ptr `plusPtr` (#offset ovrHmdDesc, DefaultEyeFov))
    mef <- peekArray oec (ptr `plusPtr` (#offset ovrHmdDesc, MaxEyeFov) )
    ero <- peekArray oec (ptr `plusPtr` (#offset ovrHmdDesc, EyeRenderOrder) )
    r  <- (#peek ovrHmdDesc, Resolution) ptr
    wp <- (#peek ovrHmdDesc, WindowsPos) ptr
    ddn <- peekCString =<< ((#peek ovrHmdDesc, DisplayDeviceName) ptr)
    di <- fmap fromIntegral ((#peek ovrHmdDesc, DisplayId) ptr :: IO CInt)
    return $ OvrHmdDesc t pn m vi pid sn fm fn hfir vfir nzim fzim
                        hc tc dic def mef ero r wp ddn di
    where
      oec = fromIntegral $ (\ (OvrEyeType n) -> n) ovrEye_Count
      toInt = fmap fromIntegral
      toFloat = fmap realToFrac
      toWord = fmap fromIntegral
  poke _ _ = undefined

type OvrHmd = Ptr OvrHmdDesc 

-- util --
getDefaultEyeFovPtr :: OvrHmd -> OvrEyeType -> (Ptr OvrFovPort)
getDefaultEyeFovPtr hmd (OvrEyeType n) =
  hmd `plusPtr` (#offset ovrHmdDesc, DefaultEyeFov)
      `plusPtr` (n' * (#size ovrFovPort ))
  where n' = fromIntegral n :: Int

newtype OvrStatusBits = OvrStatusBits { _statusBits :: CInt }
#{enum OvrStatusBits , OvrStatusBits,
    ovrStatus_OrientationTracked    = ovrStatus_OrientationTracked,
    ovrStatus_PositionTracked       = ovrStatus_PositionTracked,
    ovrStatus_CameraPoseTracked     = ovrStatus_CameraPoseTracked,
    ovrStatus_PositionConnected     = ovrStatus_PositionConnected,
    ovrStatus_HmdConnected          = ovrStatus_HmdConnected
 }
instance Storable OvrStatusBits where
  sizeOf _ = (#size ovrEyeType)
  alignment = sizeOf 
  peek ptr = do
    s <- peek ptr 
    return s
  poke ptr s = do
    poke ptr s 

data OvrSensorData = OvrSensorData
  { accelerometer :: OvrVector3f
  , gyro :: OvrVector3f
  , magnetometer :: OvrVector3f
  , temperature :: CFloat
  , timeInSecondsS :: CFloat
  } deriving (Show)
instance Storable OvrSensorData where
  sizeOf _ = (#size ovrSensorData)
  alignment = sizeOf 
  peek ptr = do
    a <- (#peek ovrSensorData, Accelerometer) ptr 
    g <- (#peek ovrSensorData, Gyro) ptr 
    m <- (#peek ovrSensorData, Magnetometer) ptr 
    t <- (#peek ovrSensorData, Temperature) ptr 
    ti <- (#peek ovrSensorData, TimeInSeconds) ptr 
    return $ OvrSensorData a g m t ti 
  poke ptr (OvrSensorData a g m t ti) = do
    (#poke ovrSensorData, Accelerometer) ptr a 
    (#poke ovrSensorData, Gyro) ptr g 
    (#poke ovrSensorData, Magnetometer) ptr m 
    (#poke ovrSensorData, Temperature) ptr t 
    (#poke ovrSensorData, TimeInSeconds) ptr ti

data OvrTrackingState = OvrTrackingState
  { headPose :: OvrPoseStatef
  , cameraPose :: OvrPosef
  , leveledCameraPose :: OvrPosef
  , rawSensorData :: OvrSensorData 
  , statusFlags :: Word32
  } deriving (Show)

instance Storable OvrTrackingState where
  sizeOf _ = (#size ovrTrackingState)
  alignment = sizeOf 
  peek ptr = do
    hp <- (#peek ovrTrackingState, HeadPose) ptr 
    cp <- (#peek ovrTrackingState, CameraPose) ptr 
    lcp <- (#peek ovrTrackingState, LeveledCameraPose) ptr
    rsd <- (#peek ovrTrackingState, RawSensorData) ptr
    sf <- fmap fromIntegral
               ((#peek ovrTrackingState, StatusFlags) ptr :: IO CUInt)
    return $ OvrTrackingState hp cp lcp rsd sf
  poke _ _ = return () 

    
data OvrFrameTiming = OvrFrameTiming
  { deltaSeconds :: CFloat
  , thisFrameSeconds :: CDouble
  , timewarpPointSeconds :: CDouble
  , nextFrameSeconds :: CDouble
  , scanoutMidpointSeconds ::CDouble
  , eyeScanoutSeconds :: [CDouble]
  }
instance Storable OvrFrameTiming where
  sizeOf _ = (#size ovrFrameTiming)
  alignment = sizeOf 
  peek ptr = do
    ds <- (#peek ovrFrameTiming, DeltaSeconds) ptr
    tfs <- (#peek ovrFrameTiming, ThisFrameSeconds) ptr
    tps <- (#peek ovrFrameTiming, TimewarpPointSeconds) ptr
    nfs <- (#peek ovrFrameTiming, NextFrameSeconds) ptr
    sms <- (#peek ovrFrameTiming, ScanoutMidpointSeconds) ptr
    ess <- peekArray 2 (ptr `plusPtr` (#offset ovrFrameTiming, EyeScanoutSeconds) )
    return $ OvrFrameTiming ds tfs tps nfs sms ess 
  poke ptr (OvrFrameTiming ds tfs tps nfs sms ess) = do
    (#poke ovrFrameTiming, DeltaSeconds) ptr ds 
    (#poke ovrFrameTiming, ThisFrameSeconds) ptr tfs 
    (#poke ovrFrameTiming, TimewarpPointSeconds) ptr tps 
    (#poke ovrFrameTiming, NextFrameSeconds) ptr nfs 
    (#poke ovrFrameTiming, ScanoutMidpointSeconds) ptr sms 
    pokeArray (ptr `plusPtr` (#offset ovrFrameTiming, EyeScanoutSeconds) ) ess 

data OvrEyeRenderDesc = OvrEyeRenderDesc
  { eye :: OvrEyeType
  , fov :: OvrFovPort
  , distortedViewport :: OvrRecti
  , pixelsPerTanAngleAtCenter :: OvrVector2f
  , hmdToEyeViewOffset :: OvrVector3f
  } deriving (Show)
instance Storable OvrEyeRenderDesc where
  sizeOf _ = (#size ovrEyeRenderDesc)
  alignment = sizeOf 
  peek ptr = do
    e <- (#peek ovrEyeRenderDesc, Eye) ptr
    f <- (#peek ovrEyeRenderDesc, Fov) ptr
    dv <- (#peek ovrEyeRenderDesc, DistortedViewport) ptr
    pptaac <- (#peek ovrEyeRenderDesc, PixelsPerTanAngleAtCenter) ptr
    va <- (#peek ovrEyeRenderDesc, HmdToEyeViewOffset) ptr
    return $ OvrEyeRenderDesc e f dv pptaac va 
  poke ptr (OvrEyeRenderDesc e f dv pptaac va) = do
    (#poke ovrEyeRenderDesc, Eye) ptr e 
    (#poke ovrEyeRenderDesc, Fov) ptr f 
    (#poke ovrEyeRenderDesc, DistortedViewport) ptr dv 
    (#poke ovrEyeRenderDesc, PixelsPerTanAngleAtCenter) ptr pptaac 
    (#poke ovrEyeRenderDesc, HmdToEyeViewOffset) ptr va 

newtype OvrRenderAPIType = OvrRenderAPIType { _renderAPI :: CInt }
  deriving (Show,Eq)
#{enum OvrRenderAPIType , OvrRenderAPIType,
    ovrRenderAPI_None = ovrRenderAPI_None,
    ovrRenderAPI_OpenGL = ovrRenderAPI_OpenGL,
    ovrRenderAPI_Android_GLES = ovrRenderAPI_Android_GLES,
    ovrRenderAPI_D3D9 = ovrRenderAPI_D3D9,
    ovrRenderAPI_D3D10 = ovrRenderAPI_D3D10,
    ovrRenderAPI_D3D11 = ovrRenderAPI_D3D11,
    ovrRenderAPI_Count = ovrRenderAPI_Count
 } 
instance Storable OvrRenderAPIType where
  sizeOf _ = (#size ovrRenderAPIType)
  alignment = sizeOf 
  peek ptr = do
    h <- peek (castPtr ptr) :: IO CInt 
    return $ OvrRenderAPIType h
  poke ptr (OvrRenderAPIType h) = do
    poke (castPtr ptr :: Ptr CInt) h 

data OvrRenderAPIConfigHeader = OvrRenderAPIConfigHeader
  { api :: OvrRenderAPIType
  , tTSize :: OvrSizei
  , multisample :: CInt
  } deriving (Show)
instance Storable OvrRenderAPIConfigHeader where
  sizeOf _ = (#size ovrRenderAPIConfigHeader)
  alignment = sizeOf 
  peek = undefined
  poke ptr (OvrRenderAPIConfigHeader a r m) = do
    (#poke ovrRenderAPIConfigHeader, API) ptr a 
    (#poke ovrRenderAPIConfigHeader, BackBufferSize) ptr r 
    (#poke ovrRenderAPIConfigHeader, Multisample) ptr m 

data OvrRenderAPIConfig = OvrRenderAPIConfig -- for OpenGL 
  { header :: OvrRenderAPIConfigHeader
  , window :: Maybe HWND
  , dc :: Maybe HDC
  }
instance Storable OvrRenderAPIConfig where
  sizeOf _ = (#size ovrRenderAPIConfig)
  alignment = sizeOf 
  peek = undefined
  poke ptr (OvrRenderAPIConfig h w d) = do
  --poke ptr (OvrRenderAPIConfig h ) = do
    (#poke ovrRenderAPIConfig, Header) ptr h 
    (#poke ovrRenderAPIConfig, PlatformData[0]) ptr $
      case w of
        Just w' -> w'
        Nothing -> nullPtr
    (#poke ovrRenderAPIConfig, PlatformData[1]) ptr $ 
      case d of
        Just d' -> d'
        Nothing -> nullPtr
    (#poke ovrRenderAPIConfig, PlatformData[0]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[1]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[2]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[3]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[4]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[5]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[6]) ptr nullPtr 
    (#poke ovrRenderAPIConfig, PlatformData[7]) ptr nullPtr 

data OvrTextureHeader = OvrTextureHeader
  { apiT :: OvrRenderAPIType
  , textureSize :: OvrSizei
  , renderViewport :: OvrRecti
  } deriving (Show)
instance Storable OvrTextureHeader where
  sizeOf _ = (#size ovrTextureHeader)
  alignment = sizeOf 
  peek ptr = do
    a <- (#peek ovrTextureHeader, API) ptr
    t <- (#peek ovrTextureHeader, TextureSize) ptr
    rv <- (#peek ovrTextureHeader, RenderViewport) ptr
    return $ OvrTextureHeader a t rv 
  poke ptr (OvrTextureHeader a t rv) = do
    (#poke ovrTextureHeader, API) ptr a
    (#poke ovrTextureHeader, TextureSize) ptr t
    (#poke ovrTextureHeader, RenderViewport) ptr rv

data OvrTexture = OvrTexture
  { headerT :: OvrTextureHeader
  , texID :: CUInt
  } deriving (Show)
instance Storable OvrTexture where
  sizeOf _ = (#size ovrTexture)
  alignment = sizeOf 
  peek ptr = do
    h <- (#peek ovrTexture, Header) ptr
    t <- (#peek ovrTexture, PlatformData[0]) ptr
    return $ OvrTexture h t  
  poke ptr (OvrTexture h t) = do
    (#poke ovrTexture, Header) ptr h 
    (#poke ovrTexture, PlatformData[0]) ptr t
    (#poke ovrTexture, PlatformData[1]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[2]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[3]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[4]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[5]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[6]) ptr nullPtr 
    (#poke ovrTexture, PlatformData[7]) ptr nullPtr 

------------------------------------------------------------------------
-- API --
foreign import ccall safe "_ovr_InitializeRenderingShim" c_ovr_InitializeRenderingShim :: IO OvrBool

foreign import ccall safe "_ovr_Initialize" c_ovr_Initialize :: IO OvrBool

foreign import ccall safe "_ovr_Shutdown" c_ovr_Shutdown :: IO ()

foreign import ccall safe "_ovr_GetVersionString" c_ovr_GetVersionString :: CString

foreign import ccall safe "_ovrHmd_Detect" c_ovrHmd_Detect :: IO CInt

foreign import ccall safe "_ovrHmd_Create" c_ovrHmd_Create :: CInt -> IO OvrHmd

foreign import ccall safe "_ovrHmd_Destroy" c_ovrHmd_Destroy :: OvrHmd -> IO () 

foreign import ccall safe "_ovrHmd_CreateDebug" c_ovrHmd_CreateDebug :: CInt -> IO OvrHmd 

foreign import ccall safe "_ovrHmd_GetLastError" c_ovrHmd_GetLastError :: OvrHmd -> IO CString

foreign import ccall safe "_ovrHmd_AttachToWindow" c_ovrHmd_AttachToWindow :: OvrHmd -> HWND -> Ptr OvrRecti -> Ptr OvrRecti -> IO OvrBool 

-------------------------------------------------------------------------------------

foreign import ccall safe "_ovrHmd_GetEnabledCaps" c_ovrHmd_GetEnabledCaps :: OvrHmd -> IO CUInt

foreign import ccall safe "_ovrHmd_SetEnabledCaps" c_ovrHmd_SetEnabledCaps :: OvrHmd -> CUInt -> IO ()

-------------------------------------------------------------------------------------
-- ***** Tracking Interface

foreign import ccall safe "_ovrHmd_ConfigureTracking" c_ovrHmd_ConfigureTracking :: OvrHmd -> CUInt -> CUInt -> IO OvrBool

foreign import ccall safe "_ovrHmd_RecenterPose" c_ovrHmd_RecenterPose :: OvrHmd -> IO ()

foreign import ccall safe "_ovrHmd_GetTrackingState" c_ovrHmd_GetTrackingState :: OvrHmd -> CDouble -> IO (Ptr OvrTrackingState)

-------------------------------------------------------------------------------------
-- ***** Graphics Setup

foreign import ccall safe "_ovrHmd_GetFovTextureSize" c_ovrHmd_GetFovTextureSize :: OvrHmd -> CInt -> Ptr OvrFovPort -> CFloat -> IO (Ptr OvrSizei)

-------------------------------------------------------------------------------------
-- | *****  SDK Distortion Rendering Functions

foreign import ccall safe "_ovrHmd_ConfigureRendering" c_ovrHmd_ConfigureRendering :: OvrHmd -> Ptr OvrRenderAPIConfig -> CUInt -> Ptr OvrFovPort -> Ptr OvrEyeRenderDesc -> IO OvrBool

foreign import ccall safe "_ovrHmd_BeginFrame" c_ovrHmd_BeginFrame :: OvrHmd -> CUInt -> IO (Ptr OvrFrameTiming)

foreign import ccall safe "_ovrHmd_EndFrame" c_ovrHmd_EndFrame :: OvrHmd -> Ptr OvrPosef -> Ptr OvrTexture -> IO ()

foreign import ccall safe "_ovrHmd_GetEyePoses" c_ovrHmd_GetEyePoses :: OvrHmd -> CUInt -> Ptr OvrVector3f -> Ptr OvrTrackingState -> IO (Ptr OvrPosef)

foreign import ccall safe "_ovrHmd_GetHmdPosePerEye" c_ovrHmd_GetHmdPosePerEye :: OvrHmd -> CInt -> IO (Ptr OvrPosef)

-------------------------------------------------------------------------------------
-- *****  Client Distortion Rendering Functions
--

foreign import ccall safe "_ovrHmd_GetRenderDesc" c_ovrHmd_GetRenderDesc :: OvrHmd -> OvrEyeType -> Ptr OvrPosef -> IO (Ptr OvrEyeRenderDesc)

data OvrDistortionVertex = OvrDistortionVertex
  { screenPosNDC :: OvrVector2f
  , timeWarpFactor :: CFloat
  , vignetteFactor :: CFloat
  , tanEyeAnglesR :: OvrVector2f
  , tanEyeAnglesG :: OvrVector2f
  , tanEyeAnglesB :: OvrVector2f
  }
instance Storable OvrDistortionVertex where
  sizeOf _ = (#size ovrDistortionVertex)
  alignment = sizeOf 
  peek ptr = do
    spn <- (#peek ovrDistortionVertex, ScreenPosNDC) ptr 
    twf <- (#peek ovrDistortionVertex, TimeWarpFactor) ptr 
    vf <- (#peek ovrDistortionVertex, VignetteFactor) ptr 
    r <- (#peek ovrDistortionVertex, TanEyeAnglesR) ptr 
    g <- (#peek ovrDistortionVertex, TanEyeAnglesG) ptr 
    b <- (#peek ovrDistortionVertex, TanEyeAnglesB) ptr 
    return $ OvrDistortionVertex spn twf vf r g b
  poke _ _ = return () 

data OvrDistortionMesh = OvrDistortionMesh
  { pVertexData :: Ptr OvrDistortionVertex
  , pIndexData :: Ptr CUShort
  , vertexCount :: CUInt
  , indexCount :: CUInt
  }
instance Storable OvrDistortionMesh where
  sizeOf _ = (#size ovrDistortionMesh)
  alignment = sizeOf 
  peek ptr = do
    pvd <- (#peek ovrDistortionMesh, pVertexData) ptr 
    pid <- (#peek ovrDistortionMesh, pIndexData) ptr 
    vc <- (#peek ovrDistortionMesh, VertexCount) ptr 
    ic <- (#peek ovrDistortionMesh, IndexCount) ptr 
    return $ OvrDistortionMesh pvd pid vc ic 
  poke _ _ = return () 

foreign import ccall safe "_ovrHmd_CreateDistortionMesh" c_ovrHmd_CreateDistortionMesh :: OvrHmd -> OvrEyeType -> Ptr OvrPosef -> CUInt -> Ptr OvrDistortionMesh -> IO OvrBool

foreign import ccall safe "_ovrHmd_DestroyDistortionMesh" c_ovrHmd_DestroyDistortionMesh :: Ptr OvrDistortionMesh -> IO ()

foreign import ccall safe "_ovrHmd_GetRenderScaleAndOffset" c_ovrHmd_GetRenderScaleAndOffset :: Ptr OvrFovPort -> Ptr OvrSizei -> Ptr OvrSizei -> Ptr OvrVector2f -> IO ()

foreign import ccall safe "_ovrHmd_GetFrameTiming" c_ovrHmd_GetFrameTiming :: OvrHmd -> CUInt -> IO (Ptr OvrFrameTiming)

foreign import ccall safe "_ovrHmd_BeginFrameTiming" c_ovrHmd_BeginFrameTiming :: OvrHmd -> CUInt -> IO (Ptr OvrFrameTiming)

foreign import ccall safe "_ovrHmd_EndFrameTiming" c_ovrHmd_EndFrameTiming :: OvrHmd -> IO ()

foreign import ccall safe "_ovrHmd_ResetFrameTiming" c_ovrHmd_ResetFrameTiming :: OvrHmd -> CUInt -> IO ()

foreign import ccall safe "_ovrHmd_GetEyeTimewarpMatrices" c_ovrHmd_GetEyeTimewarpMatrices :: OvrHmd -> OvrEyeType -> Ptr OvrPosef -> Ptr OvrMatrix4f -> IO ()

-------------------------------------------------------------------------------------
-- ***** Stateless math setup functions

foreign import ccall safe "_ovrMatrix4f_Projection" c_ovrMatrix4f_Projection :: Ptr OvrFovPort -> CFloat -> CFloat -> OvrBool -> IO (Ptr OvrMatrix4f)

foreign import ccall safe "_ovrMatrix4f_OrthoSubProjection" c_ovrMatrix4f_OrthoSubProjection :: Ptr OvrMatrix4f -> Ptr OvrVector2f -> CFloat -> CFloat -> IO (Ptr OvrMatrix4f)

foreign import ccall safe "_ovr_GetTimeInSeconds" c_ovr_GetTimeInSeconds :: IO CDouble

foreign import ccall safe "_ovr_WaitTillTime" c_ovr_WaitTillTime :: CDouble -> IO CDouble

-----------------------------------------------------------------------------------
-- ***** Latency Test interface

foreign import ccall safe "_ovrHmd_ProcessLatencyTest" c_ovrHmd_ProcessLatencyTest :: OvrHmd -> Ptr CUChar -> IO OvrBool

foreign import ccall safe "_ovrHmd_GetLatencyTestResult" c_ovrHmd_GetLatencyTestResult :: OvrHmd -> IO CString

-----------------------------------------------------------------------------------
-- ***** Health and Safety Warning Display interface

foreign import ccall safe "_ovrHmd_GetHSWDisplayState" c_ovrHmd_GetHSWDisplayState :: OvrHmd -> IO (Ptr OvrHSWDisplayState)

data OvrHSWDisplayState = OvrHSWDisplayState
  { displayed :: OvrBool
  , startTime :: CDouble
  , dismissibleTime :: CDouble
  }
instance Storable OvrHSWDisplayState where
  sizeOf _ = (#size ovrHSWDisplayState)
  alignment = sizeOf 
  peek ptr = do
    dp <- (#peek ovrHSWDisplayState, Displayed) ptr
    st <- (#peek ovrHSWDisplayState, StartTime) ptr
    dt <- (#peek ovrHSWDisplayState, DismissibleTime) ptr
    return $ OvrHSWDisplayState dp st dt 
  poke ptr (OvrHSWDisplayState dp st dt) = do
    (#poke ovrHSWDisplayState, Displayed) ptr dp 
    (#poke ovrHSWDisplayState, StartTime) ptr st 
    (#poke ovrHSWDisplayState, DismissibleTime) ptr dt 

foreign import ccall safe "_ovrHmd_DismissHSWDisplay" c_ovrHmd_DismissHSWDisplay :: OvrHmd -> IO OvrBool 

-----------------------------------------------------------------------------------
-- ***** Property Access

foreign import ccall safe "_ovrHmd_GetBool" c_ovrHmd_GetBool :: OvrHmd -> CString -> OvrBool -> IO OvrBool 

foreign import ccall safe "_ovrHmd_SetBool" c_ovrHmd_SetBool :: OvrHmd -> CString -> OvrBool -> IO OvrBool 

foreign import ccall safe "_ovrHmd_GetInt" c_ovrHmd_GetInt :: OvrHmd -> CString -> CInt -> IO CInt 

foreign import ccall safe "_ovrHmd_SetInt" c_ovrHmd_SetInt :: OvrHmd -> CString -> CInt -> IO OvrBool 

foreign import ccall safe "_ovrHmd_GetFloat" c_ovrHmd_GetFloat :: OvrHmd -> CString -> CFloat -> IO CFloat 

foreign import ccall safe "_ovrHmd_SetFloat" c_ovrHmd_SetFloat :: OvrHmd -> CString -> CFloat -> IO OvrBool 

foreign import ccall safe "_ovrHmd_GetFloatArray" c_ovrHmd_GetFloatArray :: OvrHmd -> CString -> Ptr CFloat -> CUInt -> IO CUInt 

foreign import ccall safe "_ovrHmd_SetFloatArray" c_ovrHmd_SetFloatArray :: OvrHmd -> CString -> Ptr CFloat -> CUInt -> IO OvrBool 

foreign import ccall safe "_ovrHmd_GetString" c_ovrHmd_GetString :: OvrHmd -> CString -> CString -> IO CString 

foreign import ccall safe "_ovrHmd_SetString" c_ovrHmd_SetString :: OvrHmd -> CString -> CString -> IO OvrBool 

