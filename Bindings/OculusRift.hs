--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
{-# LANGUAGE BangPatterns #-}
module Bindings.OculusRift where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types 
import Foreign.Storable 
import Foreign.Marshal.Array  
import Foreign.Marshal.Alloc
import Data.Word
import Bindings.OculusRift.Types

--import Debug.Trace

-- |
-- convert ovrBool to Bool
--
-- >>> ovrBool2Bool '1'
-- True
ovrBool2Bool :: OvrBool -> Bool
ovrBool2Bool = ('0' /= ).castCCharToChar 

bool2OvrBool :: Bool -> OvrBool
bool2OvrBool False = castCharToCChar '0' 
bool2OvrBool _ = castCharToCChar '1'

-- | 
-- castTo OvrHmdDesc
--
castToOvrHmdDesc :: Ptr OvrHmdDesc -> IO OvrHmdDesc
castToOvrHmdDesc pHmd = peek pHmd

-- |
-- printHmdDesc
--
printHmdDesc :: OvrHmdDesc -> IO ()
printHmdDesc hmd = do 
  putStrLn $ "Hmdtype : " ++ (show $ hmdtype hmd)
  putStrLn $ "ProductName : " ++ (productName hmd)
  putStrLn $ "Manufacturer : " ++ (manufacturer hmd)
  putStrLn $ "VendorID(USB) : " ++ (show $ vendorId hmd)
  putStrLn $ "ProductID(USB) : " ++ (show $ productId hmd)
  putStrLn $ "SerialNumber : " ++ (serialNumber hmd)
  putStrLn $ "Version Num : " ++ (show $ firmwareMajor hmd)
             ++ "." ++ (show $ firmwareMinor hmd)
  putStrLn $ "CameraFrustumHFovInRadians : " ++
            (show $ cameraFrustumHFovInRadians hmd)
  putStrLn $ "CameraFrustumVFovInRadians : " ++
            (show $ cameraFrustumVFovInRadians hmd)
  putStrLn $ "CameraFrustumNearZInMeters : " ++
            (show $ cameraFrustumNearZInMeters hmd)
  putStrLn $ "CameraFrustumFarZInMeters : " ++
            (show $ cameraFrustumFarZInMeters hmd) 
  putStrLn $ "HmdCaps : " ++ (show $ hmdCaps hmd)
  putStrLn $ "TrackingCaps : " ++ (show $ trackingCaps hmd)
  putStrLn $ "DistortionCaps : " ++ (show $ distortionCaps hmd)
  putStrLn $ "DefaultEyeFov : " ++ (show $ defaultEyeFov hmd)
  putStrLn $ "MaxEyeFov : " ++ (show $ maxEyeFov hmd)
  putStrLn $ "EyeRenderOrder : " ++ (show $ eyeRenderOrder hmd)
  putStrLn $ "Resolution : " ++ (show $ resolution hmd)
  putStrLn $ "WindowsPos : " ++ (show $ windowsPos hmd) 
  putStrLn $ "DisplayDeviceName : " ++ (displayDeviceName hmd)
  putStrLn $ "DisplayId : " ++ (show $ displayId hmd) 

-- | ovr_InitializeRenderingShim initializes the rendering shim appart from everything
-- else in LibOVR. This may be helpful if the application prefers to avoid
-- creating any OVR resources (allocations, service connections, etc) at this point.
-- ovr_InitializeRenderingShim does not bring up anything within LibOVR except the
-- necessary hooks to enable the Direct-to-Rift functionality.
--
-- Either ovr_InitializeRenderingShim() or ovr_Initialize() must be called before any
-- Direct3D or OpenGL initilization is done by applictaion (creation of devices, etc).
-- ovr_Initialize() must still be called after to use the rest of LibOVR APIs.
ovr_InitializeRenderingShim :: IO Bool
ovr_InitializeRenderingShim = fmap ovrBool2Bool c_ovr_InitializeRenderingShim 

-- | Library init/shutdown, must be called around all other OVR code.
-- No other functions calls are allowed before ovr_Initialize succeeds or after ovr_Shutdown.
-- Initializes all Oculus functionality.
--
ovr_Initialize :: IO Bool
ovr_Initialize = fmap ovrBool2Bool c_ovr_Initialize

-- | Shuts down all Oculus functionality.
--
ovr_Shutdown :: IO ()
ovr_Shutdown = c_ovr_Shutdown

-- | Returns version string representing libOVR version. Static, so
-- string remains valid for app lifespan
--
ovr_GetVersionString :: IO String
ovr_GetVersionString = peekCString c_ovr_GetVersionString


-- | Detects or re-detects HMDs and reports the total number detected.
-- Users can get information about each HMD by calling ovrHmd_Create with an index.
--
ovrHmd_Detect :: IO Int
ovrHmd_Detect = fmap fromIntegral c_ovrHmd_Detect

-- | Creates a handle to an HMD which doubles as a description structure.
-- Index can [0 .. ovrHmd_Detect()-1]. Index mappings can cange after each ovrHmd_Detect call.
-- If not null, then the returned handle must be freed with ovrHmd_Destroy.
ovrHmd_Create :: Int -> IO (Maybe OvrHmd)
ovrHmd_Create idx = do
  hdl <- c_ovrHmd_Create $ fromIntegral idx
  return $ if hdl /= nullPtr
             then Just hdl
             else Nothing

ovrHmd_Destroy :: OvrHmd -> IO ()
ovrHmd_Destroy = c_ovrHmd_Destroy   

-- | Creates a 'fake' HMD used for debugging only. This is not tied to specific hardware,
-- but may be used to debug some of the related rendering.
--
ovrHmd_CreateDebug :: OvrHmdType -> IO OvrHmd
ovrHmd_CreateDebug (OvrHmdType t) = c_ovrHmd_CreateDebug $ fromIntegral t

-- | Returns last error for HMD state. Returns null for no error.
-- String is valid until next call or GetLastError or HMD is destroyed.
-- Pass null hmd to get global errors (during create etc).
--
ovrHmd_GetLastError :: OvrHmd -> IO String
ovrHmd_GetLastError hmd = peekCString =<< c_ovrHmd_GetLastError hmd

-- | Platform specific function to specify the application window whose output will be 
-- displayed on the HMD. Only used if the ovrHmdCap_ExtendDesktop flag is false.
-- Windows: SwapChain associated with this window will be displayed on the HMD.
--          Specify 'destMirrorRect' in window coordinates to indicate an area
--          of the render target output that will be mirrored from 'sourceRenderTargetRect'.
--          Null pointers mean "full size".
-- @note Source and dest mirror rects are not yet implemented.
--
ovrHmd_AttachToWindow :: OvrHmd -> HWND -> Maybe OvrRecti -> Maybe OvrRecti
                      -> IO Bool 
ovrHmd_AttachToWindow hmd win destMirrorRect sourceRenderTargetRect =
  alloca $ \ destMirrorRect' -> 
  alloca $ \ sourceRenderTargetRect' -> do
    pd <- case destMirrorRect of
            Just d -> poke destMirrorRect' d >> return destMirrorRect'
            Nothing -> return nullPtr
    ps <- case sourceRenderTargetRect of
            Just s -> poke sourceRenderTargetRect' s
                        >> return sourceRenderTargetRect'
            Nothing -> return nullPtr
    fmap ovrBool2Bool (c_ovrHmd_AttachToWindow hmd win pd ps)

-------------------------------------------------------------------------------------

-- | Returns capability bits that are enabled at this time as described by ovrHmdCaps.
-- Note that this value is different font ovrHmdDesc::HmdCaps, which describes what
-- capabilities are available for that HMD.
--
ovrHmd_GetEnabledCaps :: OvrHmd -> IO OvrHmdCaps 
ovrHmd_GetEnabledCaps hmd = do
  (CUInt caps) <- c_ovrHmd_GetEnabledCaps hmd
  return $ OvrHmdCaps (fromIntegral caps)

-- | Modifies capability bits described by ovrHmdCaps that can be modified,
-- such as ovrHmd_LowPersistance.
--
ovrHmd_SetEnabledCaps :: OvrHmd -> OvrHmdCaps -> IO ()
ovrHmd_SetEnabledCaps hmd (OvrHmdCaps caps) =
  c_ovrHmd_SetEnabledCaps hmd (CUInt caps)

-------------------------------------------------------------------------------------
-- ***** Tracking Interface

-- | All tracking interface functions are thread-safe, allowing tracking state to be sampled
-- from different threads.
-- ConfigureTracking starts sensor sampling, enabling specified capabilities,
--    described by ovrTrackingCaps.
--  - supportedTrackingCaps specifies support that is requested. The function will succeed 
--	  even if these caps are not available (i.e. sensor or camera is unplugged). Support
--    will automatically be enabled if such device is plugged in later. Software should
--    check ovrTrackingState.StatusFlags for real-time status.
--  - requiredTrackingCaps specify sensor capabilities required at the time of the call.
--    If they are not available, the function will fail. Pass 0 if only specifying
--    supportedTrackingCaps.
--  - Pass 0 for both supportedTrackingCaps and requiredTrackingCaps to disable tracking.
--
--
ovrHmd_ConfigureTracking :: OvrHmd -> OvrTrackingCaps -> OvrTrackingCaps
                         -> IO Bool
ovrHmd_ConfigureTracking hmd (OvrTrackingCaps supportedTrackingCaps)
                             (OvrTrackingCaps requiredTrackingCaps) =
  fmap ovrBool2Bool $
       c_ovrHmd_ConfigureTracking hmd (CUInt supportedTrackingCaps)
                                      (CUInt requiredTrackingCaps)
 
-- | Re-centers the sensor orientation.
-- Normally this will recenter the (x,y,z) translational components and the yaw 
-- component of orientation.
--
ovrHmd_RecenterPose :: OvrHmd -> IO ()
ovrHmd_RecenterPose = c_ovrHmd_RecenterPose

-- | Returns tracking state reading based on the specified absolute system time.
-- Pass an absTime value of 0.0 to request the most recent sensor reading. In this case
-- both PredictedPose and SamplePose will have the same value.
-- ovrHmd_GetEyePose relies on this internally.
-- This may also be used for more refined timing of FrontBuffer rendering logic, etc.
--
ovrHmd_GetTrackingState :: OvrHmd -> Double -> IO OvrTrackingState
ovrHmd_GetTrackingState hmd absTime = do 
  peek =<< c_ovrHmd_GetTrackingState hmd (CDouble absTime)

-------------------------------------------------------------------------------------
-- ***** Graphics Setup

-- | Calculates the recommended texture size for rendering a given eye within the HMD
-- with a given FOV cone. Higher FOV will generally require larger textures to 
-- maintain quality.
--  - pixelsPerDisplayPixel specifies the ratio of the number of render target pixels 
--    to display pixels at the center of distortion. 1.0 is the default value. Lower
--    values can improve performance.
--
ovrHmd_GetFovTextureSize :: OvrHmd -> OvrEyeType -> Ptr OvrFovPort
                         -> Float -> IO OvrSizei
ovrHmd_GetFovTextureSize hmd (OvrEyeType et) fovp pixelsPerDisplayPixel =
  peek =<<
    c_ovrHmd_GetFovTextureSize hmd et fovp (CFloat pixelsPerDisplayPixel)

ovrHmd_GetDefaultFovTextureSize :: OvrHmd -> OvrEyeType
                         -> Float -> IO OvrSizei
ovrHmd_GetDefaultFovTextureSize hmd (OvrEyeType et)
                                pixelsPerDisplayPixel = do
  peek =<<
    c_ovrHmd_GetFovTextureSize hmd et fovp (CFloat pixelsPerDisplayPixel)
  where
    fovp = getDefaultEyeFovPtr hmd (OvrEyeType et)

ovrHmd_GetDefaultFov :: OvrHmd -> OvrEyeType -> IO OvrFovPort
ovrHmd_GetDefaultFov hmd et = peek fovp
  where fovp = getDefaultEyeFovPtr hmd et

-------------------------------------------------------------------------------------
-- | *****  SDK Distortion Rendering Functions

-- These functions support rendering of distortion by the SDK through direct
-- access to the underlying rendering API, such as D3D or GL.
-- This is the recommended approach since it allows better support for future
-- Oculus hardware, and enables a range of low-level optimizations.


-- Configures rendering and fills in computed render parameters.
-- This function can be called multiple times to change rendering settings.
-- eyeRenderDescOut is a pointer to an array of two ovrEyeRenderDesc structs
-- that are used to return complete rendering information for each eye.
--
--  - apiConfig provides D3D/OpenGL specific parameters. Pass null
--    to shutdown rendering and release all resources.
--  - distortionCaps describe desired distortion settings.
--
ovrHmd_ConfigureRendering :: OvrHmd -> Maybe OvrRenderAPIConfig
                          -> OvrDistortionCaps -> [OvrFovPort]
                          -> IO (Bool,[OvrEyeRenderDesc])
ovrHmd_ConfigureRendering hmd (Just renderAPIconfig)
                              (OvrDistortionCaps distortionCaps') eyeFovIn = do
  alloca $ \ racPtr -> 
    alloca $ \ fovPtr -> do
      alloca $ \ rdPtr -> do
        poke racPtr renderAPIconfig
        pokeArray fovPtr eyeFovIn
        ret <- fmap ovrBool2Bool $
          c_ovrHmd_ConfigureRendering hmd racPtr (CUInt distortionCaps')
                                      fovPtr rdPtr
        eyeRenderDescOut <- if rdPtr /= nullPtr
          then peekArray 2 rdPtr 
          else return []
        return (ret,eyeRenderDescOut)
ovrHmd_ConfigureRendering hmd Nothing (OvrDistortionCaps distortionCaps')
                          eyeFovIn  = do
  alloca $ \ fovPtr ->
    alloca $ \ rdPtr -> do
      pokeArray fovPtr eyeFovIn
      ret <- fmap ovrBool2Bool $
         c_ovrHmd_ConfigureRendering hmd nullPtr (CUInt distortionCaps')
                                     fovPtr rdPtr
      eyeRenderDescOut <- if rdPtr /= nullPtr
        then peekArray 2 rdPtr 
        else return []
      return (ret,eyeRenderDescOut)

-- | Begins a frame, returning timing information.
-- This should be called at the beginning of the game rendering loop (on the render thread).
-- Pass 0 for the frame index if not using ovrHmd_GetFrameTiming.
--
ovrHmd_BeginFrame :: OvrHmd -> Word32 -> IO OvrFrameTiming
ovrHmd_BeginFrame hmd frameIndex = do
  peek =<<
    c_ovrHmd_BeginFrame hmd (CUInt frameIndex)

-- | Ends a frame, submitting the rendered textures to the frame buffer.
-- - RenderViewport within each eyeTexture can change per frame if necessary.
-- - 'renderPose' will typically be the value returned from ovrHmd_GetEyePose, 
--   but can be different if a different head pose was used for rendering.
-- - This may perform distortion and scaling internally, assuming is it not 
--   delegated to another thread. 
-- - Must be called on the same thread as BeginFrame.
-- - *** This Function will call Present/SwapBuffers and potentially wait for GPU Sync ***.
--
ovrHmd_EndFrame :: OvrHmd -> [OvrPosef] -> [OvrTexture] -> IO ()
ovrHmd_EndFrame hmd renderPose eyeTexture = do
  alloca $ \ rp ->
    alloca $ \ et -> do
      pokeArray rp renderPose
      pokeArray et eyeTexture
      c_ovrHmd_EndFrame hmd rp et

-- |
-- Returns predicted head pose in outHmdTrackingState and offset eye poses in outEyePoses
-- as an atomic operation. Caller need not worry about applying HmdToEyeViewOffset to the
-- returned outEyePoses variables.
-- - Thread-safe function where caller should increment frameIndex with every frame
--   and pass the index where applicable to functions called on the  rendering thread.
-- - hmdToEyeViewOffset[2] can be ovrEyeRenderDesc.HmdToEyeViewOffset returned from 
--   ovrHmd_ConfigureRendering or ovrHmd_GetRenderDesc. For monoscopic rendering,
--   use a vector that is the average of the two vectors for both eyes.
-- - If frameIndex is not being used, pass in 0.
-- - Assuming outEyePoses are used for rendering, it should be passed into ovrHmd_EndFrame.
-- - If called doesn't need outHmdTrackingState, it can be NULL
--
ovrHmd_GetEyePoses  :: OvrHmd -> Word32 -> [OvrVector3f]
                    -> IO [OvrPosef]
ovrHmd_GetEyePoses hmd frameIndex hmdToEyeViewOffset' = 
  alloca $ \ viewOffset -> do
    pokeArray viewOffset hmdToEyeViewOffset'
    peekArray 2 =<<
      c_ovrHmd_GetEyePoses hmd (CUInt frameIndex) viewOffset nullPtr

ovrHmd_GetEyePosesWithTrackingState :: OvrHmd -> Word32 -> [OvrVector3f]
                    -> IO (OvrTrackingState, [OvrPosef])
ovrHmd_GetEyePosesWithTrackingState hmd frameIndex hmdToEyeViewOffset' = 
  alloca $ \ viewOffset ->
  alloca $ \ st -> do
    pokeArray viewOffset hmdToEyeViewOffset'
    po <- c_ovrHmd_GetEyePoses hmd (CUInt frameIndex) viewOffset st
    status <- peek st
    pose <- peekArray 2 po
    return (status,pose)

-- | Returns the predicted head pose to use when rendering the specified eye.
-- - Must be called between ovrHmd_BeginFrameTiming and ovrHmd_EndFrameTiming.
-- - If the pose is used for rendering the eye, it should be passed to ovrHmd_EndFrame.
--
ovrHmd_GetHmdPosePerEye :: OvrHmd -> OvrEyeType -> IO OvrPosef
ovrHmd_GetHmdPosePerEye hmd (OvrEyeType et) = do
  peek =<< c_ovrHmd_GetHmdPosePerEye hmd et 

-------------------------------------------------------------------------------------
-- *****  Client Distortion Rendering Functions
--
-- These functions provide the distortion data and render timing support necessary to allow
-- client rendering of distortion. Client-side rendering involves the following steps:
--
--  1. Setup ovrEyeDesc based on the desired texture size and FOV.
--     Call ovrHmd_GetRenderDesc to get the necessary rendering parameters for each eye.
-- 
--  2. Use ovrHmd_CreateDistortionMesh to generate the distortion mesh.
--
--  3. Use ovrHmd_BeginFrameTiming, ovrHmd_GetEyePose, and ovrHmd_BeginFrameTiming
--     in the rendering loop to obtain timing and predicted head orientation when
--     rendering each eye.
--      - When using timewarp, use ovr_WaitTillTime after the rendering and gpu flush, followed
--        by ovrHmd_GetEyeTimewarpMatrices to obtain the timewarp matrices used 
--        by the distortion pixel shader. This will minimize latency.
--

-- | Computes the distortion viewport, view adjust, and other rendering parameters for 
-- the specified eye. This can be used instead of ovrHmd_ConfigureRendering to do 
-- setup for client rendered distortion.
--
--foreign import ccall unsafe "_ovrHmd_GetRenderDesc" c_ovrHmd_GetRenderDesc :: OvrHmd -> OvrEyeType -> Ptr OvrPosef -> IO (Ptr OvrEyeRenderDesc)

-- | Generate distortion mesh per eye.
-- Distortion capabilities will depend on 'distortionCaps' flags. Users should 
-- render using the appropriate shaders based on their settings.
-- Distortion mesh data will be allocated and written into the ovrDistortionMesh data structure,
-- which should be explicitly freed with ovrHmd_DestroyDistortionMesh.
-- Users should call ovrHmd_GetRenderScaleAndOffset to get uvScale and Offset values for rendering.
-- The function shouldn't fail unless theres is a configuration or memory error, in which case
-- ovrDistortionMesh values will be set to null.
-- This is the only function in the SDK reliant on eye relief, currently imported from profiles, 
-- or overriden here.
--
{-
ovrHmd_CreateDistortionMesh :: OvrHmd -> OvrEyeType
                            -> Ptr OvrPosef -> CUInt
                            -> Ptr OvrDistortionMesh -> IO OvrBool
ovrHmd_CreateDistortionMesh hmd eyeType pOvrPose 
  c_ovrHmd_CreateDistortionMesh
-}

-- | Used to free the distortion mesh allocated by ovrHmd_GenerateDistortionMesh. meshData elements
-- are set to null and zeroes after the call.
--
--foreign import ccall unsafe "_ovrHmd_DestroyDistortionMesh" c_ovrHmd_DestroyDistortionMesh :: Ptr OvrDistortionMesh -> IO ()

-- | Computes updated 'uvScaleOffsetOut' to be used with a distortion if render target size or
-- viewport changes after the fact. This can be used to adjust render size every frame if desired.
--
--foreign import ccall unsafe "_ovrHmd_GetRenderScaleAndOffset" c_ovrHmd_GetRenderScaleAndOffset :: Ptr OvrFovPort -> Ptr OvrSizei -> Ptr OvrSizei -> Ptr OvrVector2f -> IO ()

-- | Thread-safe timing function for the main thread. Caller should increment frameIndex
-- with every frame and pass the index where applicable to functions called on the 
-- rendering thread.
--
--foreign import ccall unsafe "_ovrHmd_GetFrameTiming" c_ovrHmd_GetFrameTiming :: OvrHmd -> CUInt -> IO (Ptr OvrFrameTiming)

-- | Called at the beginning of the frame on the rendering thread.
-- Pass frameIndex == 0 if ovrHmd_GetFrameTiming isn't being used. Otherwise,
-- pass the same frame index as was used for GetFrameTiming on the main thread.
--
--foreign import ccall unsafe "_ovrHmd_BeginFrameTiming" c_ovrHmd_BeginFrameTiming :: OvrHmd -> CUInt -> IO (Ptr OvrFrameTiming)

-- | Marks the end of client distortion rendered frame, tracking the necessary timing information.
-- This function must be called immediately after Present/SwapBuffers + GPU sync. GPU sync is
-- important before this call to reduce latency and ensure proper timing.
--
--foreign import ccall unsafe "_ovrHmd_EndFrameTiming" c_ovrHmd_EndFrameTiming :: OvrHmd -> IO ()

-- | Initializes and resets frame time tracking. This is typically not necessary, but
-- is helpful if game changes vsync state or video mode. vsync is assumed to be on if this
-- isn't called. Resets internal frame index to the specified number.
--
--foreign import ccall unsafe "_ovrHmd_ResetFrameTiming" c_ovrHmd_ResetFrameTiming :: OvrHmd -> CUInt -> IO ()

-- | Computes timewarp matrices used by distortion mesh shader, these are used to adjust
-- for head orientation change since the last call to ovrHmd_GetEyePose when rendering
-- this eye. The ovrDistortionVertex::TimeWarpFactor is used to blend between the
-- matrices, usually representing two different sides of the screen.
-- Must be called on the same thread as ovrHmd_BeginFrameTiming.
--
--foreign import ccall unsafe "_ovrHmd_GetEyeTimewarpMatrices" c_ovrHmd_GetEyeTimewarpMatrices :: OvrHmd -> OvrEyeType -> Ptr OvrPosef -> Ptr OvrMatrix4f -> IO ()

-------------------------------------------------------------------------------------
-- ***** Stateless math setup functions

-- | Used to generate projection from ovrEyeDesc::Fov.
--
ovrMatrix4f_Projection :: OvrFovPort -> Float -> Float -> Bool
                       -> IO OvrMatrix4f
ovrMatrix4f_Projection fov' znear zfar rightHanded = peek =<< 
  (alloca $ \ f' -> do
    poke f' fov' 
    c_ovrMatrix4f_Projection f' (CFloat znear) (CFloat zfar)
                                (bool2OvrBool rightHanded))

-- | Used for 2D rendering, Y is down
-- orthoScale = 1.0f / pixelsPerTanAngleAtCenter
-- orthoDistance = distance from camera, such as 0.8m
--
ovrMatrix4f_OrthoSubProjection :: OvrMatrix4f -> OvrVector2f
                               -> Float -> Float -> IO OvrMatrix4f
ovrMatrix4f_OrthoSubProjection projection orthoScale orthoDistance
                               eyeViewAdjustX = peek =<<
  (alloca $ \ pro' ->
    alloca $ \ sc' -> do
      poke pro' projection
      poke sc' orthoScale 
      c_ovrMatrix4f_OrthoSubProjection pro' sc' (CFloat orthoDistance)
                                       (CFloat eyeViewAdjustX))

-- | Returns global, absolute high-resolution time in seconds.
-- This is the same
-- value as used in sensor messages.
ovr_GetTimeInSeconds :: IO Double
ovr_GetTimeInSeconds = fmap realToFrac c_ovr_GetTimeInSeconds

-- | Waits until the specified absolute time.
--
ovr_WaitTillTime :: Double -> IO Double
ovr_WaitTillTime = (fmap realToFrac) . (c_ovr_WaitTillTime . realToFrac)

-----------------------------------------------------------------------------------
-- ***** Latency Test interface

-- | Does latency test processing and returns 'TRUE' if specified rgb color should
-- be used to clear the screen.
--
--foreign import ccall unsafe "_ovrHmd_ProcessLatencyTest" c_ovrHmd_ProcessLatencyTest :: OvrHmd -> Ptr CUChar -> IO OvrBool

-- | Returns non-null string once with latency test result, when it is available.
-- Buffer is valid until next call.
--
--foreign import ccall unsafe "_ovrHmd_GetLatencyTestResult" c_ovrHmd_GetLatencyTestResult :: OvrHmd -> IO CString

-----------------------------------------------------------------------------------
-- ***** Health and Safety Warning Display interface

-- | Returns the current state of the HSW display. If the application is doing the rendering of
-- the HSW display then this function serves to indicate that the the warning should be 
-- currently displayed. If the application is using SDK-based eye rendering then the SDK by 
-- default automatically handles the drawing of the HSW display. An application that uses 
-- application-based eye rendering should use this function to know when to start drawing the
-- HSW display itself and can optionally use it in conjunction with ovrhmd_DismissHSWDisplay
-- as described below.
--
-- Example usage for application-based rendering:
--    bool HSWDisplayCurrentlyDisplayed = false; // global or class member variable
--    ovrHSWDisplayState hswDisplayState;
--    ovrhmd_GetHSWDisplayState(Hmd, &hswDisplayState);
--
--    if (hswDisplayState.Displayed && !HSWDisplayCurrentlyDisplayed) {
--        <insert model into the scene that stays in front of the user>
--        HSWDisplayCurrentlyDisplayed = true;
--    }
--
ovrHmd_GetHSWDisplayState :: OvrHmd -> IO OvrHSWDisplayState
ovrHmd_GetHSWDisplayState hmd = 
  peek =<< c_ovrHmd_GetHSWDisplayState hmd

-- | Dismisses the HSW display if the warning is dismissible and the earliest dismissal time 
-- has occurred. Returns true if the display is valid and could be dismissed. The application 
-- should recognize that the HSW display is being displayed (via ovrhmd_GetHSWDisplayState)
-- and if so then call this function when the appropriate user input to dismiss the warning
-- occurs.
--
-- Example usage :
--    void ProcessEvent(int key) {
--        if(key == escape) {
--            ovrHSWDisplayState hswDisplayState;
--            ovrhmd_GetHSWDisplayState(hmd, &hswDisplayState);
--
--            if(hswDisplayState.Displayed && ovrhmd_DismissHSWDisplay(hmd)) {
--                <remove model from the scene>
--                HSWDisplayCurrentlyDisplayed = false;
--            }
--        }
--    }
--
ovrHmd_DismissHSWDisplay :: OvrHmd -> IO Bool 
ovrHmd_DismissHSWDisplay hmd =
  fmap ovrBool2Bool $ c_ovrHmd_DismissHSWDisplay hmd

-----------------------------------------------------------------------------------
-- ***** Property Access

-- | Get boolean property. Returns first element if property is a boolean array.
-- | Returns defaultValue if property doesn't exist.
--
--foreign import ccall unsafe "_ovrHmd_GetBool" c_ovrHmd_GetBool :: OvrHmd -> CString -> OvrBool -> IO OvrBool 

-- | Modify bool property; false if property doesn't exist or is readonly.
--
--foreign import ccall unsafe "_ovrHmd_SetBool" c_ovrHmd_SetBool :: OvrHmd -> CString -> OvrBool -> IO OvrBool 

-- | Get integer property. Returns first element if property is an integer array.
-- Returns defaultValue if property doesn't exist.
--foreign import ccall unsafe "_ovrHmd_GetInt" c_ovrHmd_GetInt :: OvrHmd -> CString -> CInt -> IO CInt 

-- | Modify integer property; false if property doesn't exist or is readonly.
--
--foreign import ccall unsafe "_ovrHmd_SetInt" c_ovrHmd_SetInt :: OvrHmd -> CString -> CInt -> IO OvrBool 

-- | Get float property. Returns first element if property is a float array.
-- Returns defaultValue if property doesn't exist.
--
--foreign import ccall unsafe "_ovrHmd_GetFloat" c_ovrHmd_GetFloat :: OvrHmd -> CString -> CFloat -> IO CFloat 

-- | Modify float property; false if property doesn't exist or is readonly.
--
--foreign import ccall unsafe "_ovrHmd_SetFloat" c_ovrHmd_SetFloat :: OvrHmd -> CString -> CFloat -> IO OvrBool 

-- | Get float[] property. Returns the number of elements filled in, 0 if property doesn't exist.
-- Maximum of arraySize elements will be written.
--
--foreign import ccall unsafe "_ovrHmd_GetFloatArray" c_ovrHmd_GetFloatArray :: OvrHmd -> CString -> Ptr CFloat -> CUInt -> IO CUInt 

-- | Modify float[] property; false if property doesn't exist or is readonly.
--
--foreign import ccall unsafe "_ovrHmd_SetFloatArray" c_ovrHmd_SetFloatArray :: OvrHmd -> CString -> Ptr CFloat -> CUInt -> IO OvrBool 

-- | Get string property. Returns first element if property is a string array.
-- Returns defaultValue if property doesn't exist.
-- String memory is guaranteed to exist until next call to GetString or GetStringArray, or HMD is destroyed.
--
--foreign import ccall unsafe "_ovrHmd_GetString" c_ovrHmd_GetString :: OvrHmd -> CString -> CString -> IO CString 

-- | Set string property
--
--foreign import ccall unsafe "_ovrHmd_SetString" c_ovrHmd_SetString :: OvrHmd -> CString -> CString -> IO OvrBool 

