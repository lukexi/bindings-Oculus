--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main where

import Bindings.OculusRift
import Bindings.OculusRift.Types

import Control.Exception ( bracket )
import Debug.Trace ( traceIO )
import Foreign.C.String ( peekCString )
import Foreign.Storable ( peek ) 

import Data.Maybe ( isJust,fromJust )
import Data.Bits 
import Control.Monad ( forM_, forM )
import Control.Concurrent (threadDelay)

import GLFWWindow
import GLView
import Bindings.OculusRift.Types 

import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Foreign.Ptr (nullPtr)
import Linear.V4
import Control.Applicative

-------------
#if defined(mingw32_HOST_OS)
import Bindings.Utils.Windows
#endif
-------------

winSize = (1920,1080)

main :: IO ()
main = bracket
  (do
    !b <- ovr_Initialize
    !ghmd <- initGLFW winSize "oculus test" False
    return (b,ghmd))
  (\ (_,ghmd) -> do
            ovr_Shutdown 
            exitGLFW ghmd
            traceIO "exit")
  (\ (b,ghmd) -> if b 
    then do
      traceIO "init OK"
      bracket
        (do
          !maxIdx <- ovrHmd_Detect
          traceIO $ "detect = " ++ show maxIdx
          !hmd <- if maxIdx > 0 
            then ovrHmd_Create (maxIdx - 1)
            else Just <$> ovrHmd_CreateDebug ovrHmd_DK2
          return hmd)
        (\ hmd' -> if isJust hmd' 
            then do
              ovrHmd_Destroy $ fromJust hmd'
              traceIO "destroy hmd"
            else do
              traceIO "hmd is Null"
              return ())
        (mainProcess ghmd)
    else traceIO "init NG")

mainProcess _ Nothing = traceIO "create hmd NG"
mainProcess ghmd hmd' = do
  !glhdl <- initGL
  let hmd = fromJust hmd'
  traceIO $ "create hmd OK : " ++ show hmd
  !msg <- ovrHmd_GetLastError hmd
  traceIO $ "GetLastError = " ++ msg ++ " Msg End"
  traceIO " == Print HmdDesc =="
  hmdDesc <- castToOvrHmdDesc hmd
  printHmdDesc hmdDesc
  traceIO " ==================="
  !r <- ovrHmd_ConfigureTracking hmd
               ( ovrTrackingCap_Orientation  
               .|. ovrTrackingCap_MagYawCorrection
               .|. ovrTrackingCap_Position)
               ovrTrackingCap_None
  traceIO $ "ConfigureTracking : " ++ show r
  -- 
#if defined(mingw32_HOST_OS)
  !hwnd <- getWindowHandle "oculus test" 
  traceIO $ "windowHandle : " ++ show hwnd
  -- !hdc <- getWinDC hwnd
  !ba <- ovrHmd_AttachToWindow hmd hwnd
                               Nothing Nothing 
  let nativeWindow = Just hwnd
  traceIO $ "AttachToWindow : " ++ show (ba,hwnd)
#else
  let nativeWindow = Nothing
#endif
  
  recommenedTex0Size <- ovrHmd_GetDefaultFovTextureSize
                          hmd ovrEye_Left 1.0
  recommenedTex1Size <- ovrHmd_GetDefaultFovTextureSize
                          hmd ovrEye_Right 1.0
  traceIO $ "recommentedTexSize L : "
    ++ show recommenedTex0Size
    ++ " R : "
    ++ show recommenedTex1Size
  let !renderTargetSizeW = (si_w recommenedTex0Size)
                        + (si_w recommenedTex1Size)
      !renderTargetSizeH = max (si_h recommenedTex0Size)
                              (si_h recommenedTex1Size)
      twidth = fromIntegral renderTargetSizeW
      theight = fromIntegral renderTargetSizeH
  !tex <- genColorTexture 0 twidth theight
  !fbo <- genColorFrameBuffer tex twidth theight 
  --
  let !eyeTexture = genEyeTextureData tex renderTargetSizeW
                                          renderTargetSizeH
      !hd = OvrRenderAPIConfigHeader
                   ovrRenderAPI_OpenGL
                   (resolution hmdDesc) 
                   0 --  1
      !apiconf = OvrRenderAPIConfig hd nativeWindow Nothing --  (Just hdc)
      !caps =
                 ovrDistortionCap_Vignette
         --    .|. ovrDistortionCap_SRGB
             .|. ovrDistortionCap_Overdrive 
             .|. ovrDistortionCap_TimeWarp
         --    .|. ovrDistortionCap_ProfileNoSpinWaits 
             .|. ovrDistortionCap_HqDistortion
         --    .|. ovrDistortionCap_ComputeShader
         --
         --    .|. ovrDistortionCap_NoRestore
         --    .|. ovrDistortionCap_FlipInput 
  traceIO $ "OvrEyeTexture : " ++ show eyeTexture
  traceIO $ "OvrRenderAPIConfigHeader : " ++ show hd
  traceIO $ "render caps : " ++ show caps
  !lfv <- ovrHmd_GetDefaultFov hmd ovrEye_Left
  !rfv <- ovrHmd_GetDefaultFov hmd ovrEye_Right
  !(bret, eyeRD) <- ovrHmd_ConfigureRendering hmd
                    (Just apiconf) caps [lfv,rfv]
  traceIO $ "ConfigureRendering : " ++ show (bret,eyeRD)
  --
  ovrHmd_SetEnabledCaps hmd ( 
   --     ovrHmdCap_Present          
   -- .|. ovrHmdCap_Available
   -- .|. ovrHmdCap_Captured
   --  ovrHmdCap_ExtendDesktop 
   -- .|. ovrHmdCap_DisplayOff
        ovrHmdCap_LowPersistence
    .|. ovrHmdCap_DynamicPrediction
   -- .|. ovrHmdCap_NoMirrorToWindow
   -- .|. ovrHmdCap_NoVSync          
    )
  -- !tis <- ovr_GetTimeInSeconds
  -- traceIO $ "GetTimeInSeconds : " ++ (show tis)

  msg2 <- ovrHmd_GetLastError hmd
  traceIO $ "GetLastError 2 = " ++ msg2 ++ " Msg End"
  printError 
  ovrHmd_RecenterPose hmd
  tex <- loadTextureObj "test/sample_tex.png"
  mainLoop hmd ghmd glhdl (eyeTexture,tex,fbo) eyeRD tex 0
  --
  ovrHmd_ConfigureRendering hmd Nothing caps [lfv,rfv]
  return ()
  where


genColorTexture textureUnitNo width height = do
  tex <- genObjectName 
  withTexturesAt Texture2D [(tex,textureUnitNo)] $ do
    texImage2D Texture2D NoProxy 0 RGBA'
             (TextureSize2D width height) 0
             (PixelData RGBA UnsignedByte nullPtr) 
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture2DWrap $= (Repeated, ClampToEdge)
    --textureBorderColor Texture2D $= Color4 1.0 0.0 0.0 (0.0::GLfloat)
    --textureMaxAnisotropy Texture2D $= 1.0
  return tex

genColorFrameBuffer tex width height = do
  traceIO $ "tex size = " ++ (show (width,height))
  !fbo <- genObjectName :: IO FramebufferObject
  bindFramebuffer Framebuffer $= fbo

  !rbo <- genObjectName :: IO RenderbufferObject 
  bindRenderbuffer Renderbuffer $= rbo
  renderbufferStorage Renderbuffer DepthComponent'
                      (RenderbufferSize width height)

  framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rbo
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0
 
  drawBuffers $= [FBOColorAttachment 0] 

  -- unbind
  bindRenderbuffer Renderbuffer $= noRenderbufferObject 
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  return fbo

genEyeTextureData tex width height = 
  [ OvrTexture hd0 texID , OvrTexture hd1 texID ]
  where
    texID = (\ (TextureObject t') -> t' ) tex
    vpSize = OvrSizei (div width 2) height
    hd0 = OvrTextureHeader
             { apiT = ovrRenderAPI_OpenGL
             , textureSize = OvrSizei width height
             , renderViewport = OvrRecti (OvrVector2i 0 0) vpSize
             }
    hd1 = OvrTextureHeader
             { apiT = ovrRenderAPI_OpenGL
             , textureSize = OvrSizei width height
             , renderViewport = OvrRecti (OvrVector2i (div width 2) 0) vpSize
             }


mainLoop hmd glfwHdl glhdl (eyeTexture,texobj,fbo) eyeRD tex frameNo = do
  pollGLFW
  --threadDelay   10000
  --threadDelay 1000
  --threadDelay 1000000
  dt <- getDeltTime glfwHdl
  exitflg' <- getExitReqGLFW glfwHdl

  --ts <- ovrHmd_GetTrackingState hmd =<< ovr_GetTimeInSeconds
  --traceIO $ show ts
  ovrHmd_BeginFrame hmd frameNo
  bindFramebuffer Framebuffer $= fbo
  let (winW,winH) = winSize
  withViewport (Position 0 0) (Size (fromIntegral winW) (fromIntegral winH)) $
    clear [GL.ColorBuffer, GL.DepthBuffer]
  (poseL:poseR:_) <- ovrHmd_GetEyePoses hmd frameNo $ map hmdToEyeViewOffset eyeRD 
  renderPose <- forM [(ovrEye_Left,0,poseL),(ovrEye_Right,1,poseR)]
                  $ \ (eyeType,i,pose) -> do
    --pose <- ovrHmd_GetHmdPosePerEye hmd eyeType
    (OvrMatrix4f m) <- ovrMatrix4f_Projection (fov (eyeRD !! i)) 0.1 20 True
    let pm = (\ [v1,v2,v3,v4] -> V4 v1 v2 v3 v4) $
                 map (\ l -> (\ [a,b,c,d] -> V4 a b c d) $ map realToFrac l ) m
        (OvrQuatf qx qy qz qw) = orientation pose
    --traceIO $ "pose : " ++ (show eyeType) ++ " : " ++ (show pose)
    --textureBinding Texture2D $= Just texobj
    let fov' = fov $ head eyeRD 
        vPos = if eyeType == ovrEye_Left
                then Position 0 0
                else Position 1182 0
    withViewport vPos (Size 1182 1461) $ render glhdl tex pm (qx,qy,qz,qw)
    flush
    return pose
  bindFramebuffer Framebuffer $= defaultFramebufferObject      
  --traceIO $ "renderPose = " ++ (show renderPose) 
  --traceIO $ "eyeTexture = " ++ (show eyeTexture) 
  ovrHmd_EndFrame hmd renderPose eyeTexture
  --swapBuff glfwHdl
  --msg <- ovrHmd_GetLastError hmd
  --traceIO $ "GetLastError 3 = " ++ msg ++ " Msg End"
  --printError
  if exitflg' 
    then return ()
    else mainLoop hmd glfwHdl glhdl (eyeTexture,texobj,fbo) eyeRD tex (frameNo + 1)


