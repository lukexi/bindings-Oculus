{-# LANGUAGE BangPatterns #-}

module Main where

import Bindings.OculusRift
import Bindings.OculusRift.Types

import Control.Exception ( bracket )
import Debug.Trace ( traceIO )

import Data.Maybe ( isJust,fromJust )
import Data.Bits 
import Control.Concurrent (threadDelay)

import GLFWWindow
import GLView

import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Foreign.Ptr (nullPtr)
import Control.Applicative
import Data.Traversable

main :: IO ()
main = bracket
  (do
    -- !b1 <- ovr_InitializeRenderingShim
    !b2 <- ovr_Initialize
    !ghmd <- initGLFW (1920,1080) "oculus test" False
    return (b2,ghmd))
  (\ (_,ghmd) -> do
            ovr_Shutdown 
            exitGLFW ghmd
            traceIO "exit")
  (\ (b,ghmd) -> if b == True
    then do
      traceIO "init OK"
      bracket
        (do
          !maxIdx <- ovrHmd_Detect
          -- traceIO $ "detect = " ++ (show maxIdx)
          !hmd <- if maxIdx > 0 
            then ovrHmd_Create (maxIdx - 1)
            else Just <$> ovrHmd_CreateDebug ovrHmd_DK2
          return hmd)
        (\ hmd' -> do
          if isJust hmd' 
            then do
              ovrHmd_Destroy $ fromJust hmd'
              traceIO "destroy hmd"
            else do
              traceIO "hmd is Null"
              return ())
        (mainProcess ghmd)
    else traceIO "init NG")

mainProcess :: GLFWHandle -> Maybe OvrHmd -> IO ()
mainProcess _ Nothing = traceIO "create hmd NG"
mainProcess ghmd hmd' = do
  !glhdl <- initGL
  let hmd = fromJust hmd'
  traceIO $ "create hmd OK : " ++ (show hmd)
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
  traceIO $ "ConfigureTracking : " ++ (show r)
  -- 
  -- !hwnd <- getWindowHandle 
  --  =<< fmap fromJust (getWindowHdl ghmd)
  -- !hdc <- getWinDC hwnd
  -- !ba <- ovrHmd_AttachToWindow hmd hwnd
  --                            Nothing Nothing 
  --traceIO $ "AttachToWindow : " ++ (show (ba,hwnd))
  
  recommenedTex0Size <- ovrHmd_GetDefaultFovTextureSize
                          hmd ovrEye_Left 1.0
  recommenedTex1Size <- ovrHmd_GetDefaultFovTextureSize
                          hmd ovrEye_Right 1.0
  traceIO $ "recommentedTexSize L : "
    ++ (show recommenedTex0Size)
    ++ " R : "
    ++ (show recommenedTex1Size)
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
      !apiconf = OvrRenderAPIConfig hd Nothing Nothing -- (Just hwnd) (Just hdc)
      !caps =    ovrDistortionCap_TimeWarp
             .|. ovrDistortionCap_Vignette
             .|. ovrDistortionCap_NoRestore
         --    .|. ovrDistortionCap_FlipInput 
             .|. ovrDistortionCap_SRGB
             .|. ovrDistortionCap_Overdrive 
             .|. ovrDistortionCap_HqDistortion
             .|. ovrDistortionCap_ProfileNoSpinWaits 
  traceIO $ "OvrEyeTexture : " ++ (show eyeTexture)
  traceIO $ "OvrRenderAPIConfigHeader : " ++ (show hd)
  traceIO $ "render caps : " ++ (show caps)
  !lfv <- ovrHmd_GetDefaultFov hmd ovrEye_Left
  !rfv <- ovrHmd_GetDefaultFov hmd ovrEye_Right
  !(bret, eyeRD) <- ovrHmd_ConfigureRendering hmd
                    (Just apiconf) caps [lfv,rfv]
  traceIO $ "ConfigureRendering : " ++ (show (bret,eyeRD))
  --
  ovrHmd_SetEnabledCaps hmd ( 
   --     ovrHmdCap_Present          
   -- .|. ovrHmdCap_Available
   -- .|. ovrHmdCap_Captured
     ovrHmdCap_ExtendDesktop 
   -- .|. ovrHmdCap_NoMirrorToWindow
   -- .|. ovrHmdCap_DisplayOff
    .|. ovrHmdCap_LowPersistence
    .|. ovrHmdCap_DynamicPrediction
   -- .|. ovrHmdCap_NoVSync          
    )
  !tis <- ovr_GetTimeInSeconds
  traceIO $ "GetTimeInSeconds : " ++ (show tis)

  msg2 <- ovrHmd_GetLastError hmd
  traceIO $ "GetLastError 2 = " ++ msg2 ++ " Msg End"
  printError 
  ovrHmd_RecenterPose hmd
  mainLoop hmd ghmd glhdl (eyeTexture,tex,fbo) eyeRD 0
  --
  (_success, _ovrEyeRenderDescs) <- ovrHmd_ConfigureRendering hmd Nothing caps [lfv,rfv]
  return ()
  where

genColorTexture :: GLuint -> GLsizei -> GLsizei -> IO TextureObject
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

genColorFrameBuffer :: TextureObject -> GLsizei -> GLsizei -> IO FramebufferObject
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

genEyeTextureData :: TextureObject -> Int -> Int -> [OvrTexture]
genEyeTextureData tex width height = 
  [ OvrTexture hd0 textureID , OvrTexture hd1 textureID ]
  where
    textureID = (\ (TextureObject t') -> t' ) tex
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


mainLoop :: OvrHmd
         -> GLFWHandle
         -> (ShaderProgram, VertexArrayObject, VertexArrayObject)
         -> ([OvrTexture], t, FramebufferObject)
         -> [OvrEyeRenderDesc]
         -> Word32
         -> IO ()
mainLoop hmd glfwHdl glhdl (eyeTexture,texobj,fbo) eyeRD frameNo = do
  pollGLFW
  threadDelay 1000
  --threadDelay 1000000
  _dt <- getDeltTime glfwHdl
  exitflg' <- getExitReqGLFW glfwHdl

  --ts <- ovrHmd_GetTrackingState hmd =<< ovr_GetTimeInSeconds
  --traceIO $ show ts
  _frameTiming <- ovrHmd_BeginFrame hmd frameNo
  bindFramebuffer Framebuffer $= fbo
  viewport $= (Position 0 0, Size 1920 1080)
  clear [GL.ColorBuffer, GL.DepthBuffer]
  renderPose <- forM [ovrEye_Left,ovrEye_Right]
                  $ \ eyeType -> do
    pose <- ovrHmd_GetHmdPosePerEye hmd eyeType
    matrixMode $= Projection
    loadIdentity
    --pm' <- ovrMatrix4f_Projection fov' 0.0001 1 True
    --pm <- newMatrix ColumnMajor  
    --multMatrix pm

    matrixMode $= Modelview 0
    --traceIO $ "pose : " ++ (show eyeType) ++ " : " ++ (show pose)
    --textureBinding Texture2D $= Just texobj
    let _fov' = fov $ head eyeRD 
        vPos = if eyeType == ovrEye_Left
                then Position 0 0
                else Position 1182 0
    withViewport vPos (Size 1182 1461) $ render glhdl 
    return pose
  flush
  bindFramebuffer Framebuffer $= defaultFramebufferObject      
  --traceIO $ "renderPose = " ++ (show renderPose) 
  --traceIO $ "eyeTexture = " ++ (show eyeTexture) 
  withViewport (Position 0 0) (Size 1920 1080) $
    ovrHmd_EndFrame hmd renderPose eyeTexture
  --swapBuff glfwHdl
  --msg <- ovrHmd_GetLastError hmd
  --traceIO $ "GetLastError 3 = " ++ msg ++ " Msg End"
  --printError
  if exitflg' 
    then return ()
    else mainLoop hmd glfwHdl glhdl (eyeTexture,texobj,fbo) eyeRD (frameNo + 1)


