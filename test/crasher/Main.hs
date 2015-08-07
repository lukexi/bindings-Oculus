module Main where

import Bindings.OculusRift
import Bindings.OculusRift.Types

import Debug.Trace ( traceIO )

import Data.Bits 

import Graphics.Rendering.OpenGL as GL hiding (position)
import Graphics.GLUtil
import Foreign.Ptr (nullPtr)
import Data.Foldable

import Control.Monad (when, unless, forever)
import Control.Concurrent

import qualified Graphics.UI.GLFW as GLFW

{-
NOTES:

Both EndFrame and GetEyePoses will crash when called in isolation.
BeginFrame will not.

This reduces the chances that it is a 'latent' corruption introduced by earlier calls,
and means it's worth quintuple-checking all structs serialized into/outof EndFrame & GetEyePoses.

Regarding the flashing eyes, my idea is to first write a minimal C example linking with LibOVR and GLFW,
then translate that to the 'gl' package and see if we can figure out what's going on from there.

-}

main :: IO ()
main = do
  _       <- ovr_Initialize
  hmd     <- ovrHmd_CreateDebug ovrHmd_DK2
  hmdDesc <- castToOvrHmdDesc hmd
  let OvrSizei width height = resolution hmdDesc
  win     <- setupGLFW (fromIntegral width, fromIntegral height)

  clearColor $= Color4 1 1 0 1.0

  (fbo, eyeTextures, eyeViewOffsets) <- setupOculus hmd

  -- Spawn off some threads, as this seems to exacerbate the crash
  forM_ [0..1000] $ \i -> forkIO $ forever $ print i >> threadDelay ((i+1) * 10000)

  mainLoopNoCrash win hmd fbo eyeTextures eyeViewOffsets 0
  mainLoopMinimal win hmd fbo eyeTextures eyeViewOffsets 0
  mainLoopDontGetPoses win hmd fbo eyeTextures eyeViewOffsets 0
  mainLoopNormal win hmd fbo eyeTextures eyeViewOffsets 0

type MainLoop = GLFW.Window
              -> OvrHmd
              -> FramebufferObject
              -> [OvrTexture]
              -> [OvrVector3f]
              -> Word32
              -> IO ()

mainLoopNoCrash :: MainLoop
mainLoopNoCrash win hmd fbo eyeTextures eyeViewOffsets frameNo = do
  GLFW.pollEvents

  clear [GL.ColorBuffer, GL.DepthBuffer]
  GLFW.swapBuffers win
  
  mainLoopNoCrash win hmd fbo eyeTextures eyeViewOffsets (frameNo + 1)

mainLoopMinimal :: MainLoop
mainLoopMinimal win hmd fbo eyeTextures eyeViewOffsets frameNo = do
  GLFW.pollEvents

  _poses <- ovrHmd_GetEyePoses hmd frameNo eyeViewOffsets
  
  mainLoopMinimal win hmd fbo eyeTextures eyeViewOffsets (frameNo + 1)

mainLoopDontGetPoses :: MainLoop
mainLoopDontGetPoses win hmd fbo eyeTextures eyeViewOffsets frameNo = do
  GLFW.pollEvents

  _frameTiming <- ovrHmd_BeginFrame hmd frameNo
  bindFramebuffer Framebuffer $= fbo

  clear [GL.ColorBuffer, GL.DepthBuffer]

  let poses = [OvrPosef {orientation = OvrQuatf {qf_x = 0.0, qf_y = 0.0, qf_z = 0.0, qf_w = 1.0}, 
                position = OvrVector3f {v3f_x = -3.2e-2, v3f_y = 0.0, v3f_z = 0.0}},
              OvrPosef {orientation = OvrQuatf {qf_x = 0.0, qf_y = 0.0, qf_z = 0.0, qf_w = 1.0}, 
                position = OvrVector3f {v3f_x = 3.2e-2, v3f_y = 0.0, v3f_z = 0.0}}]
  
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  
  ovrHmd_EndFrame hmd poses eyeTextures

  -- printError
  mainLoopDontGetPoses win hmd fbo eyeTextures eyeViewOffsets (frameNo + 1)

mainLoopNormal :: MainLoop
mainLoopNormal win hmd fbo eyeTextures eyeViewOffsets frameNo = do
  GLFW.pollEvents

  _frameTiming <- ovrHmd_BeginFrame hmd frameNo
  bindFramebuffer Framebuffer $= fbo

  clear [GL.ColorBuffer, GL.DepthBuffer]

  poses <- ovrHmd_GetEyePoses hmd frameNo eyeViewOffsets
  -- -- print poses

  -- forM_ eyeTextures $ \(OvrTexture textureHeader _) -> do
  --   let OvrRecti (OvrVector2i x y) (OvrSizei w h) = renderViewport textureHeader
  --   viewport $= (Position (fromIntegral x) (fromIntegral y), Size (fromIntegral w) (fromIntegral h))
  --   -- Normally you'd render something here.
  
  bindFramebuffer Framebuffer $= defaultFramebufferObject

  ovrHmd_EndFrame hmd poses eyeTextures

  -- printError
  mainLoopNormal win hmd fbo eyeTextures eyeViewOffsets (frameNo + 1)



------------------
-- Setup Functions
------------------






setupGLFW :: (Int, Int) -> IO GLFW.Window
setupGLFW (desiredW, desiredH) = do
  _success <- GLFW.init
  
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

  let (halfW, halfH) = (desiredW `div` 2, desiredH `div` 2)
  Just win <- GLFW.createWindow desiredW desiredH "CrashTest" Nothing Nothing
  (frameW, frameH) <- GLFW.getFramebufferSize win
  -- Compensate for retina framebuffers on Mac
  when (frameW > desiredW && frameH > desiredH) $ GLFW.setWindowSize win halfW halfH

  GLFW.makeContextCurrent (Just win)

  GLFW.swapInterval 1
  return win



setupOculus :: OvrHmd -> IO (FramebufferObject, [OvrTexture], [OvrVector3f])
setupOculus hmd = do
  hmdDesc <- castToOvrHmdDesc hmd
  
  let trackingCaps =  ovrTrackingCap_Orientation  
                  .|. ovrTrackingCap_MagYawCorrection
                  .|. ovrTrackingCap_Position
  _r <- ovrHmd_ConfigureTracking hmd trackingCaps ovrTrackingCap_None
  
  (eyeTextures, fbo) <- genTextureAndFramebuffer hmd

  let configHeader  = OvrRenderAPIConfigHeader
                        ovrRenderAPI_OpenGL
                        (resolution hmdDesc) 
                        0 --  1 <- multisampling on/off
      apiConfig     = OvrRenderAPIConfig configHeader Nothing Nothing
      distortionCaps = ovrDistortionCap_TimeWarp
                   .|. ovrDistortionCap_Vignette
                   -- .|. ovrDistortionCap_TimewarpJitDelay
                   -- .|. ovrDistortionCap_SRGB
                   .|. ovrDistortionCap_Overdrive 
                   .|. ovrDistortionCap_HqDistortion
  
  lfv <- ovrHmd_GetDefaultFov hmd ovrEye_Left
  rfv <- ovrHmd_GetDefaultFov hmd ovrEye_Right
  (_result, eyeRD) <- ovrHmd_ConfigureRendering hmd
                      (Just apiConfig) distortionCaps [lfv,rfv]
  
  let hmdCaps = ovrHmdCap_ExtendDesktop 
             .|. ovrHmdCap_LowPersistence
             .|. ovrHmdCap_DynamicPrediction
  ovrHmd_SetEnabledCaps hmd hmdCaps

  lastError <- ovrHmd_GetLastError hmd
  unless (null lastError) $ traceIO $ "GetLastError: " ++ lastError
  printError
  ovrHmd_RecenterPose hmd

  return (fbo, eyeTextures, map hmdToEyeViewOffset eyeRD)

genTextureAndFramebuffer :: OvrHmd -> IO ([OvrTexture], FramebufferObject)
genTextureAndFramebuffer hmd = do
  recommenedTex0Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Left 1.0
  recommenedTex1Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Right 1.0
  let renderTargetSizeW = si_w recommenedTex0Size
                        + si_w recommenedTex1Size
      renderTargetSizeH = si_h recommenedTex0Size `max` 
                          si_h recommenedTex1Size
      renderTargetSizeWI = fromIntegral renderTargetSizeW
      renderTargetSizeHI = fromIntegral renderTargetSizeH
  eyeTextureObject <- genColorTexture 0 renderTargetSizeWI renderTargetSizeHI
  fbo              <- genColorFrameBuffer eyeTextureObject renderTargetSizeWI renderTargetSizeHI
  --
  let eyeTextures = genEyeTextureData eyeTextureObject renderTargetSizeW renderTargetSizeH
  return (eyeTextures, fbo)

genColorTexture :: GLuint -> GLsizei -> GLsizei -> IO TextureObject
genColorTexture textureUnitNo width height = do
  tex <- genObjectName 
  withTexturesAt Texture2D [(tex,textureUnitNo)] $ do
    texImage2D Texture2D NoProxy 0 RGBA'
              (TextureSize2D width height) 0
              (PixelData RGBA UnsignedByte nullPtr) 
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture2DWrap $= (Repeated, ClampToEdge)
  return tex

genColorFrameBuffer :: TextureObject -> GLsizei -> GLsizei -> IO FramebufferObject
genColorFrameBuffer tex width height = do
  fbo <- genObjectName :: IO FramebufferObject
  rbo <- genObjectName :: IO RenderbufferObject 

  
  -- Setup 
  do
    bindFramebuffer Framebuffer $= fbo
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    do
      bindRenderbuffer Renderbuffer $= rbo
      renderbufferStorage Renderbuffer DepthComponent'
                          (RenderbufferSize width height)
      bindRenderbuffer Renderbuffer $= noRenderbufferObject 


    framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rbo
    
    -- drawBuffers $= [FBOColorAttachment 0]

    status <- framebufferStatus Framebuffer
    when (status /= Complete) $ error ("Framebuffer not complete: " ++ show status)

    bindFramebuffer  Framebuffer  $= defaultFramebufferObject

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



