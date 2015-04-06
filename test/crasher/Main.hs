module Main where

import Bindings.OculusRift
import Bindings.OculusRift.Types

import Debug.Trace ( traceIO )

import Data.Bits 

import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Foreign.Ptr (nullPtr)
import Data.Foldable

import Control.Concurrent
import Control.Monad (forever)

import qualified Graphics.UI.GLFW as GLFW

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

main :: IO ()
main = do
  _ <- ovr_Initialize
  hmd <- ovrHmd_CreateDebug ovrHmd_DK2
  hmdDesc <- castToOvrHmdDesc hmd
  let OvrSizei width height = resolution hmdDesc
  win <- setupGLFW (fromIntegral width, fromIntegral height)


  -- startupTestThreads

  clearColor $= Color4 1 0 0 1.0

  setupOculus win hmd

startupTestThreads :: IO ()
startupTestThreads = forM_ [0..100] $ \i -> 
  forkIO . forever $ print (i::Int) >> threadDelay 100000

setupOculus :: GLFW.Window -> OvrHmd -> IO ()
setupOculus win hmd = do
  traceIO $ "create hmd OK : " ++ (show hmd)
  msg <- ovrHmd_GetLastError hmd
  traceIO $ "GetLastError = " ++ msg ++ " Msg End"
  -- traceIO " == Print HmdDesc =="
  hmdDesc <- castToOvrHmdDesc hmd
  -- printHmdDesc hmdDesc
  -- traceIO " ==================="
  _r <- ovrHmd_ConfigureTracking hmd
                  (ovrTrackingCap_Orientation  
               .|. ovrTrackingCap_MagYawCorrection
               .|. ovrTrackingCap_Position)
               ovrTrackingCap_None
  
  recommenedTex0Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Left 1.0
  recommenedTex1Size <- ovrHmd_GetDefaultFovTextureSize hmd ovrEye_Right 1.0
  let renderTargetSizeW = (si_w recommenedTex0Size)
                        + (si_w recommenedTex1Size)
      renderTargetSizeH = max (si_h recommenedTex0Size)
                              (si_h recommenedTex1Size)
      twidth = fromIntegral renderTargetSizeW
      theight = fromIntegral renderTargetSizeH
  tex <- genColorTexture 0 twidth theight
  fbo <- genColorFrameBuffer tex twidth theight 
  --
  let eyeTexture = genEyeTextureData tex renderTargetSizeW
                                          renderTargetSizeH
      hd = OvrRenderAPIConfigHeader
                   ovrRenderAPI_OpenGL
                   (resolution hmdDesc) 
                   0 --  1
      apiconf = OvrRenderAPIConfig hd Nothing Nothing
      caps =    ovrDistortionCap_TimeWarp
             .|. ovrDistortionCap_Vignette
             .|. ovrDistortionCap_NoRestore
             .|. ovrDistortionCap_SRGB
             .|. ovrDistortionCap_Overdrive 
             .|. ovrDistortionCap_HqDistortion
  -- traceIO $ "OvrEyeTexture : " ++ (show eyeTexture)
  
  lfv <- ovrHmd_GetDefaultFov hmd ovrEye_Left
  rfv <- ovrHmd_GetDefaultFov hmd ovrEye_Right
  (_bret, eyeRD) <- ovrHmd_ConfigureRendering hmd
                    (Just apiconf) caps [lfv,rfv]
  -- traceIO $ "ConfigureRendering : " ++ (show (bret,eyeRD))
  --
  ovrHmd_SetEnabledCaps hmd ( 
     ovrHmdCap_ExtendDesktop 
    .|. ovrHmdCap_LowPersistence
    .|. ovrHmdCap_DynamicPrediction
    )

  msg2 <- ovrHmd_GetLastError hmd
  traceIO $ "GetLastError 2 = " ++ msg2 ++ " Msg End"
  printError 
  ovrHmd_RecenterPose hmd
  mainLoop win hmd (eyeTexture,tex,fbo) eyeRD (twidth, theight) 0
  --
  (_success, _ovrEyeRenderDescs) <- ovrHmd_ConfigureRendering hmd Nothing caps [lfv,rfv]
  return ()

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
  bindFramebuffer Framebuffer $= fbo

  rbo <- genObjectName :: IO RenderbufferObject 
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


mainLoop :: GLFW.Window
         -> OvrHmd
         -> ([OvrTexture], t, FramebufferObject)
         -> [OvrEyeRenderDesc]
         -> (GLsizei, GLsizei)
         -> Word32
         -> IO ()
mainLoop win hmd (eyeTexture,texobj,fbo) eyeRD (twidth, theight) frameNo = do
  GLFW.pollEvents

  _frameTiming <- ovrHmd_BeginFrame hmd frameNo
  bindFramebuffer Framebuffer $= fbo

  (frameWI, frameHI) <- GLFW.getFramebufferSize win
  let (frameW, frameH) = (fromIntegral frameWI, fromIntegral frameHI)
  viewport $= (Position 0 0, Size frameW frameH)
  clear [GL.ColorBuffer, GL.DepthBuffer]

  poses <- ovrHmd_GetEyePoses hmd frameNo (map hmdToEyeViewOffset eyeRD)
  
  bindFramebuffer Framebuffer $= defaultFramebufferObject      
  
  viewport $= (Position 0 0, Size frameW frameH)
  ovrHmd_EndFrame hmd poses eyeTexture

  mainLoop win hmd (eyeTexture,texobj,fbo) eyeRD (twidth, theight) (frameNo + 1)


