--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module GLView where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil

initGL :: IO (ShaderProgram, GL.VertexArrayObject, GL.VertexArrayObject)
initGL = do
  -- texture Texture2D $= Enabled
  -- shadeModel        $= Smooth
  clearColor        $= Color4 1 0 0 1.0
  clearDepth        $= 1.0
  depthFunc         $= Just Less

  --
  lineSmooth        $= Enabled
  lineWidth         $= 10.0
  blend             $= Enabled
  blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
  --blendFunc         $= (SrcAlpha, OneMinusConstantAlpha)
  --alphaFunc         $= Just (Greater, 0.2)
  --lightModelAmbient $= Color4 0.1 0.1 0.1 0.2
  --lighting        $= Enabled
  --colorMaterial     $= Just (FrontAndBack, AmbientAndDiffuse)
  colorMaterial     $= Just (GL.Front, AmbientAndDiffuse)

  -- init shaders
  sp <- simpleShaderProgramWith "test/case1/simple.vert"
                                "test/case1/simple.frag"
                                $ \ p -> do
    attribLocation p "VertexPosition" $= AttribLocation 0
    bindFragDataLocation p "FragColor" $= 0

  GL.currentProgram GL.$= Just (program sp)
  pLog <- GL.get $ GL.programInfoLog (program sp)
  putStrLn pLog

  -- load objects
  tvao <- makeVAO $ do
    makeBuffer ArrayBuffer triangle
    enableAttrib sp "VertexPosition"
    setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
    makeBuffer ArrayBuffer triColor 
    enableAttrib sp "VertexColor"
    setAttrib sp "VertexColor" ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
    return ()

  qvao <- makeVAO $ do
    makeBuffer ArrayBuffer quads
    enableAttrib sp "VertexPosition"
    setAttrib sp "VertexPosition" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
    makeBuffer ArrayBuffer qColor 
    enableAttrib sp "VertexColor"
    setAttrib sp "VertexColor" ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
    return ()
  GL.currentProgram GL.$= Nothing

  return (sp,tvao,qvao)


render :: (ShaderProgram, GL.VertexArrayObject, GL.VertexArrayObject)
       -> IO ()
render (shprg,tvao,qvao) = do
  GL.currentProgram GL.$= Just (program shprg)
  GL.clientState GL.VertexArray $= GL.Enabled

  let vUnifLoc = getUniform shprg "ViewMat"
  uniformMat vUnifLoc $= [[0.5,0,0,0],[0,0.5,0,0] ,[0,0,0.5,0],[0,0,0,1]]
  --asUniform vUnifLoc -- camMatrix camera2D

  setUniform shprg "TransMat" $ Vertex3 1 0 (0 :: GLfloat)
  withVAO tvao $ do
    GL.drawArrays GL.Triangles 0 $ fromIntegral (3::Int)
  
  setUniform shprg "TransMat" $ Vertex3 (-1) 0 (0 :: GLfloat)
  withVAO qvao $ do
    GL.drawArrays GL.Quads 0 $ fromIntegral (4::Int)

  GL.currentProgram GL.$= Nothing
  --return ()

triangle, triColor, quads, qColor :: [GL.GLfloat]
triColor = [
  1.0, 1.0, 0.0, 1.0,
  1.0, 0.0, 1.0, 1.0,
  0.0, 1.0, 1.0, 1.0]
triangle =
  [ -1.0, -1.0,  0.0
  ,  1.0, -1.0,  0.0
  ,  0.0,  1.0,  0.0]
qColor =
  [ 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 0.0, 1.0
  , 1.0, 0.0, 1.0, 1.0
  , 0.0, 1.0, 1.0, 1.0]
quads =
  [ -1.0, -1.0,  0.0
  ,  1.0, -1.0,  0.0
  ,  1.0,  1.0,  0.0
  , -1.0,  1.0,  0.0
  ]



