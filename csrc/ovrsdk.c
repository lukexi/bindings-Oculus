/*
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
*/
#include <memory.h>
#include "OVR_CAPI_0_5_0.h"

ovrBool _ovr_InitializeRenderingShim()
{
        return ovr_InitializeRenderingShim();
}
ovrBool _ovr_Initialize()
{
        return ovr_Initialize(NULL);
}
void _ovr_Shutdown()
{
        ovr_Shutdown();
}
const char* _ovr_GetVersionString()
{
        return ovr_GetVersionString();
}
int _ovrHmd_Detect()
{
        return ovrHmd_Detect();
}
ovrHmd _ovrHmd_Create(int index)
{
        return ovrHmd_Create(index);
}
void _ovrHmd_Destroy(ovrHmd hmd)
{
        return ovrHmd_Destroy(hmd);
}
ovrHmd _ovrHmd_CreateDebug(ovrHmdType type)
{
        return ovrHmd_CreateDebug(type);
}
const char* _ovrHmd_GetLastError(ovrHmd hmd)
{
        return ovrHmd_GetLastError(hmd);
}
ovrBool _ovrHmd_AttachToWindow(ovrHmd hmd, void* window,
                        const ovrRecti* destMirrorRect,
                        const ovrRecti* sourceRenderTargetRect)
{
        return ovrHmd_AttachToWindow(hmd, window, destMirrorRect,
                                     sourceRenderTargetRect);
}
unsigned int _ovrHmd_GetEnabledCaps(ovrHmd hmd)
{
        return ovrHmd_GetEnabledCaps(hmd);
}
void _ovrHmd_SetEnabledCaps(ovrHmd hmd, unsigned int hmdCaps)
{
        ovrHmd_SetEnabledCaps(hmd, hmdCaps);
}

/***** Tracking Interface */
ovrBool _ovrHmd_ConfigureTracking(ovrHmd hmd,
                        unsigned int supportedTrackingCaps,
                        unsigned int requiredTrackingCaps)
{
        return ovrHmd_ConfigureTracking(hmd, supportedTrackingCaps,
                                        requiredTrackingCaps);
}
void _ovrHmd_RecenterPose(ovrHmd hmd)
{
        ovrHmd_RecenterPose(hmd);
}
ovrTrackingState g_TrackingState;
ovrTrackingState* _ovrHmd_GetTrackingState(ovrHmd hmd, double absTime)
{
        g_TrackingState = ovrHmd_GetTrackingState(hmd, absTime);
        return &g_TrackingState;
}

/***** Graphics Setup */
ovrSizei g_FovTextureSize;
ovrSizei* _ovrHmd_GetFovTextureSize(ovrHmd hmd, ovrEyeType eye,
                        ovrFovPort* fov,
                        float pixelsPerDisplayPixel)
{
        ovrFovPort tfov;
        memcpy(&tfov, fov, sizeof(ovrFovPort));
        g_FovTextureSize = ovrHmd_GetFovTextureSize(hmd, eye, tfov,
                                                  pixelsPerDisplayPixel);
        return &g_FovTextureSize;
}

/*****  SDK Distortion Rendering Functions */
ovrRenderAPIConfig g_apiConfig;
ovrFovPort g_eyeFovIn[2];
ovrBool _ovrHmd_ConfigureRendering(ovrHmd hmd,
                        ovrRenderAPIConfig* pApiConfig,
                        unsigned int distortionCaps,
                        ovrFovPort* pEyeFovIn,
                        ovrEyeRenderDesc* pEyeRenderDescOut)
{
        ovrRenderAPIConfig *p = NULL;
        memcpy(g_eyeFovIn, pEyeFovIn, sizeof(ovrFovPort) * 2);
        if (pApiConfig != NULL){
          memcpy(&g_apiConfig, pApiConfig, sizeof(ovrRenderAPIConfig));
          p = &g_apiConfig;
        }
                                   
        return ovrHmd_ConfigureRendering(hmd, p, distortionCaps,
                                         g_eyeFovIn, pEyeRenderDescOut);
}
ovrFrameTiming g_BeginFrame;
ovrFrameTiming* _ovrHmd_BeginFrame(ovrHmd hmd, unsigned int frameIndex)
{
        g_BeginFrame = ovrHmd_BeginFrame(hmd, frameIndex);
        return &g_BeginFrame;
}
ovrPosef g_renderPose[2];
ovrTexture g_eyeTexture[2];
void _ovrHmd_EndFrame(ovrHmd hmd,
                        ovrPosef* pRenderPose,
                        ovrTexture* pEyeTexture)
{
        memcpy(g_renderPose, pRenderPose, sizeof(ovrPosef) * 2);
        memcpy(g_eyeTexture, pEyeTexture, sizeof(ovrTexture) * 2);

        ovrHmd_EndFrame(hmd, g_renderPose, g_eyeTexture);
}
ovrPosef g_outEyePoses[2];
ovrPosef* _ovrHmd_GetEyePoses(ovrHmd hmd, unsigned int frameIndex,
                        ovrVector3f* pHmdToEyeViewOffset,
                        ovrTrackingState* pOutHmdTrackingState)
{
        ovrVector3f tHmdToEyeViewOffset[2];
        memcpy( &tHmdToEyeViewOffset,
                pHmdToEyeViewOffset, sizeof(ovrVector3f) * 2);

        ovrHmd_GetEyePoses(hmd, frameIndex,
                        tHmdToEyeViewOffset,
                        g_outEyePoses,
                        pOutHmdTrackingState);
        return &g_outEyePoses[0];
}
ovrPosef g_GetEyePose;
ovrPosef* _ovrHmd_GetHmdPosePerEye(ovrHmd hmd, ovrEyeType eye)
{
        g_GetEyePose = ovrHmd_GetHmdPosePerEye(hmd, eye);
        return &g_GetEyePose;
}

/*****  Client Distortion Rendering Functions */
ovrEyeRenderDesc g_GetRenderDesc;
ovrEyeRenderDesc* _ovrHmd_GetRenderDesc(ovrHmd hmd,
                        ovrEyeType eyeType, ovrFovPort* pFov)
{
        ovrFovPort fov;
        memcpy(&fov, pFov, sizeof(ovrFovPort));
        g_GetRenderDesc = ovrHmd_GetRenderDesc(hmd, eyeType, fov);
        return &g_GetRenderDesc;
}
ovrBool _ovrHmd_CreateDistortionMesh(ovrHmd hmd,
                        ovrEyeType eyeType, ovrFovPort* pFov,
                        unsigned int distortionCaps,
                        ovrDistortionMesh *meshData)
{
        ovrFovPort fov;
        memcpy(&fov, pFov, sizeof(ovrFovPort));
        return ovrHmd_CreateDistortionMesh(hmd, eyeType, fov,
                                           distortionCaps, meshData);
}
ovrBool _ovrHmd_CreateDistortionMeshDebug(ovrHmd hmd,
                        ovrEyeType eyeType, ovrFovPort* pFov,
                        unsigned int distortionCaps,
                        ovrDistortionMesh *meshData,
                        float debugEyeReliefOverrideInMetres) 
{
        ovrFovPort fov;
        memcpy(&fov, pFov, sizeof(ovrFovPort));
        return ovrHmd_CreateDistortionMeshDebug(hmd, eyeType, fov,
                                         distortionCaps, meshData,
                                         debugEyeReliefOverrideInMetres);
}
void _ovrHmd_DestroyDistortionMesh(ovrDistortionMesh* meshData)
{
        ovrHmd_DestroyDistortionMesh(meshData);
}
void _ovrHmd_GetRenderScaleAndOffset(ovrFovPort* pFov,
                        ovrSizei* pTextureSize, ovrRecti* pRenderViewport,
                        ovrVector2f* pUvScaleOffsetOut)
{
        ovrFovPort fov;
        ovrSizei textureSize;
        ovrRecti renderViewport;
        ovrVector2f uvScaleOffsetOut[2];

        memcpy(&fov, pFov, sizeof(ovrFovPort));
        memcpy(&textureSize, pTextureSize, sizeof(ovrSizei));
        memcpy(&renderViewport, pRenderViewport, sizeof(ovrRecti));
        memcpy(uvScaleOffsetOut, pUvScaleOffsetOut, sizeof(ovrVector2f));

        ovrHmd_GetRenderScaleAndOffset(fov, textureSize, renderViewport,
                                       uvScaleOffsetOut);
}
ovrFrameTiming g_GetFrameTiming;
ovrFrameTiming* _ovrHmd_GetFrameTiming(ovrHmd hmd, unsigned int frameIndex)
{
        g_GetFrameTiming = ovrHmd_GetFrameTiming(hmd, frameIndex);
        return &g_GetFrameTiming;
}
ovrFrameTiming g_BeginFrameTiming;
ovrFrameTiming* _ovrHmd_BeginFrameTiming(ovrHmd hmd,
                        unsigned int frameIndex)
{
        g_BeginFrameTiming = ovrHmd_BeginFrameTiming(hmd, frameIndex);
        return &g_BeginFrameTiming;
}
void _ovrHmd_EndFrameTiming(ovrHmd hmd)
{
        ovrHmd_EndFrameTiming(hmd);
}
void _ovrHmd_ResetFrameTiming(ovrHmd hmd,
                        unsigned int frameIndex)
{
        ovrHmd_ResetFrameTiming(hmd, frameIndex);
}
void _ovrHmd_GetEyeTimewarpMatrices(ovrHmd hmd, ovrEyeType eye,
                        ovrPosef* pRenderPose, ovrMatrix4f* pTwmOut)
{
        ovrPosef renderPose;
        ovrMatrix4f twmOut[2];

        memcpy(&renderPose, pRenderPose, sizeof(ovrPosef));
        memcpy(twmOut, pTwmOut, sizeof(ovrMatrix4f) * 2);

        ovrHmd_GetEyeTimewarpMatrices(hmd, eye, renderPose, twmOut);
}
void _ovrHmd_GetEyeTimewarpMatricesDebug(ovrHmd hmd, ovrEyeType eye,
                        ovrPosef* pRenderPose, ovrQuatf* pPlayerTorsoMotion, ovrMatrix4f* pTwmOut,
                        double debugTimingOffsetInSeconds)
{
        ovrPosef renderPose;
        ovrQuatf playerTorsoMotion;
        ovrMatrix4f twmOut[2];

        memcpy(&renderPose, pRenderPose, sizeof(ovrPosef));
        memcpy(&playerTorsoMotion, pPlayerTorsoMotion, sizeof(ovrQuatf));
        memcpy(twmOut, pTwmOut, sizeof(ovrMatrix4f) * 2);

        ovrHmd_GetEyeTimewarpMatricesDebug(hmd, eye, renderPose, playerTorsoMotion, twmOut,
                                      debugTimingOffsetInSeconds);
}


/*-------------------------------------------------*/
/***** Stateless math setup functions */
ovrMatrix4f g_Projection;
ovrMatrix4f* _ovrMatrix4f_Projection(ovrFovPort* pFov,
                        float znear, float zfar, ovrBool rightHanded)
{
        ovrFovPort fov;
        memcpy(&fov, pFov, sizeof(ovrFovPort));
        g_Projection = ovrMatrix4f_Projection(fov, znear, zfar,
                                              rightHanded);
        return &g_Projection;
}
ovrMatrix4f g_OrthoSubProjection;
ovrMatrix4f* _ovrMatrix4f_OrthoSubProjection(ovrMatrix4f* pProjection,
                        ovrVector2f* pOrthoScale, float orthoDistance,
                        float eyeViewAdjustX)
{
        ovrMatrix4f projection;
        ovrVector2f orthoScale;
        memcpy(&projection, pProjection, sizeof(ovrMatrix4f));
        memcpy(&orthoScale, pOrthoScale, sizeof(ovrVector2f));
        g_OrthoSubProjection = ovrMatrix4f_OrthoSubProjection(
                                     projection, orthoScale,
                                     orthoDistance, eyeViewAdjustX);
        return &g_OrthoSubProjection;
}
double _ovr_GetTimeInSeconds()
{
        return ovr_GetTimeInSeconds();
}
double _ovr_WaitTillTime(double absTime)
{
        return ovr_WaitTillTime(absTime);
}
/*-------------------------------------------------*/
/***** Latency Test interface */
unsigned char g_RgbColorOut[3];
ovrBool _ovrHmd_ProcessLatencyTest(ovrHmd hmd,
                        unsigned char* pRgbColorOut)
{
        ovrBool ret = ovrHmd_ProcessLatencyTest(hmd, g_RgbColorOut);
        memcpy(pRgbColorOut, g_RgbColorOut, sizeof(unsigned char) * 3);
        return ret;
}
const char* _ovrHmd_GetLatencyTestResult(ovrHmd hmd)
{
        return ovrHmd_GetLatencyTestResult(hmd);
}
ovrBool _ovrHmd_GetLatencyTest2DrawColor(ovrHmd hmd,
                        unsigned char* pRgbColorOut)
{
        ovrBool ret = ovrHmd_GetLatencyTest2DrawColor(hmd, g_RgbColorOut);
        memcpy(pRgbColorOut, g_RgbColorOut, sizeof(unsigned char) * 3);
        return ret;
}

/*-------------------------------------------------*/
/***** Health and Safety Warning Display interface */
ovrHSWDisplayState g_hasWarningState;
ovrHSWDisplayState* _ovrHmd_GetHSWDisplayState(ovrHmd hmd)
{
        ovrHmd_GetHSWDisplayState(hmd, &g_hasWarningState);
        return &g_hasWarningState;
}
ovrBool _ovrHmd_DismissHSWDisplay(ovrHmd hmd)
{
        return ovrHmd_DismissHSWDisplay(hmd);
}
/***** Property Access */
ovrBool _ovrHmd_GetBool(ovrHmd hmd, const char* propertyName,
                        ovrBool defaultVal)
{
        return ovrHmd_GetBool(hmd, propertyName, defaultVal);
}
ovrBool _ovrHmd_SetBool(ovrHmd hmd, const char* propertyName,
                        ovrBool value)
{
        return ovrHmd_SetBool(hmd, propertyName, value);
}
int _ovrHmd_GetInt(ovrHmd hmd, const char* propertyName,
                        int defaultVal)
{
        return ovrHmd_GetInt(hmd, propertyName, defaultVal);
}
ovrBool _ovrHmd_SetInt(ovrHmd hmd, const char* propertyName,
                        int value)
{
        return ovrHmd_SetInt(hmd, propertyName, value);
}
float _ovrHmd_GetFloat(ovrHmd hmd, const char* propertyName,
                        float defaultVal)
{
        return ovrHmd_GetFloat(hmd, propertyName, defaultVal);
}
ovrBool _ovrHmd_SetFloat(ovrHmd hmd, const char* propertyName,
                        float value)
{
        return ovrHmd_SetFloat(hmd, propertyName, value);
}
unsigned int _ovrHmd_GetFloatArray(ovrHmd hmd, const char* propertyName,
                        float values[], unsigned int arraySize)
{
        return ovrHmd_GetFloatArray(hmd, propertyName,
                                                values, arraySize);
}
ovrBool _ovrHmd_SetFloatArray(ovrHmd hmd, const char* propertyName,
                        float values[], unsigned int arraySize)
{
        return ovrHmd_SetFloatArray(hmd, propertyName,
                                    values, arraySize);
}

const char* _ovrHmd_GetString(ovrHmd hmd, const char* propertyName,
                        const char* defaultVal)
{
        return ovrHmd_GetString(hmd, propertyName, defaultVal);
}
ovrBool _ovrHmd_SetString(ovrHmd hmddesc, const char* propertyName,
                        const char* value)
{
        return ovrHmd_SetString(hmddesc, propertyName, value);
}
ovrBool _ovrHmd_StartPerfLog(ovrHmd hmd, const char* fileName,
                        const char* userData1)
{
        return ovrHmd_StartPerfLog(hmd, fileName, userData1);
}
ovrBool _ovrHmd_StopPerfLog(ovrHmd hmd)
{
        return ovrHmd_StopPerfLog(hmd);
}
