### Common Lisp bindings to [OpenVR](https://github.com/ValveSoftware/openvr/) API

It is intended that at least most of the OpenVR API will be exposed to a fairly high level, still a work in progress.
It comes with the OpenVR binaries version 1.10.30.

So far, nearly everything is exposed, with the notable exceptions of the [Skeletal Input API](https://github.com/ValveSoftware/openvr/wiki/SteamVR-Skeletal-Input) and some parts of the System API. Large parts of the API are not yet tested, but it has been used to run VR experiences through [trial-vr](https://github.com/selwynsimsek/trial-vr).

Interface | Exposed? | Tested? | Works? | Version
--- | --- | --- | --- | ---
System | 85% | 0% | 0% | IVRSystem_020
Chaperone | 100% | 100% | 75% | IVRChaperone_003
ChaperoneSetup | 100% | 100% | 100% | IVRChaperoneSetup_006
Compositor | 100% | 70% | 57% | IVRCompositor_022
Overlay | 100% | 69% | 69% | IVROverlay_022
Resources | 100% | 100% | 100% | IVRResources_001
RenderModels | -  | 0% | 0% | IVRRenderModels_006
ExtendedDisplay | 100% | 100% | 100% | IVRExtendedDisplay_001
Settings | 100% | 100% | 25% | IVRSettings_002
Applications | 100% | 27% |  23% | IVRApplications_006
TrackedCamera | 100% | 31% | 23% | IVRTrackedCamera_006
Screenshots | 100% | 100% | 100% | IVRScreenshots_001
DriverManager | 100% | 100% | 100% | IVRDriverManager_001
Input | 90% | ~80% | ~75% | IVRInput_007
IOBuffer | 100% | 0% | 0% | IVRIOBuffer_002
SpatialAnchors | 100% | 0% | 0% | IVRSpatialAnchors_001
Debug | 100% | 100% | 100% | IVRDebug_001
Notifications | 100% | Not implemented in OpenVR | - | IVRNotifications_002
HeadsetView | 100% | 0% | 0% | IVRHeadsetView_001
