### Common Lisp bindings to [OpenVR](https://github.com/ValveSoftware/openvr/) API

It is intended that at least most of the OpenVR API will be exposed to a fairly high level, still a work in progress.

Interface | Percentage of interface exposed | Tested? | Version
--- | --- | --- | ---
System | 85% | N | IVRSystem_020
Chaperone | 75% | Y | IVRChaperone_003
ChaperoneSetup | 100% | Partly | IVRChaperoneSetup_006
Compositor | 90% | Mostly | IVRCompositor_022
Overlay | 70% | N | IVROverlay_022
Resources | 100% | Y | IVRResources_001
RenderModels | -  | - | IVRRenderModels_006
ExtendedDisplay | 100% | Y | IVRExtendedDisplay_001
Settings | 100% | Some | IVRSettings_002
Applications | 100% | Some | IVRApplications_006
TrackedCamera | 100% | N | IVRTrackedCamera_006
Screenshots | 100% | N | IVRScreenshots_001
DriverManager | 100% | Y | IVRDriverManager_001
Input | 90% | Some | IVRInput_007
IOBuffer | 100% | N | IVRIOBuffer_002
SpatialAnchors | 100% | N | IVRSpatialAnchors_001
Debug | 100% | Y | IVRDebug_001
Notifications | 100% | Not implemented in OpenVR | IVRNotifications_002
