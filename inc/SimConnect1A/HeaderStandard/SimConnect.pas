unit SimConnect;
{
 This translation follows the convention of SimConnect.h, where the API
 definitions are mapped directly to the SimConnect.Dll library
 This means that any application built using this interface will fail to load
 on any PC that does not have SimConnect.Dll installed, with the OS reporting
 that the application has not been properly installed.
 The alternative version uses run-time dynamic loading, which requires an
 explicit initialisation call in the application, but can fail gracefully, or
 disable FS X connectivity if SimConnect.Dll is not installed.

 First translated from SimConnect.h (plus .res, .rc and manifest files)
 by Dick "rhumbaflappy" on the AVSim forum

 Partly checked (in process) by Jacky Brouze / JAB
 Comments welcome to jacky.brouze@vtx.ch

 Further updates by Ken Adam (KA) ken@akadamia.co.uk
    - Formatted using DelForExp
    - Some types corrected
    - Delphi style equivalent types added
    - Further testing with SDK examples
    - Support passsing method to CallDispatch
    - Pack records to ensure correct size and location of fields

 Updates for FSX SP1A: (Ken Adam)
    - Changed manifest
    - Additional constants
    - Additional enumerated types
    - Emended API declarations for AddToClientdataDefinition, RequestClientData,
      SetClientData
    - Added API declarations for Text and Facilties
}

{$R 'SimConnect.res' 'SimConnect.rc'}

interface

uses Windows, Messages, SysUtils, Classes;

//----------------------------------------------------------------------------
//        Constants
//----------------------------------------------------------------------------

type
  SIMCONNECT_OBJECT_ID = DWORD;

const
  DWORD_MAX         = $FFFFFFFF;
  FLT_MAX           = 3.402823466E+38;
  SIMCONNECT_UNUSED = DWORD_MAX; // special value to indicate unused event, ID
  SIMCONNECT_OBJECT_ID_USER = 0;        // proxy value for User vehicle ObjectID
  SIMCONNECT_CAMERA_IGNORE_FIELD = FLT_MAX;
    //Used to tell the Camera API to NOT modify the value in this part Of the argument.
  SIMCONNECT_CLIENTDATA_MAX_SIZE = 8192; // SP1A

  // Notification Group priority values
  SIMCONNECT_GROUP_PRIORITY_HIGHEST = 1; // highest priority
  SIMCONNECT_GROUP_PRIORITY_HIGHEST_MASKABLE = 10000000;
    // highest priority that allows events to be masked
  SIMCONNECT_GROUP_PRIORITY_STANDARD = 1900000000; // standard priority
  SIMCONNECT_GROUP_PRIORITY_DEFAULT = 2000000000; // default priority
  SIMCONNECT_GROUP_PRIORITY_LOWEST = 4000000000;
    // priorities lower than this will be ignored

  //Weather observations Metar strings
  MAX_METAR_LENGTH  = 2000;

  // Maximum thermal size is 100 km.
  MAX_THERMAL_SIZE  = 100000.0;
  MAX_THERMAL_RATE  = 1000.0;

  // SIMCONNECT_DATA_INITPOSITION.Airspeed
  INITPOSITION_AIRSPEED_CRUISE = -1;    // aircraft's cruise airspeed
  INITPOSITION_AIRSPEED_KEEP = -2;      // keep current airspeed

  // AddToClientDataDefinition dwSizeOrType parameter type values (SP1A)
  SIMCONNECT_CLIENTDATATYPE_INT8 = -1;  //  8-bit integer number
  SIMCONNECT_CLIENTDATATYPE_INT16 = -2; // 16-bit integer number
  SIMCONNECT_CLIENTDATATYPE_INT32 = -3; // 32-bit integer number
  SIMCONNECT_CLIENTDATATYPE_INT64 = -4; // 64-bit integer number
  SIMCONNECT_CLIENTDATATYPE_FLOAT32 = -5; // 32-bit floating-point number (float)
  SIMCONNECT_CLIENTDATATYPE_FLOAT64 = -6;  // 64-bit floating-point number (double)

  // AddToClientDataDefinition dwOffset parameter special values
  SIMCONNECT_CLIENTDATAOFFSET_AUTO = -1;  // automatically compute offset of the ClientData variable


  //----------------------------------------------------------------------------
  //        Type enumerations
  //----------------------------------------------------------------------------

type

  // Receive data Types
  SIMCONNECT_RECV_ID = (
    SIMCONNECT_RECV_ID_NULL,
    SIMCONNECT_RECV_ID_EXCEPTION,
    SIMCONNECT_RECV_ID_OPEN,
    SIMCONNECT_RECV_ID_QUIT,
    SIMCONNECT_RECV_ID_EVENT,
    SIMCONNECT_RECV_ID_EVENT_OBJECT_ADDREMOVE,
    SIMCONNECT_RECV_ID_EVENT_FILENAME,
    SIMCONNECT_RECV_ID_EVENT_FRAME,
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA,
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE,
    SIMCONNECT_RECV_ID_WEATHER_OBSERVATION,
    SIMCONNECT_RECV_ID_CLOUD_STATE,
    SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID,
    SIMCONNECT_RECV_ID_RESERVED_KEY,
    SIMCONNECT_RECV_ID_CUSTOM_ACTION,
    SIMCONNECT_RECV_ID_SYSTEM_STATE,
    SIMCONNECT_RECV_ID_CLIENT_DATA,
    SIMCONNECT_RECV_ID_EVENT_WEATHER_MODE,
    SIMCONNECT_RECV_ID_AIRPORT_LIST,
    SIMCONNECT_RECV_ID_VOR_LIST,
    SIMCONNECT_RECV_ID_NDB_LIST,
    SIMCONNECT_RECV_ID_WAYPOINT_LIST
    );
  TSimConnectRecvId = SIMCONNECT_RECV_ID;

  // Data data Types
  SIMCONNECT_DATAType = (
    SIMCONNECT_DATAType_INVALID,        // invalid data Type
    SIMCONNECT_DATAType_INT32,          // 32-bit integer number
    SIMCONNECT_DATAType_INT64,          // 64-bit integer number
    SIMCONNECT_DATAType_FLOAT32,        // 32-bit floating-point number (Single)
    SIMCONNECT_DATAType_FLOAT64,        // 64-bit floating-point number (Double)
    SIMCONNECT_DATAType_STRING8,        // 8-byte String
    SIMCONNECT_DATAType_STRING32,       // 32-byte String
    SIMCONNECT_DATAType_STRING64,       // 64-byte String
    SIMCONNECT_DATAType_STRING128,      // 128-byte String
    SIMCONNECT_DATAType_STRING256,      // 256-byte String
    SIMCONNECT_DATAType_STRING260,      // 260-byte String
    SIMCONNECT_DATAType_STRINGV,        // variable-length String
    SIMCONNECT_DATAType_INITPOSITION,   // see SIMCONNECT_DATA_INITPOSITION
    SIMCONNECT_DATAType_MARKERSTATE,    // see SIMCONNECT_DATA_MARKERSTATE
    SIMCONNECT_DATAType_WAYPOINT,       // see SIMCONNECT_DATA_WAYPOINT
    SIMCONNECT_DATAType_LATLONALT,      // see SIMCONNECT_DATA_LATLONALT
    SIMCONNECT_DATAType_XYZ,            // see SIMCONNECT_DATA_XYZ
    SIMCONNECT_DATAType_MAX             // enum limit
    );
  TSimConnectDataType = SIMCONNECT_DATAType;

  // Exception error Types
  SIMCONNECT_EXCEPTION = (
    SIMCONNECT_EXCEPTION_NONE,
    SIMCONNECT_EXCEPTION_ERROR,
    SIMCONNECT_EXCEPTION_SIZE_MISMATCH,
    SIMCONNECT_EXCEPTION_UNRECOGNIZED_ID,
    SIMCONNECT_EXCEPTION_UNOPENED,
    SIMCONNECT_EXCEPTION_VERSION_MISMATCH,
    SIMCONNECT_EXCEPTION_TOO_MANY_GROUPS,
    SIMCONNECT_EXCEPTION_NAME_UNRECOGNIZED,
    SIMCONNECT_EXCEPTION_TOO_MANY_EVENT_NAMES,
    SIMCONNECT_EXCEPTION_EVENT_ID_DUPLICATE,
    SIMCONNECT_EXCEPTION_TOO_MANY_MAPS,
    SIMCONNECT_EXCEPTION_TOO_MANY_OBJECTS,
    SIMCONNECT_EXCEPTION_TOO_MANY_REQUESTS,
    SIMCONNECT_EXCEPTION_WEATHER_INVALID_PORT,
    SIMCONNECT_EXCEPTION_WEATHER_INVALID_METAR,
    SIMCONNECT_EXCEPTION_WEATHER_UNABLE_TO_GET_OBSERVATION,
    SIMCONNECT_EXCEPTION_WEATHER_UNABLE_TO_CREATE_STATION,
    SIMCONNECT_EXCEPTION_WEATHER_UNABLE_TO_REMOVE_STATION,
    SIMCONNECT_EXCEPTION_INVALID_DATA_Type,
    SIMCONNECT_EXCEPTION_INVALID_DATA_SIZE,
    SIMCONNECT_EXCEPTION_DATA_ERROR,
    SIMCONNECT_EXCEPTION_INVALID_ARRAY,
    SIMCONNECT_EXCEPTION_CREATE_OBJECT_FAILED,
    SIMCONNECT_EXCEPTION_LOAD_FLIGHTPLAN_FAILED,
    SIMCONNECT_EXCEPTION_OPERATION_INVALID_FOR_OBJECT_Type,
    SIMCONNECT_EXCEPTION_ILLEGAL_OPERATION,
    SIMCONNECT_EXCEPTION_ALREADY_SUBSCRIBED,
    SIMCONNECT_EXCEPTION_INVALID_ENUM,
    SIMCONNECT_EXCEPTION_DEFINITION_ERROR,
    SIMCONNECT_EXCEPTION_DUPLICATE_ID,
    SIMCONNECT_EXCEPTION_DATUM_ID,
    SIMCONNECT_EXCEPTION_OUT_OF_BOUNDS,
    SIMCONNECT_EXCEPTION_ALREADY_CREATED,
    SIMCONNECT_EXCEPTION_OBJECT_OUTSIDE_REALITY_BUBBLE,
    SIMCONNECT_EXCEPTION_OBJECT_CONTAINER,
    SIMCONNECT_EXCEPTION_OBJECT_AI,
    SIMCONNECT_EXCEPTION_OBJECT_ATC,
    SIMCONNECT_EXCEPTION_OBJECT_SCHEDULE
    );
  TSimConnectException = SIMCONNECT_EXCEPTION;
  // Object Types
  SIMCONNECT_SIMOBJECT_Type = (
    SIMCONNECT_SIMOBJECT_Type_USER,
    SIMCONNECT_SIMOBJECT_Type_ALL,
    SIMCONNECT_SIMOBJECT_Type_AIRCRAFT,
    SIMCONNECT_SIMOBJECT_Type_HELICOPTER,
    SIMCONNECT_SIMOBJECT_Type_BOAT,
    SIMCONNECT_SIMOBJECT_Type_GROUND
    );
  TSimConnectSimObjectType = SIMCONNECT_SIMOBJECT_Type;

  // EventState values
  SIMCONNECT_STATE = (
    SIMCONNECT_STATE_OFF,
    SIMCONNECT_STATE_ON
    );
  TSimConnectState = SIMCONNECT_STATE;

  // Object Data Request Period values
  SIMCONNECT_PERIOD = (
    SIMCONNECT_PERIOD_NEVER,
    SIMCONNECT_PERIOD_ONCE,
    SIMCONNECT_PERIOD_VISUAL_FRAME,
    SIMCONNECT_PERIOD_SIM_FRAME,
    SIMCONNECT_PERIOD_SECOND
    );
  TSimConnectPeriod = SIMCONNECT_PERIOD;

  // Mission End values
  SIMCONNECT_MISSION_END = (
    SIMCONNECT_MISSION_FAILED,
    SIMCONNECT_MISSION_CRASHED,
    SIMCONNECT_MISSION_SUCCEEDED
    );
  TSimConnectMissionEnd = SIMCONNECT_MISSION_END;

  // SP1A Additions
  // ClientData Request Period values
  SIMCONNECT_CLIENT_DATA_PERIOD = (
    SIMCONNECT_CLIENT_DATA_PERIOD_NEVER,
    SIMCONNECT_CLIENT_DATA_PERIOD_ONCE,
    SIMCONNECT_CLIENT_DATA_PERIOD_VISUAL_FRAME,
    SIMCONNECT_CLIENT_DATA_PERIOD_ON_SET,
    SIMCONNECT_CLIENT_DATA_PERIOD_SECOND
    );
  TSimConnectClientDataPeriod = SIMCONNECT_CLIENT_DATA_PERIOD;

  SIMCONNECT_TEXT_TYPE = (
    SIMCONNECT_TEXT_TYPE_SCROLL_BLACK,
    SIMCONNECT_TEXT_TYPE_SCROLL_WHITE,
    SIMCONNECT_TEXT_TYPE_SCROLL_RED,
    SIMCONNECT_TEXT_TYPE_SCROLL_GREEN,
    SIMCONNECT_TEXT_TYPE_SCROLL_BLUE,
    SIMCONNECT_TEXT_TYPE_SCROLL_YELLOW,
    SIMCONNECT_TEXT_TYPE_SCROLL_MAGENTA,
    SIMCONNECT_TEXT_TYPE_SCROLL_CYAN,
    SIMCONNECT_TEXT_TYPE_PRINT_BLACK = $0100,
    SIMCONNECT_TEXT_TYPE_PRINT_WHITE,
    SIMCONNECT_TEXT_TYPE_PRINT_RED,
    SIMCONNECT_TEXT_TYPE_PRINT_GREEN,
    SIMCONNECT_TEXT_TYPE_PRINT_BLUE,
    SIMCONNECT_TEXT_TYPE_PRINT_YELLOW,
    SIMCONNECT_TEXT_TYPE_PRINT_MAGENTA,
    SIMCONNECT_TEXT_TYPE_PRINT_CYAN,
    SIMCONNECT_TEXT_TYPE_MENU = $0200
    );
  TSimConnectTextType = SIMCONNECT_TEXT_TYPE;

  SIMCONNECT_TEXT_RESULT = (
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_1,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_2,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_3,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_4,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_5,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_6,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_7,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_8,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_9,
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_10,
    SIMCONNECT_TEXT_RESULT_DISPLAYED = $00010000,
    SIMCONNECT_TEXT_RESULT_QUEUED,
    SIMCONNECT_TEXT_RESULT_REMOVED,
    SIMCONNECT_TEXT_RESULT_REPLACED,
    SIMCONNECT_TEXT_RESULT_TIMEOUT
    );
  TSimConnectTextResult = SIMCONNECT_TEXT_RESULT;

  SIMCONNECT_WEATHER_MODE = (
    SIMCONNECT_WEATHER_MODE_THEME,
    SIMCONNECT_WEATHER_MODE_RWW,
    SIMCONNECT_WEATHER_MODE_CUSTOM,
    SIMCONNECT_WEATHER_MODE_GLOBAL
    );
  TSimConnectWeatherMode = SIMCONNECT_WEATHER_MODE;

  SIMCONNECT_FACILITY_LIST_TYPE = (
    SIMCONNECT_FACILITY_LIST_TYPE_AIRPORT,
    SIMCONNECT_FACILITY_LIST_TYPE_WAYPOINT,
    SIMCONNECT_FACILITY_LIST_TYPE_NDB,
    SIMCONNECT_FACILITY_LIST_TYPE_VOR,
    SIMCONNECT_FACILITY_LIST_TYPE_COUNT // invalid
    );
  TSimConnectFacilityListType = SIMCONNECT_FACILITY_LIST_TYPE;
type
  SIMCONNECT_VOR_FLAGS = DWORD;         // flags for SIMCONNECT_RECV_ID_VOR_LIST
  TSimConnectVorFlags = SIMCONNECT_VOR_FLAGS;
const
  SIMCONNECT_RECV_ID_VOR_LIST_HAS_NAV_SIGNAL = $00000001; // Has Nav signal
  SIMCONNECT_RECV_ID_VOR_LIST_HAS_LOCALIZER = $00000002; // Has localizer
  SIMCONNECT_RECV_ID_VOR_LIST_HAS_GLIDE_SLOPE = $00000004; // Has Nav signal
  SIMCONNECT_RECV_ID_VOR_LIST_HAS_DME = $00000008; // Station has DME
  // end of SP1A Additions

  // bits for the Waypoint Flags field: may be combined
type
  SIMCONNECT_WAYPOINT_FLAGS = DWORD;
  TSimConnectWaypointFlags = SIMCONNECT_WAYPOINT_FLAGS;
const
  SIMCONNECT_WAYPOINT_NONE = $00;
  SIMCONNECT_WAYPOINT_SPEED_REQUESTED = $04;
    // requested speed at waypoint is valid
  SIMCONNECT_WAYPOINT_THROTTLE_REQUESTED = $08;
    // request a specific throttle percentage
  SIMCONNECT_WAYPOINT_COMPUTE_VERTICAL_SPEED = $10;
    // compute vertical to speed to reach waypoint altitude when crossing the waypoint
  SIMCONNECT_WAYPOINT_ALTITUDE_IS_AGL = $20; // AltitudeIsAGL
  SIMCONNECT_WAYPOINT_ON_GROUND = $00100000; // place this waypoint on the ground
  SIMCONNECT_WAYPOINT_REVERSE = $00200000;
    // Back up to this waypoint. Only valid on first waypoint
  SIMCONNECT_WAYPOINT_WRAP_TO_FIRST = $00400000;
    // Wrap around back to first waypoint. Only valid on last waypoint.

type
  SIMCONNECT_EVENT_FLAG = DWORD;
  TSimConnectEventFlag = SIMCONNECT_EVENT_FLAG;
const
  SIMCONNECT_EVENT_FLAG_DEFAULT = $00000000;
  SIMCONNECT_EVENT_FLAG_FAST_REPEAT_TIMER = $00000001;
    // set event repeat timer to simulate fast repeat
  SIMCONNECT_EVENT_FLAG_SLOW_REPEAT_TIMER = $00000002;
    // set event repeat timer to simulate slow repeat
  SIMCONNECT_EVENT_FLAG_GROUPID_IS_PRIORITY = $00000010;
    // interpret GroupID parameter as priority value

type
  SIMCONNECT_DATA_REQUEST_FLAG = DWORD;
  TSimConnectDataRequestFlag = SIMCONNECT_DATA_REQUEST_FLAG;
const
  SIMCONNECT_DATA_REQUEST_FLAG_DEFAULT = $00000000;
  SIMCONNECT_DATA_REQUEST_FLAG_CHANGED = $00000001;
    // send requested data when value(s) change
  SIMCONNECT_DATA_REQUEST_FLAG_TAGGED = $00000002;
    // send requested data in tagged format

type
  SIMCONNECT_DATA_SET_FLAG = DWORD;
  TSimConnectDataSetFlag = SIMCONNECT_DATA_SET_FLAG;
const
  SIMCONNECT_DATA_SET_FLAG_DEFAULT = $00000000; // data is in tagged format
  SIMCONNECT_DATA_SET_FLAG_TAGGED = $00000001; // data is in tagged format

type
  SIMCONNECT_CREATE_CLIENT_DATA_FLAG = DWORD;
  TSimConnectCreateClientDataFlag = SIMCONNECT_CREATE_CLIENT_DATA_FLAG;
const
  SIMCONNECT_CREATE_CLIENT_DATA_FLAG_DEFAULT = $00000000;
  SIMCONNECT_CREATE_CLIENT_DATA_FLAG_READ_ONLY = $00000001;
  // permit only ClientData creator to write into ClientData

  // SP1A additions
type
  SIMCONNECT_CLIENT_DATA_REQUEST_FLAG = DWORD;
  TSimConnectClientDataRequestFlag = SIMCONNECT_CLIENT_DATA_REQUEST_FLAG;
const
  SIMCONNECT_CLIENT_DATA_REQUEST_FLAG_DEFAULT = $00000000;
  SIMCONNECT_CLIENT_DATA_REQUEST_FLAG_CHANGED = $00000001;  // send requested ClientData when value(s) change
  SIMCONNECT_CLIENT_DATA_REQUEST_FLAG_TAGGED = $00000002;  // send requested ClientData in tagged format

type
  SIMCONNECT_CLIENT_DATA_SET_FLAG = DWORD;
  TSimConnectClientDataSetFlag = SIMCONNECT_CLIENT_DATA_SET_FLAG;
const
  SIMCONNECT_CLIENT_DATA_SET_FLAG_DEFAULT = $00000000;
  SIMCONNECT_CLIENT_DATA_SET_FLAG_TAGGED = $00000001; // data is in tagged format
  // End of SP1A additions

type
  SIMCONNECT_VIEW_SYSTEM_EVENT_DATA = DWORD;
  TSimConnectViewSystemEventData = SIMCONNECT_VIEW_SYSTEM_EVENT_DATA;
const
  SIMCONNECT_VIEW_SYSTEM_EVENT_DATA_COCKPIT_2D = $00000001;
    // 2D Panels in cockpit view
  SIMCONNECT_VIEW_SYSTEM_EVENT_DATA_COCKPIT_VIRTUAL = $00000002;
    // Virtual (3D) panels in cockpit view
  SIMCONNECT_VIEW_SYSTEM_EVENT_DATA_ORTHOGONAL = $00000004;
    // Orthogonal (Map) view

type
  SIMCONNECT_SOUND_SYSTEM_EVENT_DATA = DWORD;
  TSimConnectSoundSystemEventData = SIMCONNECT_SOUND_SYSTEM_EVENT_DATA;
const
  SIMCONNECT_SOUND_SYSTEM_EVENT_DATA_MASTER = $00000001; // Sound Master

  //----------------------------------------------------------------------------
  //        User-defined enums
  //----------------------------------------------------------------------------

type
  SIMCONNECT_NOTIFICATION_GROUP_ID = DWORD;
  SIMCONNECT_INPUT_GROUP_ID = DWORD;
  SIMCONNECT_DATA_DEFINITION_ID = DWORD;
  SIMCONNECT_DATA_REQUEST_ID = DWORD;
  SIMCONNECT_CLIENT_EVENT_ID = DWORD;
  SIMCONNECT_CLIENT_DATA_ID = DWORD;
  SIMCONNECT_CLIENT_DATA_DEFINITION_ID = DWORD;

  //----------------------------------------------------------------------------
  //        Structure Types
  //----------------------------------------------------------------------------

type
  PSIMCONNECT_RECV = ^SIMCONNECT_RECV;
  SIMCONNECT_RECV = packed record
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
  end;
  TSimConnectRecv = SIMCONNECT_RECV;
  PSimConnectRecv = PSIMCONNECT_RECV;

  PSIMCONNECT_RECV_EXCEPTION = ^SIMCONNECT_RECV_EXCEPTION;
  SIMCONNECT_RECV_EXCEPTION = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwException: DWORD;
    dwSendID: DWORD;
    dwIndex: DWORD;
  end;
  TSimConnectRecvException = SIMCONNECT_RECV_EXCEPTION;
  PSimConnectRecvException = PSIMCONNECT_RECV_EXCEPTION;

  PSIMCONNECT_RECV_OPEN = ^SIMCONNECT_RECV_OPEN;
  SIMCONNECT_RECV_OPEN = packed record  {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    szApplicationName: array[0..255] of Char;
    dwApplicationVersionMajor: DWORD;
    dwApplicationVersionMinor: DWORD;
    dwApplicationBuildMajor: DWORD;
    dwApplicationBuildMinor: DWORD;
    dwSimConnectVersionMajor: DWORD;
    dwSimConnectVersionMinor: DWORD;
    dwSimConnectBuildMajor: DWORD;
    dwSimConnectBuildMinor: DWORD;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;
  TSimConnectRecvOpen = SIMCONNECT_RECV_OPEN;
  PSimConnectRecvOpen = PSIMCONNECT_RECV_OPEN;

  PSIMCONNECT_RECV_QUIT = ^SIMCONNECT_RECV_QUIT;
  SIMCONNECT_RECV_QUIT = packed record  {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
  end;
  TSimConnectRecvQuit = SIMCONNECT_RECV_QUIT;
  PSimConnectRecvQuit = PSIMCONNECT_RECV_QUIT;

  PSIMCONNECT_RECV_EVENT = ^SIMCONNECT_RECV_EVENT;
  SIMCONNECT_RECV_EVENT = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
  end;
  TSimConnectRecvEvent = SIMCONNECT_RECV_EVENT;
  PSimConnectRecvEvent = PSIMCONNECT_RECV_EVENT;

  // when dwID == SIMCONNECT_RECV_ID_EVENT_FILENAME
  PSIMCONNECT_RECV_EVENT_FILENAME = ^SIMCONNECT_RECV_EVENT_FILENAME;
  SIMCONNECT_RECV_EVENT_FILENAME = packed record {SIMCONNECT_RECV_EVENT}
    // "Inherits" SIMCONNECT_RECV_EVENT
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
    // End of Inherit
    szFileName: array[0..256 - 1] of Char; // uEventID-dependent context
    dwFlags: DWORD;
  end;
  TSimConnectRecvEventFileName = SIMCONNECT_RECV_EVENT_FILENAME;
  PSimConnectRecvEventFileName = PSIMCONNECT_RECV_EVENT_FILENAME;

  PSIMCONNECT_RECV_EVENT_OBJECT_ADDREMOVE =
    ^SIMCONNECT_RECV_EVENT_OBJECT_ADDREMOVE;
  SIMCONNECT_RECV_EVENT_OBJECT_ADDREMOVE = packed record {SIMCONNECT_RECV_EVENT}
    // "Inherits" SIMCONNECT_RECV_EVENT
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
    // End of Inherit
    eObjType: TSimConnectSimObjectType;
  end;
  TSimConnectRecvEventObjectAddRemove = SIMCONNECT_RECV_EVENT_OBJECT_ADDREMOVE;
  PSimConnectRecvEventObjectAddRemove = PSIMCONNECT_RECV_EVENT_OBJECT_ADDREMOVE;

  // when dwID == SIMCONNECT_RECV_ID_EVENT_FRAME
  PSIMCONNECT_RECV_EVENT_FRAME = ^SIMCONNECT_RECV_EVENT_FRAME;
  SIMCONNECT_RECV_EVENT_FRAME = packed record {SIMCONNECT_RECV_EVENT}
    // "Inherits" SIMCONNECT_RECV_EVENT
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
    // End of Inherit
    FrameRate: single;
    fSimSpeed: single;
  end;
  TSimConnectRecvEventFrame = SIMCONNECT_RECV_EVENT_FRAME;
  PSimConnectRecvEventFrame = PSIMCONNECT_RECV_EVENT_FRAME;

  PSIMCONNECT_RECV_SIMOBJECT_DATA = ^SIMCONNECT_RECV_SIMOBJECT_DATA;
  SIMCONNECT_RECV_SIMOBJECT_DATA = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwObjectID: DWORD;
    dwDefineID: DWORD;
    dwFlags: DWORD;                     // SIMCONNECT_DATA_REQUEST_FLAG
    dwentrynumber: DWORD;
      // if multiple objects returned, this is number <entrynumber> out of <outof>.
    dwoutof: DWORD;                     // note: starts with 1, not 0.
    dwDefineCount: DWORD; // data count (number of datums, *not* byte count)
    dwData: DWORD; // data begins here, dwDefineCount data items
  end;
  TSimConnectRecvSimObjectData = SIMCONNECT_RECV_SIMOBJECT_DATA;
  PSimConnectRecvSimObjectData = PSIMCONNECT_RECV_SIMOBJECT_DATA;

  // when dwID == SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYType
  PSIMCONNECT_RECV_SIMOBJECT_DATA_BYTYPE =
    ^SIMCONNECT_RECV_SIMOBJECT_DATA_BYTYPE;
  SIMCONNECT_RECV_SIMOBJECT_DATA_BYTYPE = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwObjectID: DWORD;
    dwDefineID: DWORD;
    dwFlags: DWORD;                     // SIMCONNECT_DATA_REQUEST_FLAG
    dwentrynumber: DWORD;
      // if multiple objects returned, this is number <entrynumber> out of <outof>.
    dwoutof: DWORD;                     // note: starts with 1, not 0.
    dwDefineCount: DWORD; // data count (number of datums, *not* byte count)
    dwData: DWORD; // data begins here, dwDefineCount data items
  end;
  TSimConnectRecvSimObjectDataByType = SIMCONNECT_RECV_SIMOBJECT_DATA_BYTYPE;
  PSimConnectRecvSimObjectDataByType = PSIMCONNECT_RECV_SIMOBJECT_DATA_BYTYPE;

  // when dwID == SIMCONNECT_RECV_ID_CLIENT_DATA
  PSIMCONNECT_RECV_CLIENT_DATA = ^SIMCONNECT_RECV_CLIENT_DATA;
  SIMCONNECT_RECV_CLIENT_DATA = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwObjectID: DWORD;
    dwDefineID: DWORD;
    dwFlags: DWORD;                     // SIMCONNECT_DATA_REQUEST_FLAG
    dwentrynumber: DWORD;
      // if multiple objects returned, this is number <entrynumber> out of <outof>.
    dwoutof: DWORD;                     // note: starts with 1, not 0.
    dwDefineCount: DWORD; // data count (number of datums, *not* byte count)
    dwData: DWORD; // data begins here, dwDefineCount data items
  end;
  TSimConnectRecvClientData = SIMCONNECT_RECV_CLIENT_DATA;
  PSimConnectRecvClientData = PSIMCONNECT_RECV_CLIENT_DATA;

  // when dwID == SIMCONNECT_RECV_ID_WEATHER_OBSERVATION
  PSIMCONNECT_RECV_WEATHER_OBSERVATION = ^SIMCONNECT_RECV_WEATHER_OBSERVATION;
  SIMCONNECT_RECV_WEATHER_OBSERVATION = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    szMetar: array[0..0] of Char;
      // Variable length String whose maximum size is MAX_METAR_LENGTH
  end;
  TSimConnectRecvWeatherObservation = SIMCONNECT_RECV_WEATHER_OBSERVATION;
  PSimConnectRecvWeatherObservation = PSIMCONNECT_RECV_WEATHER_OBSERVATION;

  // when dwID == SIMCONNECT_RECV_ID_CLOUD_STATE
  PSIMCONNECT_RECV_CLOUD_STATE = ^SIMCONNECT_RECV_CLOUD_STATE;
  SIMCONNECT_RECV_CLOUD_STATE = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    rgbData: array[0..0] of byte;
  end;
  TSimConnectRecvCloudState = SIMCONNECT_RECV_CLOUD_STATE;
  PSimConnectRecvCloudState = PSIMCONNECT_RECV_CLOUD_STATE;

  // when dwID == SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID
  PSIMCONNECT_RECV_ASSIGNED_OBJECT_ID = ^SIMCONNECT_RECV_ASSIGNED_OBJECT_ID;
  SIMCONNECT_RECV_ASSIGNED_OBJECT_ID = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwObjectID: DWORD;
  end;
  TSimConnectRecvAssignedObjectId = SIMCONNECT_RECV_ASSIGNED_OBJECT_ID;
  PSimConnectRecvAssignedObjectId = PSIMCONNECT_RECV_ASSIGNED_OBJECT_ID;

  // when dwID == SIMCONNECT_RECV_ID_RESERVED_KEY
  PSIMCONNECT_RECV_RESERVED_KEY = ^SIMCONNECT_RECV_RESERVED_KEY;
  SIMCONNECT_RECV_RESERVED_KEY = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    szChoiceReserved: array[0..29] of Char;
    szReservedKey: array[0..49] of Char;
  end;
  TSimConnectRecvReservedKey = SIMCONNECT_RECV_RESERVED_KEY;
  PSimConnectRecvReservedKey = PSIMCONNECT_RECV_RESERVED_KEY;

  // when dwID == SIMCONNECT_RECV_ID_SYSTEM_STATE
  PSIMCONNECT_RECV_SYSTEM_STATE = ^SIMCONNECT_RECV_SYSTEM_STATE;
  SIMCONNECT_RECV_SYSTEM_STATE = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwInteger: DWORD;
    fFloat: single;
    szString: array[0..255] of Char;
  end;
  TSimConnectRecvSystemState = SIMCONNECT_RECV_SYSTEM_STATE;
  PSimConnectRecvSystemState = PSIMCONNECT_RECV_SYSTEM_STATE;

  PSIMCONNECT_RECV_CUSTOM_ACTION = ^SIMCONNECT_RECV_CUSTOM_ACTION;
  SIMCONNECT_RECV_CUSTOM_ACTION = packed record {SIMCONNECT_RECV_EVENT}
    // "Inherits" SIMCONNECT_RECV_EVENT
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
    // End of Inherit
    guidInstanceId: TGUID; // Instance id of the action that executed
    dwWaitForCompletion: DWORD;         // Wait for completion flag on the action
    szPayLoad: array[0..0] of Char;
      // Variable length String payload associated with the mission action.
  end;
  TSimConnectRecvCustomAction = SIMCONNECT_RECV_CUSTOM_ACTION;
  PSimConnectRecvCustomAction = PSIMCONNECT_RECV_CUSTOM_ACTION;

  // SP1A additions
  PSIMCONNECT_RECV_EVENT_WEATHER_MODE = ^SIMCONNECT_RECV_EVENT_WEATHER_MODE;
  SIMCONNECT_RECV_EVENT_WEATHER_MODE = packed record {SIMCONNECT_RECV_EVENT}
    // "Inherits" SIMCONNECT_RECV_EVENT
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    uGroupID: DWORD;
    uEventID: DWORD;
    dwData: DWORD;                      // uEventID-dependent context
    // End of Inherit
    // No event specific data - the new weather mode is in the base structure dwData member.
  end;
  TSimConnectRecvEventWeatherMode = SIMCONNECT_RECV_EVENT_WEATHER_MODE;
  PSimConnectRecvEventWeatherMode = PSIMCONNECT_RECV_EVENT_WEATHER_MODE;

  // SIMCONNECT_RECV_FACILITIES_LIST
  PSIMCONNECT_RECV_FACILITIES_LIST = ^SIMCONNECT_RECV_FACILITIES_LIST;
  SIMCONNECT_RECV_FACILITIES_LIST = packed record {SIMCONNECT_RECV}
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    dwEntryNumber: DWORD; // when the array of items is too big for one send, which send this is (0..dwOutOf-1)
    dwOutOf: DWORD; // total number of transmissions the list is chopped into
  end;
  TSimConnectRecvFaciltiesList = SIMCONNECT_RECV_FACILITIES_LIST;
  PSimConnectRecvFaciltiesList = PSIMCONNECT_RECV_FACILITIES_LIST;

  // SIMCONNECT_DATA_FACILITY_AIRPORT
  PSIMCONNECT_DATA_FACILITY_AIRPORT = ^SIMCONNECT_DATA_FACILITY_AIRPORT;
  SIMCONNECT_DATA_FACILITY_AIRPORT = packed record
    Icao: array[0..8] of char;          // ICAO of the object
    Latitude: double;                   // degrees
    Longitude: double;                  // degrees
    Altitude: double;                   // meters
  end;
  TSimConnectDataFacilityAirport = SIMCONNECT_DATA_FACILITY_AIRPORT;
  PSimConnectDataFacilityAirport = PSIMCONNECT_DATA_FACILITY_AIRPORT;

  // SIMCONNECT_RECV_AIRPORT_LIST
  PSIMCONNECT_RECV_AIRPORT_LIST = ^SIMCONNECT_RECV_AIRPORT_LIST;
  SIMCONNECT_RECV_AIRPORT_LIST = packed record {SIMCONNECT_RECV_FACILITIES_LIST}
    // "Inherits" SIMCONNECT_RECV_FACILITIES_LIST
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of nested Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    dwEntryNumber: DWORD; // when the array of items is too big for one send, which send this is (0..dwOutOf-1)
    dwOutOf: DWORD; // total number of transmissions the list is chopped into
    // End of Inherit
    rgData: array[0..0] of SIMCONNECT_DATA_FACILITY_AIRPORT;
  end;
  TSimConnectRecvAirportsList = SIMCONNECT_RECV_AIRPORT_LIST;
  PSimConnectRecvAirportsList = PSIMCONNECT_RECV_AIRPORT_LIST;

  // SIMCONNECT_DATA_FACILITY_WAYPOINT
  PSIMCONNECT_DATA_FACILITY_WAYPOINT = ^SIMCONNECT_DATA_FACILITY_WAYPOINT;
  SIMCONNECT_DATA_FACILITY_WAYPOINT = packed record  {SIMCONNECT_DATA_FACILITY_AIRPORT}
    // "inherits" SIMCONNECT_DATA_FACILITY_AIRPORT
    Icao: array[0..8] of char;          // ICAO of the object
    Latitude: double;                   // degrees
    Longitude: double;                  // degrees
    Altitude: double;                   // meters
    // End of Inherit
    fMagVar: single;                    // Magvar in degrees
  end;
  TSimConnectDataFacilityWaypoint = SIMCONNECT_DATA_FACILITY_WAYPOINT;
  PSimConnectDataFacilityWaypoint = PSIMCONNECT_DATA_FACILITY_WAYPOINT;

  // SIMCONNECT_RECV_WAYPOINT_LIST
  PSIMCONNECT_RECV_WAYPOINT_LIST = ^SIMCONNECT_RECV_WAYPOINT_LIST;
  SIMCONNECT_RECV_WAYPOINT_LIST = packed record {SIMCONNECT_RECV_FACILITIES_LIST}
    // "Inherits" SIMCONNECT_RECV_FACILITIES_LIST
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of nested Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    dwEntryNumber: DWORD; // when the array of items is too big for one send, which send this is (0..dwOutOf-1)
    dwOutOf: DWORD; // total number of transmissions the list is chopped into
    // End of Inherit
    rgData: array[0..0] of SIMCONNECT_DATA_FACILITY_WAYPOINT;
  end;
  TSimConnectRecvWaypointList = SIMCONNECT_RECV_WAYPOINT_LIST;
  PSimConnectRecvWaypointList = PSIMCONNECT_RECV_WAYPOINT_LIST;

  // SIMCONNECT_DATA_FACILITY_NDB
  PSIMCONNECT_DATA_FACILITY_NDB = ^SIMCONNECT_DATA_FACILITY_NDB;
  SIMCONNECT_DATA_FACILITY_NDB = packed record  {SIMCONNECT_DATA_FACILITY_WAYPOINT}
    // "Inherits" SIMCONNECT_DATA_FACILITY_WAYPOINT
    // "Inherits" SIMCONNECT_DATA_FACILITY_AIRPORT
    Icao: array[0..8] of char;          // ICAO of the object
    Latitude: double;                   // degrees
    Longitude: double;                  // degrees
    Altitude: double;                   // meters
    // End of Nested Inherit
    fMagVar: single;                    // Magvar in degrees
    // End of Inherit
    fFrequency: DWORD;                  // frequency in Hz
  end;
  TSimConnectDataFacilityNdb = SIMCONNECT_DATA_FACILITY_NDB;
  PSimConnectDataFacilityNdb = PSIMCONNECT_DATA_FACILITY_NDB;

  // SIMCONNECT_RECV_NDB_LIST
  PSIMCONNECT_RECV_NDB_LIST = ^SIMCONNECT_RECV_NDB_LIST;
  SIMCONNECT_RECV_NDB_LIST = packed record {SIMCONNECT_RECV_FACILITIES_LIST}
    // "Inherits" SIMCONNECT_RECV_FACILITIES_LIST
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of nested Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    dwEntryNumber: DWORD; // when the array of items is too big for one send, which send this is (0..dwOutOf-1)
    dwOutOf: DWORD; // total number of transmissions the list is chopped into
    // End of Inherit
    rgData: array[0..0] of SIMCONNECT_DATA_FACILITY_NDB;
  end;
  TSimConnectRecvNdbList = SIMCONNECT_RECV_NDB_LIST;
  PSimConnectRecvNdbList = PSIMCONNECT_RECV_NDB_LIST;

  // SIMCONNECT_DATA_FACILITY_VOR
  PSIMCONNECT_DATA_FACILITY_VOR = ^SIMCONNECT_DATA_FACILITY_VOR;
  SIMCONNECT_DATA_FACILITY_VOR = packed record {SIMCONNECT_DATA_FACILITY_NDB}
    // "Inherits" SIMCONNECT_DATA_FACILITY_NDB
    // "Inherits" SIMCONNECT_DATA_FACILITY_WAYPOINT
    // "Inherits" SIMCONNECT_DATA_FACILITY_AIRPORT
    Icao: array[0..8] of char;          // ICAO of the object
    Latitude: double;                   // degrees
    Longitude: double;                  // degrees
    Altitude: double;                   // meters
    // End of Nested Inherit
    fMagVar: single;                    // Magvar in degrees
    // End of Nested Inherit
    fFrequency: DWORD;                  // frequency in Hz
    // End of Inherit
    Flags: DWORD;                       // SIMCONNECT_VOR_FLAGS
    fLocalizer: single;                 // Localizer in degrees
    GlideLat: double; // Glide Slope Location (deg, deg, meters)
    GlideLon: double;
    GlideAlt: double;
    fGlideSlopeAngle: single;           // Glide Slope in degrees
  end;
  TSimConnectDataFacilityVor = SIMCONNECT_DATA_FACILITY_VOR;
  PSimConnectDataFacilityVor = PSIMCONNECT_DATA_FACILITY_VOR;

  // SIMCONNECT_RECV_VOR_LIST
  PSIMCONNECT_RECV_VOR_LIST = ^SIMCONNECT_RECV_VOR_LIST;
  SIMCONNECT_RECV_VOR_LIST = packed record {SIMCONNECT_RECV_FACILITIES_LIST}
    // "Inherits" SIMCONNECT_RECV_FACILITIES_LIST
    // "Inherits" SIMCONNECT_RECV
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    // End of nested Inherit
    dwRequestID: DWORD;
    dwArraySize: DWORD;
    dwEntryNumber: DWORD; // when the array of items is too big for one send, which send this is (0..dwOutOf-1)
    dwOutOf: DWORD; // total number of transmissions the list is chopped into
    // End of Inherit
    rgData: array[0..0] of SIMCONNECT_DATA_FACILITY_VOR;
  end;
  TSimConnectRecvVorList = SIMCONNECT_RECV_VOR_LIST;
  PSimConnectRecvVorList = PSIMCONNECT_RECV_VOR_LIST;

  // End of SP1A additions

  // SIMCONNECT_DATAType_INITPOSITION
  PSIMCONNECT_DATA_INITPOSITION = ^SIMCONNECT_DATA_INITPOSITION;
  SIMCONNECT_DATA_INITPOSITION = packed record
    Latitude: double;                   // degrees
    Longitude: double;                  // degrees
    Altitude: double;                   // feet
    Pitch: double;                      // degrees
    Bank: double;                       // degrees
    Heading: double;                    // degrees
    OnGround: DWORD;                    // 1=force to be on the ground
    Airspeed: DWORD;                    // knots
  end;
  TSimConnectDataInitPosition = SIMCONNECT_DATA_INITPOSITION;
  PSimConnectDataInitPosition = PSIMCONNECT_DATA_INITPOSITION;

  // SIMCONNECT_DATAType_MARKERSTATE
  SIMCONNECT_DATA_MARKERSTATE = packed record
    szMarkerName: array[0..63] of Char;
    dwMarkerState: DWORD;
  end;
  TSimConnectDataMarkerState = SIMCONNECT_DATA_MARKERSTATE;

  // SIMCONNECT_DATAType_WAYPOINT
  SIMCONNECT_DATA_WAYPOINT = packed record
    Latitude: double;
    Longitude: double;
    Altitude: double;
    Flags: LongInt;
    ktsSpeed: double;
    percentThrottle: double;
  end;
  TSimConnectDataWaypoint = SIMCONNECT_DATA_WAYPOINT;

  // SIMCONNECT_DATA_LATLONALT
  SIMCONNECT_DATA_LATLONALT = packed record
    Latitude: double;
    Longitude: double;
    Altitude: double;
  end;
  TSimConnectDataLatLonAlt = SIMCONNECT_DATA_LATLONALT;

  // SIMCONNECT_DATA_XYZ
  SIMCONNECT_DATA_XYZ = packed record
    x: double;
    y: double;
    z: double;
  end;
  TSimConnectDataXYZ = SIMCONNECT_DATA_XYZ;

  // A procedure to pass to CallDispatch (SDK standard method)
  TDispatchProc = procedure(pData: PSimConnectRecv; cbData: DWORD; pContext:
    Pointer); stdcall;
  // A method to pass to CallDispatch (allows direct link to method of form
  // Note that overloading causes a compiler error, so this version is
  // used in CallDispatchMethod (which calls the same DLL entry)
  TDispatchMethod = procedure(pData: PSimConnectRecv; cbData: DWORD; pContext:
    Pointer) of object; stdcall;

  //----------------------------------------------------------------------------
  //        Function Declarations
  //----------------------------------------------------------------------------

function SimConnect_AddToDataDefinition(
  hSimConnect: THandle;
  DefineID: SIMCONNECT_DATA_DEFINITION_ID;
  DatumName: string;
  UnitsName: string;
  DatumType: SIMCONNECT_DATAType = SIMCONNECT_DATAType_FLOAT64;
  fEpsilon: Single = 0;
  DatumID: DWORD = SIMCONNECT_UNUSED
  ): HRESULT; StdCall;

function SimConnect_Close(
  hSimConnect: THandle
  ): HRESULT; StdCall;

function SimConnect_FlightLoad(
  hSimConnect: THandle;
  const szFileName: PChar
  ): HRESULT; StdCall;

function SimConnect_FlightSave(
  hSimConnect: THandle;
  const szFileName: PChar;
  const szDescription: PChar;
  Flags: DWORD
  ): HRESULT; StdCall;

function SimConnect_GetNextDispatch(
  hSimConnect: THandle;
  var ppData: PSimConnectRecv;
  var pcbData: DWORD
  ): HRESULT; StdCall;

function SimConnect_Open(
  var phSimConnect: THandle;
  szName: PChar;
  hWnd: HWnd;
  UserEventWin32: DWORD;
  hEventHandle: THandle;
  ConfigIndex: DWORD
  ): HRESULT; StdCall;

function SimConnect_RequestDataOnSimObject(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  DefineID: SIMCONNECT_DATA_DEFINITION_ID;
  ObjectID: SIMCONNECT_OBJECT_ID;
  Period: SIMCONNECT_PERIOD;
  Flags: SIMCONNECT_DATA_REQUEST_FLAG = 0;
  origin: DWORD = 0;
  interval: DWORD = 0;
  limit: DWORD = 0
  ): HRESULT; StdCall;

function SimConnect_CallDispatch(
  hSimConnect: THandle;
  pfcnDispatch: TDispatchProc;
  pContext: Pointer
  ): HRESULT; StdCall;

function SimConnect_CallDispatchMethod(
  hSimConnect: THandle;
  pfcnDispatch: TDispatchMethod;
  pContext: Pointer
  ): HRESULT; StdCall;

function SimConnect_RequestDataOnSimObjectType(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  DefineID: SIMCONNECT_DATA_DEFINITION_ID;
  dwRadiusMeters: DWORD;
  ObjectType: SIMCONNECT_SIMOBJECT_Type
  ): HRESULT; StdCall;

function SimConnect_SetInputGroupState(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_INPUT_GROUP_ID;
  dwState: DWORD
  ): HRESULT; StdCall;

function SimConnect_SetDataOnSimObject(
  hSimConnect: THandle;
  DefineID: SIMCONNECT_DATA_DEFINITION_ID;
  ObjectID: SIMCONNECT_OBJECT_ID;
  Flags: SIMCONNECT_DATA_SET_FLAG;
  ArrayCount: DWORD;
  cbUnitSize: DWORD;
  pDataSet: Pointer
  ): HRESULT; StdCall;

function SimConnect_MapClientEventToSimEvent(
  hSimConnect: THandle;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  EventName: string = ''
  ): HRESULT; StdCall;

function SimConnect_MapInputEventToClientEvent(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_INPUT_GROUP_ID;
  const szInputDefinition: PChar;
  DownEventID: SIMCONNECT_CLIENT_EVENT_ID;
  DownValue: DWORD = 0;
  UpEventID: SIMCONNECT_CLIENT_EVENT_ID = Ord(SIMCONNECT_UNUSED);
  UpValue: DWORD = 0;
  bMaskable: BOOL = False
  ): HRESULT; StdCall;

function SimConnect_AddClientEventToNotificationGroup(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  bMaskable: BOOL = False
  ): HRESULT; StdCall;

function SimConnect_SetNotificationGroupPriority(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID;
  uPriority: Cardinal
  ): HRESULT; StdCall;

function SimConnect_RetrieveString(
  pData: PSimConnectRecv;
  cbData: Cardinal;
  pStringV: PChar;
  var pszString: PChar;
  var pcbString: Cardinal
  ): HRESULT; StdCall;

function SimConnect_CameraSetRelative6DOF(
  hSimConnect: THandle;
  fDeltaX: Single;
  fDeltaY: Single;
  fDeltaZ: Single;
  fPitchDeg: Single;
  fBankDeg: Single;
  fHeadingDeg: Single
  ): HRESULT; StdCall;

function SimConnect_SetInputGroupPriority(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_INPUT_GROUP_ID;
  uPriority: Cardinal
  ): HRESULT; StdCall;

function SimConnect_RemoveClientEvent(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID;
  EventID: SIMCONNECT_CLIENT_EVENT_ID
  ): HRESULT; StdCall;

function SimConnect_MenuDeleteItem(
  hSimConnect: THandle;
  MenuEventID: SIMCONNECT_CLIENT_EVENT_ID
  ): HRESULT; StdCall;

function SimConnect_MenuAddItem(
  hSimConnect: THandle;
  const szMenuItem: PChar;
  MenuEventID: SIMCONNECT_CLIENT_EVENT_ID;
  dwData: Cardinal
  ): HRESULT; StdCall;

function SimConnect_RequestReservedKey(
  hSimConnect: THandle;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  szKeyChoice1: string;
  szKeyChoice2: string;
  szKeyChoice3: string
  ): HRESULT; StdCall;

function SimConnect_TransmitClientEvent(
  hSimConnect: THandle;
  ObjectID: SIMCONNECT_OBJECT_ID;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  dwData: Cardinal;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID;
  Flags: SIMCONNECT_EVENT_FLAG
  ): HRESULT; StdCall;

function SimConnect_WeatherRequestObservationAtNearestStation(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  lat: Single;
  lon: Single
  ): HRESULT; StdCall;

function SimConnect_WeatherRequestObservationAtStation(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  const szICAO: PChar
  ): HRESULT; StdCall;

function SimConnect_AICreateSimulatedObject(
  hSimConnect: THandle;
  const szContainerTitle: PChar;
  InitPos: SIMCONNECT_DATA_INITPOSITION;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_AICreateNonATCAircraft(
  hSimConnect: THandle;
  const szContainerTitle: PChar;
  const szTailNumber: PChar;
  InitPos: SIMCONNECT_DATA_INITPOSITION;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_AISetAircraftFlightPlan(
  hSimConnect: THandle;
  ObjectID: SIMCONNECT_OBJECT_ID;
  const szFlightPlanPath: PChar;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_AICreateParkedATCAircraft(
  hSimConnect: THandle;
  const szContainerTitle: PChar;
  const szTailNumber: PChar;
  const szAirportID: PChar;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_SubscribeToSystemEvent(
  hSimConnect: THandle;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  const SystemEventName: PChar
  ): HRESULT; StdCall;

function SimConnect_AICreateEnrouteATCAircraft(
  hSimConnect: THandle;
  const szContainerTitle: PChar;
  const szTailNumber: PChar;
  iFlightNumber: Integer;
  const szFlightPlanPath: PChar;
  dFlightPlanPosition: Double;
  bTouchAndGo: Bool;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_RequestSystemState(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  const szState: PChar
  ): HRESULT; StdCall;

function SimConnect_SetSystemState(
  hSimConnect: THandle;
  const szState: PChar;
  dwInteger: Cardinal;
  fFloat: Single;
  const szString: PChar
  ): HRESULT; StdCall;

function SimConnect_GetLastSentPacketID(
  hSimConnect: THandle;
  var pdwSendID: Cardinal
  ): HRESULT; StdCall;

function SimConnect_MenuAddSubItem(
  hSimConnect: THandle;
  MenuEventID: SIMCONNECT_CLIENT_EVENT_ID;
  const szMenuItem: PChar;
  SubMenuEventID: SIMCONNECT_CLIENT_EVENT_ID;
  dwData: Cardinal
  ): HRESULT; StdCall;

function SimConnect_MenuDeleteSubItem(
  hSimConnect: THandle;
  MenuEventID: SIMCONNECT_CLIENT_EVENT_ID;
  const SubMenuEventID: SIMCONNECT_CLIENT_EVENT_ID
  ): HRESULT; StdCall;

function SimConnect_RequestResponseTimes(
  hSimConnect: THandle;
  nCount: Cardinal;
  var fElapsedSeconds: Single
  ): HRESULT; StdCall;

function SimConnect_FlightPlanLoad(
  hSimConnect: THandle;
  const szFileName: PChar
  ): HRESULT; StdCall;

// BEWARE!!!!!
// Unlike the C version these must not pass the GUID as a "const"
// In Delphi that results in passing a pointer, not a value, and it will fail
function SimConnect_ExecuteMissionAction(
  hSimConnect: THandle;
  guidInstanceId: TGUID
  ): HRESULT; StdCall;

function SimConnect_CompleteCustomMissionAction(
  hSimConnect: THandle;
  guidInstanceId: TGUID
  ): HRESULT; StdCall;

//-------------------------
// 							Not	checked
//-------------------------

function SimConnect_SetSystemEventState(
  hSimConnect: THandle;
  EventID: SIMCONNECT_CLIENT_EVENT_ID;
  dwState: SIMCONNECT_STATE
  ): HRESULT; StdCall;

function SimConnect_ClearNotificationGroup(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID
  ): HRESULT; StdCall;

function SimConnect_RequestNotificationGroup(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_NOTIFICATION_GROUP_ID;
  dwReserved: Cardinal;
  Flags: Cardinal
  ): HRESULT; StdCall;

function SimConnect_ClearDataDefinition(
  hSimConnect: THandle;
  DefineID: SIMCONNECT_DATA_DEFINITION_ID
  ): HRESULT; StdCall;

function SimConnect_RemoveInputEvent(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_INPUT_GROUP_ID;
  const szInputDefinition: PChar
  ): HRESULT; StdCall;

function SimConnect_ClearInputGroup(
  hSimConnect: THandle;
  GroupID: SIMCONNECT_INPUT_GROUP_ID
  ): HRESULT; StdCall;

function SimConnect_UnsubscribeFromSystemEvent(
  hSimConnect: THandle;
  EventID: SIMCONNECT_CLIENT_EVENT_ID
  ): HRESULT; StdCall;

function SimConnect_WeatherRequestInterpolatedObservation(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  lat: Single;
  lon: Single;
  alt: Single
  ): HRESULT; StdCall;

function SimConnect_WeatherCreateStation(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  const szICAO: PChar;
  const szName: PChar;
  lat: Single;
  lon: Single;
  alt: Single
  ): HRESULT; StdCall;

function SimConnect_WeatherRemoveStation(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  const szICAO: PChar
  ): HRESULT; StdCall;

function SimConnect_WeatherSetObservation(
  hSimConnect: THandle;
  Seconds: Cardinal;
  const szMETAR: PChar
  ): HRESULT; StdCall;

function SimConnect_WeatherSetModeServer(
  hSimConnect: THandle;
  dwPort: Cardinal;
  dwSeconds: Cardinal
  ): HRESULT; StdCall;

function SimConnect_WeatherSetModeTheme(
  hSimConnect: THandle;
  const szThemeName: PChar
  ): HRESULT; StdCall;

function SimConnect_WeatherSetModeGlobal(
  hSimConnect: THandle
  ): HRESULT; StdCall;

function SimConnect_WeatherSetModeCustom(
  hSimConnect: THandle
  ): HRESULT; StdCall;

function SimConnect_WeatherSetDynamicUpdateRate(
  hSimConnect: THandle;
  dwRate: Cardinal
  ): HRESULT; StdCall;

function SimConnect_WeatherRequestCloudState(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  minLat: Single;
  minLon: Single;
  minAlt: Single;
  maxLat: Single;
  maxLon: Single;
  maxAlt: Single;
  dwFlags: LongInt
  ): HRESULT; StdCall;

function SimConnect_WeatherCreateThermal(
  hSimConnect: THandle;
  RequestID: SIMCONNECT_DATA_REQUEST_ID;
  lat: Single;
  lon: Single;
  alt: Single;
  radius: Single;
  height: Single;
  coreRate: Single;
  coreTurbulence: Single;
  sinkRate: Single;
  sinkTurbulence: Single;
  coreSize: Single;
  coreTransitionSize: Single;
  sinkLayerSize: Single;
  sinkTransitionSize: Single
  ): HRESULT; StdCall;

function SimConnect_WeatherRemoveThermal(
  hSimConnect: THandle;
  ObjectID: SIMCONNECT_OBJECT_ID
  ): HRESULT; StdCall;

function SimConnect_AIReleaseControl(
  hSimConnect: THandle;
  ObjectID: SIMCONNECT_OBJECT_ID;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_AIRemoveObject(
  hSimConnect: THandle;
  ObjectID: SIMCONNECT_OBJECT_ID;
  RequestID: SIMCONNECT_DATA_REQUEST_ID
  ): HRESULT; StdCall;

function SimConnect_InsertString(
  pDest: PChar;
  cbDest: Cardinal;
  var ppEnd: Pointer;
  var pcbStringV: Cardinal;
  const pSource: PChar
  ): HRESULT; StdCall;

function SimConnect_MapClientDataNameToID(
  hSimConnect: THandle;
  const szClientDataName: PChar;
  ClientDataID: SIMCONNECT_CLIENT_DATA_ID
  ): HRESULT; StdCall;

function SimConnect_CreateClientData(
  hSimConnect: THandle;
  ClientDataID: SIMCONNECT_CLIENT_DATA_ID;
  dwSize: Cardinal;
  Flags: SIMCONNECT_CREATE_CLIENT_DATA_FLAG
  ): HRESULT; StdCall;

// Modified for SP1A
function SimConnect_AddToClientDataDefinition(
    hSimConnect: THandle;
    DefineID: SIMCONNECT_CLIENT_DATA_DEFINITION_ID;
    dwOffset: Cardinal;
    dwSizeOrType: Cardinal;
    fEpsilon : single = 0;
    DatumID: DWORD = SIMCONNECT_UNUSED
    ): HRESULT; StdCall;

function SimConnect_ClearClientDataDefinition(
  hSimConnect: THandle;
  DefineID: SIMCONNECT_CLIENT_DATA_DEFINITION_ID
  ): HRESULT; StdCall;

function SimConnect_RequestClientData(
    hSimConnect: THandle;
    ClientDataID: SIMCONNECT_CLIENT_DATA_ID;
    RequestID: SIMCONNECT_DATA_REQUEST_ID;
    DefineID: SIMCONNECT_CLIENT_DATA_DEFINITION_ID;
    Period : SIMCONNECT_CLIENT_DATA_PERIOD = SIMCONNECT_CLIENT_DATA_PERIOD_ONCE;
    Flags : SIMCONNECT_CLIENT_DATA_REQUEST_FLAG  = 0;
    origin  : DWORD= 0;
    interval : DWORD= 0;
    limit : DWORD= 0
    ): HRESULT; StdCall;

function SimConnect_SetClientData(
    hSimConnect: THandle;
    ClientDataID: SIMCONNECT_CLIENT_DATA_ID;
    DefineID: SIMCONNECT_CLIENT_DATA_DEFINITION_ID;
    dwReserved: Cardinal;
    cbUnitSize: Cardinal;
    pDataSet: Pointer
    ): HRESULT; StdCall;

  // SP1A Additions
function SimConnect_Text(
    hSimConnect: THandle;
    TextType: TSimConnectTextType;
    fTimeSeconds: single;
    EventID: SIMCONNECT_CLIENT_EVENT_ID;
    cbUnitSize: DWORD;
    pDataSet: Pointer
    ): HRESULT; StdCall;

function SimConnect_SubscribeToFacilities(
    hSimConnect: THandle;
    ListType: SIMCONNECT_FACILITY_LIST_TYPE;
    RequestID: SIMCONNECT_DATA_REQUEST_ID
    ): HRESULT; StdCall;

function SimConnect_UnsubscribeToFacilities(
    hSimConnect: THandle;
    ListType: SIMCONNECT_FACILITY_LIST_TYPE
    ): HRESULT; StdCall;

function SimConnect_RequestFacilitiesList(
    hSimConnect: THandle;
    ListType: SIMCONNECT_FACILITY_LIST_TYPE;
    RequestID: SIMCONNECT_DATA_REQUEST_ID
    ): HRESULT; StdCall;
  // End of SP1A Additions

implementation

function SimConnect_MapClientEventToSimEvent; external 'SIMCONNECT.DLL';
function SimConnect_TransmitClientEvent; external 'SIMCONNECT.DLL';
function SimConnect_SetSystemEventState; external 'SIMCONNECT.DLL';
function SimConnect_AddClientEventToNotificationGroup; external
'SIMCONNECT.DLL';
function SimConnect_RemoveClientEvent; external 'SIMCONNECT.DLL';
function SimConnect_SetNotificationGroupPriority; external 'SIMCONNECT.DLL';
function SimConnect_ClearNotificationGroup; external 'SIMCONNECT.DLL';
function SimConnect_RequestNotificationGroup; external 'SIMCONNECT.DLL';
function SimConnect_AddToDataDefinition; external 'SIMCONNECT.DLL';
function SimConnect_ClearDataDefinition; external 'SIMCONNECT.DLL';
function SimConnect_RequestDataOnSimObject; external 'SIMCONNECT.DLL';
function SimConnect_RequestDataOnSimObjectType; external 'SIMCONNECT.DLL';
function SimConnect_SetDataOnSimObject; external 'SIMCONNECT.DLL';
function SimConnect_MapInputEventToClientEvent; external 'SIMCONNECT.DLL';
function SimConnect_SetInputGroupPriority; external 'SIMCONNECT.DLL';
function SimConnect_RemoveInputEvent; external 'SIMCONNECT.DLL';
function SimConnect_ClearInputGroup; external 'SIMCONNECT.DLL';
function SimConnect_SetInputGroupState; external 'SIMCONNECT.DLL';
function SimConnect_RequestReservedKey; external 'SIMCONNECT.DLL';
function SimConnect_SubscribeToSystemEvent; external 'SIMCONNECT.DLL';
function SimConnect_UnsubscribeFromSystemEvent; external 'SIMCONNECT.DLL';
function SimConnect_WeatherRequestInterpolatedObservation; external
'SIMCONNECT.DLL';
function SimConnect_WeatherRequestObservationAtStation; external
'SIMCONNECT.DLL';
function SimConnect_WeatherRequestObservationAtNearestStation; external
'SIMCONNECT.DLL';
function SimConnect_WeatherCreateStation; external 'SIMCONNECT.DLL';
function SimConnect_WeatherRemoveStation; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetObservation; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetModeServer; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetModeTheme; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetModeGlobal; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetModeCustom; external 'SIMCONNECT.DLL';
function SimConnect_WeatherSetDynamicUpdateRate; external 'SIMCONNECT.DLL';
function SimConnect_WeatherRequestCloudState; external 'SIMCONNECT.DLL';
function SimConnect_WeatherCreateThermal; external 'SIMCONNECT.DLL';
function SimConnect_WeatherRemoveThermal; external 'SIMCONNECT.DLL';
function SimConnect_AICreateParkedATCAircraft; external 'SIMCONNECT.DLL';
function SimConnect_AICreateEnrouteATCAircraft; external 'SIMCONNECT.DLL';
function SimConnect_AICreateNonATCAircraft; external 'SIMCONNECT.DLL';
function SimConnect_AICreateSimulatedObject; external 'SIMCONNECT.DLL';
function SimConnect_AIReleaseControl; external 'SIMCONNECT.DLL';
function SimConnect_AIRemoveObject; external 'SIMCONNECT.DLL';
function SimConnect_AISetAircraftFlightPlan; external 'SIMCONNECT.DLL';
function SimConnect_ExecuteMissionAction; external 'SIMCONNECT.DLL';
function SimConnect_CompleteCustomMissionAction; external 'SIMCONNECT.DLL';
function SimConnect_Close; external 'SIMCONNECT.DLL';
function SimConnect_RetrieveString; external 'SIMCONNECT.DLL';
function SimConnect_GetLastSentPacketID; external 'SIMCONNECT.DLL';
function SimConnect_Open; external 'SIMCONNECT.DLL';
function SimConnect_CallDispatch; external 'SIMCONNECT.DLL';
// This effectively overloads CallDispatch to allow direct access to methods
function SimConnect_CallDispatchMethod; external 'SIMCONNECT.DLL' name
'SimConnect_CallDispatch';
function SimConnect_GetNextDispatch; external 'SIMCONNECT.DLL';
function SimConnect_RequestResponseTimes; external 'SIMCONNECT.DLL';
function SimConnect_InsertString; external 'SIMCONNECT.DLL';
function SimConnect_CameraSetRelative6DOF; external 'SIMCONNECT.DLL';
function SimConnect_MenuAddItem; external 'SIMCONNECT.DLL';
function SimConnect_MenuDeleteItem; external 'SIMCONNECT.DLL';
function SimConnect_MenuAddSubItem; external 'SIMCONNECT.DLL';
function SimConnect_MenuDeleteSubItem; external 'SIMCONNECT.DLL';
function SimConnect_RequestSystemState; external 'SIMCONNECT.DLL';
function SimConnect_SetSystemState; external 'SIMCONNECT.DLL';
function SimConnect_MapClientDataNameToID; external 'SIMCONNECT.DLL';
function SimConnect_CreateClientData; external 'SIMCONNECT.DLL';
function SimConnect_AddToClientDataDefinition; external 'SIMCONNECT.DLL';
function SimConnect_ClearClientDataDefinition; external 'SIMCONNECT.DLL';
function SimConnect_RequestClientData; external 'SIMCONNECT.DLL';
function SimConnect_SetClientData; external 'SIMCONNECT.DLL';
function SimConnect_FlightLoad; external 'SIMCONNECT.DLL';
function SimConnect_FlightSave; external 'SIMCONNECT.DLL';
function SimConnect_FlightPlanLoad; external 'SIMCONNECT.DLL';
// SP1A Additions
function SimConnect_Text; external 'SIMCONNECT.DLL';
function SimConnect_SubscribeToFacilities; external 'SIMCONNECT.DLL';
function SimConnect_UnsubscribeToFacilities; external 'SIMCONNECT.DLL';
function SimConnect_RequestFacilitiesList; external 'SIMCONNECT.DLL';
// End of SP1A Additions

end.

