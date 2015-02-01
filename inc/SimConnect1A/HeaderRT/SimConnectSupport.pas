{ 25/05/2007 10:23:10 (GMT+1:00) > [Akadamia] checked in   }
{ 21/05/2007 17:50:48 (GMT+1:00) > [Akadamia] checked in Updates for SP1A  }
{ 21/05/2007 17:44:13 (GMT+1:00) > [Akadamia] checked out /Updates for SP1A}
{ 21/05/2007 17:16:13 (GMT+1:00) > [Akadamia] checked in Updates for SP1A  }
{ 21/05/2007 13:49:54 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:30:31 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:30:20 (GMT+0:00) > [Akadamia] checked out /}
{-----------------------------------------------------------------------------
 Unit Name: SimConnectSupport
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit SimConnectSupport;

interface
uses
  Windows, SimConnect;

  // Diagnostic constant strings
const
  SimConnectRecvIdNames: array[TSimConnectRecvId] of string = (
    'NULL', 'EXCEPTION', 'OPEN', 'QUIT', 'EVENT', 'EVENT OBJECT ADDREMOVE',
    'EVENT FILENAME', 'EVENT FRAME', 'SIMOBJECT_DATA', 'SIMOBJECT DATA BYType',
    'WEATHER OBSERVATION', 'CLOUD STATE', 'ASSIGNED OBJECT ID', 'RESERVED KEY',
    'CUSTOM ACTION', 'SYSTEM STATE', 'CLIENT DATA', 'EVENT_WEATHER_MODE',
    'AIRPORT LIST', 'VOR LIST', 'NDB LIST', 'WAYPOINT LIST');
  SimConnectExceptionNames: array[TSimConnectException] of string = (
    'NONE', 'ERROR', 'SIZE MISMATCH', 'UNRECOGNIZED ID', 'UNOPENED',
    'VERSION MISMATCH', 'TOO MANY GROUPS', 'NAME UNRECOGNIZED',
    'TOO MANY EVENT NAMES', 'EVENT ID DUPLICATE', 'TOO MANY MAPS',
    'TOO MANY OBJECTS', 'TOO MANY REQUESTS', 'WEATHER INVALID PORT',
    'WEATHER INVALID METAR', 'WEATHER UNABLE TO GET OBSERVATION',
    'WEATHER UNABLE TO CREATE STATION', 'WEATHER UNABLE TO REMOVE STATION',
    'INVALID DATA Type', 'INVALID DATA SIZE', 'DATA ERROR', 'INVALID ARRAY',
    'CREATE OBJECT FAILED', 'LOAD FLIGHTPLAN FAILED',
    'OPERATION INVALID FOR OBJECT Type', 'ILLEGAL OPERATION',
    'ALREADY SUBSCRIBED', 'INVALID ENUM', 'DEFINITION ERROR', 'DUPLICATE ID',
    'DATUM ID', 'OUT OF BOUNDS', 'ALREADY CREATED',
    'OBJECT OUTSIDE REALITY BUBBLE', 'OBJECT CONTAINER', 'OBJECT AI',
    'OBJECT ATC', 'OBJECT SCHEDULE');
  SimConnectSimObjectTypeNames: array[TSimConnectSimObjectType] of string = (
    'USER', 'ALL', 'AIRCRAFT', 'HELICOPTER', 'BOAT', 'GROUND');

type
  // Experimental variant record structure which maps all the derived
  // SIMCONNECT_RECV types into single type.
  PSimConnectRecvAll = ^TSimConnectRecvAll;
  TSimConnectRecvAll = record
    dwSize: DWORD;
    dwVersion: DWORD;
    dwID: DWORD;
    case SIMCONNECT_RECV_ID of
      SIMCONNECT_RECV_ID_NULL: ();
      SIMCONNECT_RECV_ID_EXCEPTION: (
        dwException: DWORD;
        dwSendID: DWORD;
        dwIndex: DWORD;
        );
      SIMCONNECT_RECV_ID_OPEN:
      (szApplicationName: array[0..255] of Char;
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
        );
      SIMCONNECT_RECV_ID_QUIT: ();
      SIMCONNECT_RECV_ID_EVENT,
        SIMCONNECT_RECV_ID_EVENT_OBJECT_ADDREMOVE,
        SIMCONNECT_RECV_ID_EVENT_FILENAME,
        SIMCONNECT_RECV_ID_EVENT_FRAME,
        SIMCONNECT_RECV_ID_CUSTOM_ACTION: (
        uGroupID: DWORD;
        uEventID: DWORD;
        dwEventData: DWORD; // uEventID-dependent context (Name changed)
        case SIMCONNECT_RECV_ID of
          SIMCONNECT_RECV_ID_EVENT_OBJECT_ADDREMOVE:
          (eObjType: TSimConnectSimObjectType);
          SIMCONNECT_RECV_ID_EVENT_FILENAME:
          (szFileName: array[0..256 - 1] of Char; // uEventID-dependent context
            dwFlags: DWORD);
          SIMCONNECT_RECV_ID_EVENT_FRAME:
          (FrameRate: single;
            fSimSpeed: single);
          SIMCONNECT_RECV_ID_CUSTOM_ACTION:
          (guidInstanceId: TGUID; // Instance id Of the action that executed
            dwWaitForCompletion: DWORD; // Wait for completion flag on the action
            szPayLoad: array[0..0] of Char;  // Variable length String payload associated with the mission action.
            );
          );
      SIMCONNECT_RECV_ID_SIMOBJECT_DATA,
        SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE,
        SIMCONNECT_RECV_ID_CLIENT_DATA,
        SIMCONNECT_RECV_ID_WEATHER_OBSERVATION,
        SIMCONNECT_RECV_ID_CLOUD_STATE,
        SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID,
        SIMCONNECT_RECV_ID_SYSTEM_STATE: (
        dwRequestID: DWORD;
        case SIMCONNECT_RECV_ID of
          SIMCONNECT_RECV_ID_SIMOBJECT_DATA,
            SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE,
            SIMCONNECT_RECV_ID_CLIENT_DATA,
            SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID: (
            dwObjectID: DWORD;
            case SIMCONNECT_RECV_ID of
              SIMCONNECT_RECV_ID_SIMOBJECT_DATA,
                SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE,
                SIMCONNECT_RECV_ID_CLIENT_DATA: (
                dwDefineID: DWORD;
                dwFlagsRequest: DWORD; // SIMCONNECT_DATA_REQUEST_FLAG (Name Changed)
                dwentrynumber: DWORD; // if multiple objects returned, this is number <entrynumber> out of <outof>.
                dwoutof: DWORD;         // note: starts with 1, not 0.
                dwDefineCount: DWORD; // data count (number of datums, *not* byte count)
                dwData: DWORD; // data begins here, dwDefineCount data items
                );
              SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID: ();
              );
          SIMCONNECT_RECV_ID_WEATHER_OBSERVATION: (
            szMetar: array[0..0] of Char;  // Variable length String whose maximum size is MAX_METAR_LENGTH
            );
          SIMCONNECT_RECV_ID_CLOUD_STATE: (
            dwArraySize: DWORD;
            rgbData: array[0..0] of byte;
            );
          SIMCONNECT_RECV_ID_SYSTEM_STATE: (
            dwInteger: DWORD;
            fFloat: single;
            zString: array[0..255] of Char;
            );
          );
      SIMCONNECT_RECV_ID_RESERVED_KEY: (
        szChoiceReserved: array[0..29] of Char;
        szReservedKey: array[0..49] of Char;
        );
  end;

function SimConnectException(E: PSimConnectRecvException): string;

implementation
uses
  SysUtils;

function SimConnectException(E: PSimConnectRecvException): string;
begin
  Result := Format('Exception: %s (SendID=%d, Index=%d)',
    [SimConnectExceptionNames[TSimConnectException(E^.dwException)],
    E^.dwSendID, E^.dwIndex]);
end;

end.

