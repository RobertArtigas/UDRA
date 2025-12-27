!Assumes C6+
!MG Changes:
! Removed C55 Support in the interest of clarity (especially since _ver_.inc was not shipping and no-one seems to have fixed it, proving no-one is using C55 with this class)
! Moved KeyState Equates to an INCLUDE - and then removed the INCLUDE, as it was never used!
! Removed GlobalAsciiFileName - it limited filenames to just a dozen or so characters
! Changed DebugPrefix from STRING to CSTRING to can avoid CLIP()s when using
! Added GetCategory     PROCEDURE(STRING pCategory),BOOL  !TRUE if found, FALSE if not found
! Added NowToString     PROCEDURE(),STRING
! Split out .Debug to DebugToODS and DebugToFile (adding both new methods)
! Added PurgeUpTo               PROCEDURE(*ST::DebugEventQueue xaQ, REAL xPurgeUpTo)  !Assumes xaQ is sorted by StarDate
!RA Changes:
! Added Methods to do global tracking of how deep in the rabit hole you are
! Added Attribute to keep track of the nested level
!RA Changes:
! TUFO interface added. Helpful in finding out what you are dealing with.
! Combine MG debuger methods and UD methods. Need to support legacy code for both.
! REMOVE: Added SQLCallBackInterface here
! REMOVE: Added FileCallBackInterface here (Code originally created by Mike Hanson)

    OMIT('_EndOfInclude_',_UltimateDebug_)
_UltimateDebug_                         EQUATE(1)
!Region Doc
!------------------------------------
!ClarionLive Ultimate Debug Class
!Category: Clarion Live!
!
!Debug class to send messages to DebugView, available at http://technet.microsoft.com/en-us/sysinternals/bb896647
!
!Code contributed by:
!     Mark Riffey
!     Roberto Artigas
!     Mike Hanson
!     Mark Goldberg
!     John Hickey
!     Skip Williams see http://www.clarionmag.com/cmag/v5/v5n01debuger.html
!
!Class Methods:
!
!     Debug(<string>) - Send any string to the debug window
!     DebugQueue(<queue>,<string>) - Display the contents of a queue
!     DebugEvent - Use to display events and additional useful information
!     DebugClear - Clears the DebugView window
!
!Class Properties
!
!     DebugOff - TRUE to turn debugging off, FALSE to turn debugging on
!     DebugPrefix - Used to filter Debug messages, prepended to the Output String
!     SaveToFile - if TRUE, will save debug messages to an ASCII file
!     ASCIIFileName - name of ASCII file to save debug messages to, default is 'Debug.txt'
!
!--------------------------------------------------------------------------------
!EndRegion Doc

  INCLUDE('Equates.clw'),ONCE
  INCLUDE('Errors.clw'),ONCE
  INCLUDE('TUFO.INT'),ONCE

! RA.2018.12.29.SAT: Added these equates from the Mark Golberg debuger.inc file.
! Combining Mark Golberg's Debuger and UltimateDebug to have features from both.
! So that by just creating a global/local class it can handle both types of messages.
! THE THREE ITEMS BELLOW ARE DEFINED SOMEWHERE ELSE ALSO. ADD THE 2 FOR MOW!!!!
eqDBG2                                  EQUATE('<4,2,7>')   ! CTRL+D, CTRL+B, CTRL+G   !added 5/22/03 by Mark Goldberg (to be used by Assert(x,y) as in Assert(0,eqDBG&'my message')
MAXMSGLEN2                              EQUATE(1000)        ! MG: I doubt that this is an OS limitation
MAX:FEQ2                                EQUATE(2147483647)  ! Equivalent of CLEAR(Long,1) = 2^31 - 1

DEBUGER::CLEAR_CMDLINE                  EQUATE(0)
DEBUGER::CLEAR_ALWAYS                   EQUATE(1)
DEBUGER::CLEAR_NEVER                    EQUATE(2)

DEBUGER::DISABLED                       EQUATE(0)
DEBUGER::ENABLED                        EQUATE(1)
DEBUGER::ENABLE_CMDLINE                 EQUATE(2)

DEBUGER::DUMPQUE_NOFILE                 EQUATE('NOFILE')
DEBUGER::DUMPQUE_PROMPT                 EQUATE('PROMPT')    ! Note: if an empty string is passed, and self.dumpQue_AsciiByDefault then will prompt too

DEBUGER::ASCII_NEVER                    EQUATE(0)
DEBUGER::ASCII_PROMPT                   EQUATE(1)
DEBUGER::ASCII_NOPROMT                  EQUATE(2)
  
 !INCLUDE('UDRA_KeyState.equ'),ONCE         ! RA.2014.09.12 - Refactoring NOT quite finished
  INCLUDE('UDRA_Time.equ'),ONCE
  PRAGMA ('link (C%V%ASC%X%%L%.LIB)')                       !Notation requires C6+


! RA.2014.09.12 - Added back the equates. The refactoring left them out and while you could
! use the standard equates that are used like this in the JH templates (UltimateDebug4.tpw)
! Ms. Lisa Daugherty ran into this today in an application and alerted me to the inconsistency.
! Ms. Lisa Daugherty also provided the solution. She need to compile her application quick.
! Thank you Ms. Lisa Daugherty
udKeyState:Shift                        EQUATE(0100h) !Shift key is pressed
udKeyState:Ctrl                         EQUATE(0200h) !Ctrl key is pressed
udKeyState:Alt                          EQUATE(0400h) !Alt key is pressed
udKeyState:EnterOnNum                   EQUATE(0800h) !Num Pad Enter Key, n/a with MESSAGE()
udKeyState:CapsLock                     EQUATE(1000h) !Caps Lock is On
udKeyState:NumLock                      EQUATE(2000h) !Num Lock is On
udKeyState:ScrollLock                   EQUATE(4000h) !Scroll Lock is On
udKeyState:InsertKey                    EQUATE(8000h) !Insert Mode is On,
! RA.2014.09.12 - Added back the equates.


ST::DebugEventQueue                     QUEUE,TYPE
Date                                        STRING(10)
Time                                        STRING(10)
EventNo                                     SIGNED
EventName                                   CSTRING(31)
FieldFeq                                    SIGNED
FieldName                                   CSTRING(31)
Keycode                                     UNSIGNED
StarDate                                    REAL
                                        END

DebugCategoryQueue                      QUEUE,TYPE
Category                                    STRING(40)
                                        END


!If you want a prefix to be included in the beginning of each debug string,
!then copy this to your application directory, and edit it as desired.
!ST::DEBUG:Prefix                        EQUATE('!')
ST::DEBUG:Debugging                       EQUATE(1)

qtUserEventNameMG                         QUEUE,TYPE                                ! RA.2018.03.03 - Avoid duplicate names
EventEquate                                 LONG
EventName                                   CSTRING(40)
                                          END

!--------------------------------------------------------------------------------
ThreadDebugCore                         CLASS,TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
!Region Properties
lThread                                     LONG(0)                                 ! RA.2018.11.24 - Thread Number
gLevels                                     LONG(0)                                 ! RA.2018.11.27 - Count of nested procedures
szEXE                                       CSTRING(FILE:MaxFilePath + 1)           ! RA.2018.11.27 - Name of Executable
szMOD                                       CSTRING(FILE:MaxFilePath + 1)           ! RA.2018.11.27 - Name of Module
szPRC                                       CSTRING(FILE:MaxFilePath + 1)           ! RA.2018.11.27 - Name ov Procedure
DebugPrefix                                 CSTRING(21)                             ! Only for ODS (not File)
DebugTrace                                  BOOL(FALSE)                             ! RA.2019.02.06 - Prefix: SELF.LOW_WHERE
DebugMe                                     BOOL(FALSE)                             ! RA.2014.09.13 - Need to check some things
TablesBeingTraced                           SHORT(0)                                ! MR (Mark Riffey) contribution

DebugOff                                    BOOL(FALSE)
DebugNoCR                                   BOOL(FALSE)                             ! When Non-False will split Debug messages by CarriageReturn <13>
SaveToFile                                  BOOL(FALSE)
ASCIIFileName                               STRING(FILE:MaxFilePath)
EventOffset                                 LONG,PRIVATE                            ! Internal Value, needed by 'WslDebug$NameMessage'
UserEventNameQ                              &qtUserEventNameMG                      ! RA.2018.03.03 - Avoid duplicate names

!Region Flags For DebugEvent
ShowAll                                     BOOL(FALSE)
ShowField                                   BOOL(FALSE)
ShowFocus                                   BOOL(FALSE)
ShowSelected                                BOOL(FALSE)
ShowSelStart                                BOOL(FALSE)
ShowSelEnd                                  BOOL(FALSE)
ShowKeyCode                                 BOOL(FALSE)
ShowError                                   BOOL(FALSE)
ShowThread                                  BOOL(FALSE)
ShowContents                                BOOL(FALSE)
ShowScreenText                              BOOL(FALSE)
ShowAcceptAll                               BOOL(FALSE)
!EndRegion Flags For DebugEvent

!Region MG-DEBUGER Properties
LevelWarn                                   LONG(20)                                            ! RA.2018.12.30.SUN: Added
! RA.2018.12.29.SAT: Added these attributes from the Mark Golberg debuger.inc file.
osMode                                      BYTE(FALSE)
Duplicates                                  LONG(50)
NumberSame                                  LONG(0)
ThisMsg                                     CSTRING(MAXMSGLEN2)
LastMsg                                     CSTRING(MAXMSGLEN2)
AppendCRLF                                  BYTE(FALSE)
ShowMessageWindow_Beep                      BYTE(FALSE) 
DelayActive                                 BYTE(FALSE)
DelayBody                                   CSTRING(MAXMSGLEN2)
DelayHeader                                 CSTRING(MAXMSGLEN2)
!EndRegion MG-DEBUGER Properties

!EndRegion Properties

!Region Methods
RawODS                                      PROCEDURE(STRING pMSG)

Construct                                   PROCEDURE()
Destruct                                    PROCEDURE()

LOW_RYB                                     PROCEDURE(STRING pMSG)
LOW_WHERE                                   PROCEDURE()           ,STRING                               ! RA.2019.01.27.SUN: Added
LOW_TRACE                                   PROCEDURE(STRING pMSG),STRING                               ! RA.2019.01.24.THU: Added
LOW_THREAD                                  PROCEDURE(STRING pMSG),STRING
LOW_LEVEL                                   PROCEDURE(STRING pMSG),STRING
LOW_DEBUG                                   PROCEDURE(STRING pMSG),STRING
LOW_PREFIX                                  PROCEDURE(STRING pMSG),STRING

Debug                                       PROCEDURE(STRING pMSG)
Enter                                       PROCEDURE(STRING pEXE, STRING pMOD, STRING pPRC)
Enter                                       PROCEDURE(STRING pMSG)
Level                                       PROCEDURE(STRING pMSG)
Exits                                       PROCEDURE(STRING pMSG)
Exits                                       PROCEDURE(STRING pEXE, STRING pMOD, STRING pPRC)

Show                                        PROCEDURE(STRING pMSG, BOOL pShow=FALSE)                    ! RA.2019.01.27.SUN: Added
Show                                        PROCEDURE(STRING pMSG, STRING pTitle, BOOL pShow=FALSE)     ! RA.2019.01.27.SUN: Added

TableTraceEnable                            PROCEDURE(*FILE pTable)                                     ! Mark Riffey contribution
TableTraceDisable                           PROCEDURE(*FILE pTable)                                     ! Mark Riffey contribution

DebugEntry                                  PROCEDURE(STRING pMSG)
DebugLevel                                  PROCEDURE(STRING pMSG)
DebugExit                                   PROCEDURE(STRING pMSG)

DebugToODS                                  PROCEDURE(STRING pDebugString)
DebugToFile                                 PROCEDURE(STRING pDebugString)
DebugToFile_StartEachRow                    PROCEDURE(),STRING,VIRTUAL

ClearDebugView                              PROCEDURE()
GPF                                         PROCEDURE()

AddUserEvent                                PROCEDURE(STRING argEventName,Long argEventEquate)
DebugEvent                                  PROCEDURE(<STRING pDebugProcedure>)
GetEventDescr                               PROCEDURE(),STRING                                          !defaults to EVENT()
GetEventDescr                               PROCEDURE(LONG   argEvent),STRING
GetEventDescr_WM                            PROCEDURE(LONG   argEvent),STRING                           !WndProc Events WM_*
GetFEQDescr                                 PROCEDURE(SIGNED argFEQ=-1),STRING                          !defaults to FIELD()
GetUserEvent                                PROCEDURE(LONG argEventEquate),STRING
SetEventOffSet                              PROCEDURE(),PRIVATE
!HuntForOffset                              PROCEDURE(),PRIVATE
!EndRegion Methods

!Region MG-DEBUGER Methods
DebugBreak                                  PROCEDURE()                   ! 32bit api call (does nothing in 16bit)
!DebugOut                                    PROCEDURE(STRING pDebugString)
ShowMessageWindow                           PROCEDURE(STRING argBody, STRING argHeader)
!                                           Note: self.ShowMessageWindow_Beep FALSE->no beep ELSE beep

Message                                     PROCEDURE(STRING body,<STRING header>,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE)   !simply calls .DebugOut, added for compatibility to Russ's version of the Debuger

!                                           Parm 1 = body of message
!                                           Parm 2 = message header
!                                           Parm 3 = if not FALSE then a message() will be issued along with the debugoutstring
!                                           Parm 4 = if not FALSE then the debugout will be issued regardless of debug setting.
Debugout                                    PROCEDURE(STRING body,<STRING header>,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE)

GetVarSize                                  PROCEDURE(*? xaVar),LONG      !--- see softvelocity.public.clarion6 "Variable Data Type" Sept,12,2006 (code posted by dedpahom) -----!
GetVarType                                  PROCEDURE(*? xaVar),LONG      !--- see softvelocity.public.clarion6 "Variable Data Type" Sept,12,2006 (code posted by dedpahom) -----!

DescribeDataType                            PROCEDURE(LONG xType),STRING                !<-- assumes a DATATYPE:*  (see equates.clw)
DescribeDataTypeShort                       PROCEDURE(LONG xType, LONG xSize),STRING    !<-- assumes a DATATYPE:*  (see equates.clw)
DescribeType                                PROCEDURE(LONG xType),STRING                !<-- assumes a CREATE:*

DumpGroup                                   PROCEDURE(*GROUP xaGroup)
DumpGroup                                   PROCEDURE(*GROUP xaGroup, STRING xLinePrefix)
DumpRecord                                  PROCEDURE(*FILE pFile)
DumpRecord                                  PROCEDURE(*FILE pFile, STRING xLinePrefix)

DumpGroupWindow                             PROCEDURE(*GROUP pGroup)
DumpRecordWindow                            PROCEDURE(*FILE pFile)
DumpGroupWindow                             PROCEDURE(*GROUP pGroup,STRING pMsg,<STRING pStructureType>)
DumpRecordWindow                            PROCEDURE(*FILE pFile,STRING xMsg,<STRING pStructureType>)
FormatMessageList                           PROCEDURE(STRING pMsg,*QUEUE pMsgQueue)
DumpQueueWindow                             PROCEDURE(*QUEUE pQueue,<STRING pMsg>,<BYTE pNoTouch>)

mg_init                                     PROCEDURE(STRING ProgramName)

!!!<summary>Initialized the class
!!!  <see cref="AppendCRLF"/>
!!!</summary>
!!!<param name='ProgramName'>Program Name</param>
!!!<param name='DebugAways'><para>Enumerated, see DEBUBUER:: equates, defaults to DEBUGER::ENABLE_CMDLINE</para>
!!!                                     <para>if true then debuger on regardless of project debug setting</para>
!!!                                    <para>'/Debuger' on the command line will also turn on the debuger functions for that launch</para></param>
!!!<param name='DuplicateCount'>Default=50 number of duplicate debug messages in a row before warning message.
!!!                                        if zero, then do not track duplicates</param>
!!!<param name='argClear'>Enumerated, see DEBUGER::CLEAR_*  Defaults to NEVER
!!!                       will send message to DebugView to clear itself (requires ver 4.3 or higher to work)</param>
!!!<param name='argAsciiByDefault'>Enumerated, see DEBUGER:ASCII_, Defaults to PROMPT 
!!!                                used to control default behavior of dumpQueue when argFilename is omitted</param>
Init                                        PROCEDURE(STRING ProgramName,BYTE DebugAlways=DEBUGER::ENABLE_CMDLINE ,SHORT DuplicateCount=50, BYTE argClear=DEBUGER::CLEAR_NEVER, BYTE argAsciiByDefault=DEBUGER::ASCII_PROMPT)
Kill                                        PROCEDURE()

!EndRegion MG-DEBUGER Methods
                                        END
!--------------------------------------------------------------------------------
UltimateDebug                           CLASS(ThreadDebugCore),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
!Region Properties
ProcedureInfoToDebugView                    BOOL(FALSE)
!DebugLog                                   BOOL(FALSE)
DebugEvent                                  SIGNED
HotKey                                      UNSIGNED
PurgeStarTime                               REAL
EventQ                                      &ST::DebugEventQueue
IgnoreEventQ                                &ST::DebugEventQueue
CategoryQueue                               &DebugCategoryQueue
ProcAlertKey                                LONG
!EndRegion Properties

!Region Methods
Init                                        PROCEDURE()
Kill                                        PROCEDURE()
AddCategoryToDebug                          PROCEDURE(STRING pCategory) !Used to Filter Messages not belonging to active Categories
CalcStarDate                                PROCEDURE(<LONG D>,<LONG T>),REAL,PRIVATE
Construct                                   PROCEDURE()
Debug                                       PROCEDURE(STRING pCategory,STRING pDebugString)  !Filter messages not belonging to active Categories
DebugEntry                                  PROCEDURE(STRING pCategory,STRING pDebugString)  ! RA.2014.03.30 - Nested tracking
DebugLevel                                  PROCEDURE(STRING pCategory,STRING pDebugString)  ! RA.2014.03.30 - Nested tracking
DebugExit                                   PROCEDURE(STRING pCategory,STRING pDebugString)  ! RA.2014.03.30 - Nested Tracking
GetCategory                                 PROCEDURE(STRING pCategory),BOOL  !TRUE if found, FALSE if not found
GetEventName                                PROCEDURE(SIGNED Event),STRING
DebugGroup                                  PROCEDURE(*GROUP pGroup,<*GROUP pGroup2>,STRING pMsg,<STRING pStructureType>)
DebugGroup                                  PROCEDURE(*GROUP pGroup,STRING pMsg,<STRING pStructureType>)
DebugQueue                                  PROCEDURE(*QUEUE pQueue,<STRING pMsg>,<BYTE pNoTouch>)
DebugRecord                                 PROCEDURE(*FILE pFile,STRING pMsg)
DebugRecord                                 PROCEDURE(*FILE pFile1,*FILE pFile2,STRING pMsg)
Destruct                                    PROCEDURE()
FormatMessageList                           PROCEDURE(STRING pMsg,*QUEUE pMsgQueue)
IgnoreEvent                                 PROCEDURE(SIGNED Event)
LogEvent                                    PROCEDURE
Message                                     PROCEDURE(STRING pDebugString)
PurgeUpTo                                   PROCEDURE(*ST::DebugEventQueue xaQ, REAL xPurgeUpTo)  !Assumes xaQ is sorted by StarDate
SetApplicationName                          PROCEDURE(STRING pApplicationName,STRING pProgramExtension),STRING
SetShortApplicationName                     PROCEDURE(STRING pApplicationName,STRING pProgramExtension),STRING
SetDebugEvent                               PROCEDURE(SIGNED DebugEvent)
SetHotKey                                   PROCEDURE(UNSIGNED HotKey)
SetPurgeTime                                PROCEDURE(LONG DebugPurge)
ShowProcedureInfo                           PROCEDURE(STRING pProcedure,STRING pApplication,STRING pHelpID,STRING pCreated,STRING pModified,STRING pCompiled)
TakeEvent                                   PROCEDURE
!EndRegion Methods
                                        END


!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
! RA.2019.02.23: (REPLACEMENT CLASS) :RA.2019.02.23
Ultimate_Debug                          CLASS(ThreadDebugCore),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
                                        END
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------


!--------------------------------------------------------------------------------
! RA.2014.07.29 -  Begin the SQL CallBack Interface class
!--------------------------------------------------------------------------------
  INCLUDE('FILECB.INC'),ONCE

UltimateDebugTraceSQL                   CLASS,IMPLEMENTS(SQLCallBackInterface),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
!Region Properties
UD                                          &UltimateDebug,PROTECTED
MyFile                                      &FILE,PROTECTED
MyFileName                                  CSTRING(FILE:MaxFilePath + 1)
ErrCode                                     CSTRING(FILE:MaxFilePath + 1)
ErrMsg                                      CSTRING(FILE:MaxFilePath + 1)
Opcode                                      SIGNED
Usage                                       UNSIGNED,PROTECTED
FileAction                                  CSTRING(41),PROTECTED
ActionLevel                                 LONG,PROTECTED
!EndRegion Properties

!Region Methods
Init                                        PROCEDURE(FILE aFile),VIRTUAL
Kill                                        PROCEDURE,VIRTUAL
ExecutingCode                               PROCEDURE(CONST *CSTRING inStr, *BYTE Err, *CSTRING FileErrCode, *CSTRING FileErrMsg),STRING,VIRTUAL
!EndRegion Methods
                                        END
!--------------------------------------------------------------------------------
! RA.2014.07.29 - Begin implementation of Mike Hanson's original class
! Name has been changed to make it consistent with the conventions here.
UltimateDebugTraceFile                  CLASS,IMPLEMENTS(FileCallBackInterface),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
!Region Properties
UD                                          &UltimateDebug,PROTECTED
MyFile                                      &FILE,PROTECTED
Usage                                       UNSIGNED,PROTECTED
Direction                                   CSTRING(11),PROTECTED
FileAction                                  CSTRING(41),PROTECTED
ActionLevel                                 LONG,PROTECTED
MyFileName                                  CSTRING(FILE:MaxFilePath + 1)
ErrCode                                     CSTRING(FILE:MaxFilePath + 1)
ErrMsg                                      CSTRING(FILE:MaxFilePath + 1)
Opcode                                      SIGNED
!EndRegion Properties

!Region Methods
Init                                        PROCEDURE(FILE aFile),VIRTUAL
Kill                                        PROCEDURE,VIRTUAL
GetLastAction                               PROCEDURE(UNSIGNED Opcode),VIRTUAL
DriverMessage                               PROCEDURE(*Params Parameters),VIRTUAL
!EndRegion Methods
                                        END
!--------------------------------------------------------------------------------
! RA.2015.01.03 -  Begin Mike Hanson's StarDateClass modified definition
! Did not want to change his original class so I created something with a different class name.
! This allowed me to include everything that was needed in this collection of classes.

INCLUDE('Equates.clw'),ONCE

!TIME:Second                            EQUATE(100)
!TIME:Minute                            EQUATE(60*TIME:Second)
!TIME:Hour                              EQUATE(60*TIME:Minute)
!TIME:Day                               EQUATE(24*TIME:Hour)

!TIME:Midnight                          EQUATE(1)  !Exists in Clarion 9.1+
TIME:Noon                               EQUATE(TIME:Midnight + 12*TIME:Hour)

MIN_TIME                                EQUATE(TIME:Midnight)
MAX_TIME                                EQUATE(TIME:Day)

StarDateClass                           CLASS,TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW') !,_CTLinkMode_),DLL(_CTDllMode_)
!Region Properties
SimulatedClock                              LONG,PROTECTED
SimulatedToday                              LONG,PROTECTED
!EndRegion Properties

!Region Methods
SetSimulatedTime                            PROCEDURE(LONG TodayValue,LONG ClockValue),VIRTUAL
GetClock                                    PROCEDURE,LONG
GetToday                                    PROCEDURE,LONG

CalcStardate                                PROCEDURE(LONG D,LONG T),REAL
CalcStardateDelta                           PROCEDURE(REAL STAR1),REAL                          ! RA.2015.01.06 - Startdate difference from now
CalcStardateDelta                           PROCEDURE(LONG EarlierD,LONG EarlierT),REAL
CalcStardateDelta                           PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2),REAL
GetStardate                                 PROCEDURE,REAL
SplitStardate                               PROCEDURE(REAL StarDate_IN,*? D_OUT,*? T_OUT)

AddDaysTime                                 PROCEDURE(REAL STAR1,REAL STAR2),REAL               ! RA.2015.01.03 - Add stardate and return startdate
AddDaysTime                                 PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2),REAL     ! RA.2015.01.03 - Add date+time and return startdate
FormatOneDayTime                            PROCEDURE(REAL STAR1),STRING                        ! RA.2015.01.03 - StarDate ddd:hh:mm:ss output format
FormatOneDayTime                            PROCEDURE(LONG D1,LONG T1),STRING                   ! RA.2015.01.03 - StarDate ddd:hh:mm:ss output format
FormatDaysTime                              PROCEDURE(REAL STAR1,REAL STAR2),STRING             ! RA.2015.01.03 - StarDate ddd:hh:mm:ss output format
FormatDaysTime                              PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2),STRING   ! RA.2015.01.02 - StarDate ddd:hh:mm:ss output format
!EndRegion Methods
                                        END
!--------------------------------------------------------------------------------
  INCLUDE('UDRA_ctQueue.inc'),ONCE
  INCLUDE('UDRA_ctAsciiLogger.inc'),ONCE
  INCLUDE('Errors.clw'),ONCE

TIMING:COUNT:MAXIMUM                    EQUATE(4294967295)                  ! If you reach this then do not count anymore and no average

!gtYada    GROUP,TYPE
!actual details here, and only here
!               END

!qtYada    QUEUE(gtYada),TYPE
!                END

!ctQYada  CLASS(ctQueue),TYPE,.....
!Q                &qtYada
!...
!                END

TimingGroup                             GROUP,TYPE
AverageTime                                 REAL                            ! StarDate [days:hours:minutes:secods]
AppProcName                                 STRING(064)                     ! Application-Procedure Name
ExecuteCount                                ULONG                           ! Times this procedure was executed (MAXIMUM is 4,294,967,295)
ExecuteTime                                 REAL                            ! StarDate [days:hours:minutes:secods]
                                        END

TimingQueue                             QUEUE(TimingGroup),TYPE
                                        END

TimingClass                             CLASS(ctQueue),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW') !,_CTLinkMode_),DLL(_CTDllMode_)
UD                                          &UltimateDebug,PROTECTED
oStarDate                                   &StarDateClass,PROTECTED
oLogFile                                    &ctAsciiLogger,PROTECTED
TimingQ                                     &TimingQueue,PROTECTED

ReportName                                  STRING(256),PROTECTED

CONSTRUCT                                   PROCEDURE
DESTRUCT                                    PROCEDURE
Init                                        PROCEDURE(STRING pReportName)
Kill                                        PROCEDURE()
Description                                 PROCEDURE(),STRING,DERIVED

Count                                       PROCEDURE(),LONG                                  ! Alias for ._Records
GetRow                                      PROCEDURE(LONG xPointer),LONG,PROC
AddRow                                      PROCEDURE(),LONG,PROC
PutRow                                      PROCEDURE(),LONG,PROC

Find                                        PROCEDURE(STRING pAppProcName, REAL pStarDate)
Done                                        PROCEDURE(STRING pLogFileName)

_Find                                       PROCEDURE(STRING pAppProcName),BOOL
_Records                                    PROCEDURE(),LONG
                                        END
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
ThreadDebugLocal                        CLASS(ThreadDebugCore),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
Construct                                   PROCEDURE()
Destruct                                    PROCEDURE()
                                        END


!END-OMIT('_EndOfInclude_',_UltimateDebug_)
