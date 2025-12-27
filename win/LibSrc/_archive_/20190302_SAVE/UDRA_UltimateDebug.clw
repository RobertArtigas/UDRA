					MEMBER()

!MG Changes:
! Removed _ABC[Dll|Link]Mode_ equates
! Added ODS - an CW String to CSTRING wrapper (plus <13,10>)
! DbgClearView - uses ODS - as it shouldn't appear in the AsciiFile
! Using UltimateString, for CR Splitting
! SELF.EventQ.FieldName = SELF.GetFEQDescr(FIELD())
! ShowProcedureInfo - changed code inside of the ACCEPT loop to do a CASE ACCEPTED()
! Split out functionality into UltimateDebugerCore
! Discovered that .UserEventNameQ was left null in .Construct, nor was it Free'd()/Disposed in Destruct
! Discovered .Destruct was doing a FREE without first checking if not NULL
!RA Changes:
! Added Methods to do global tracking of how deep in the rabit hole you are
! Added Attribute to keep track of the nested level
!RA Changes:
! After UltimateDebug decided to go to a local class object only (which broke some of my items), it was
! decided to take the UltimateDebug Class and the Mark Golberg debuger Class and create a combined Class.
! Needed to blend both sets of code to substitude legacy DB (Russ' debuger) and UD (Ultimate Debug) code.
! There was also a need to track [Thread][Level] across DebugView++ tracing. It was simpler[???] to fork.
! It might be also useful to be able to do both global and local objects. Do an implementation of this.
! This is an amalgamation of many individual efforts. Many minds have worked on this code for many years.
! Added a number of methods in the "Dump" area including the "Dump*Window" methods that were
! modified from the "Debug" to window methods created by Mike Hanson. Added the TUFO interface to
! allow for getting the TYPE and LENGTH of the data. A Limitation of 30 BYTES was added to the display
! of the data to not overwhelm the display with strings with 1000's characters.

!==============================================================================
!If all fields begin with the same prefix, then you can have that auto-stripped
!by uncommenting the following line.
!E_StripPrefix         EQUATE(1)
!==============================================================================
WidthMultiplier:Narrow          EQUATE(5)  !normal columns (numbers, lowercase, etc.)
WidthMultiplier:Wide            EQUATE(7)  !uppercase columns
!==============================================================================
  INCLUDE('EQUATES.CLW')
  INCLUDE('Errors.clw')
  INCLUDE('UDRA_UltimateDebug.INC'),ONCE
  INCLUDE('UDRA_UltimateString.inc'), ONCE
! ToDo _wsldebug$setlogfile

! RA.2018.12.31.MON: Copied from Mark Golber's debuger.clw file
Verbose EQUATE(1) !0..N   

                          COMPILE('***',_width32_)
ANSI_NAME EQUATE('A')
                          !  ***
                          OMIT('***',_width32_)
ANSI_NAME EQUATE('')
                          !  ***

  MAP
    MODULE('kernel')
      DebugBreak(),PASCAL !,RAW,NAME('debugbreak')
      OutputDebugString(*CSTRING),PASCAL,RAW,NAME('OutputDebugStringA')
    END
    MODULE('C%V%RUN%X%')
      RTL:NameMessage (*CSTRING Answer, UNSIGNED EventNum ),RAW,NAME('WslDebug$NameMessage')   !Note: use   Event() + EVENT:FIRST  else will get WM_*
      RTL:FieldName(SIGNED FEQ),*CSTRING,RAW,NAME('Cla$FIELDNAME')
    END
    ODS_MG (STRING xMSG)              ! RA.2018.03.03 - ???
    ODS_RYB(STRING sMSG)              ! RA.2018.11.27: Added
  END

  PRAGMA('define(profile=>off)')
  COMPILE('***',profile)
! @===========================================================================================
! Don`t try and profile this, you get infinite recursion (when used by EnterProc/LeaveProc)
! Set profile=>off on this module in the project
! Note: in C6, profile=>off    possibly renamed to   proc_trace=>off
!
  This comment will prevent you from compiling unless profile=>off on this modu1e
! @===========================================================================================
  !end-COMPILE('***',profile)

                                                           COMPILE('_ifdef_',EVENT:APP=0)
EVENT:APP                             EQUATE(08000h)
EVENT:APP_LAST                        EQUATE(0BFFFh)
                                                      !END_COMPILE('_ifdef_',EVENT:APP=0)
!==============================================================================

!Region Local Procedures
!==============================================================================
ODS_MG                                      PROCEDURE(STRING xMSG)
!==============================================================================
szMSG  &CSTRING
  CODE
  szMSG &= NEW CSTRING( SIZE(xMSG) + 3 )   ! 3 = LEN(<13,10,0>)
  szMSG  =                   xMSG & '<13,10,0>'
  OutputDebugString(szMSG)
  DISPOSE(szMSG)
  RETURN
!==============================================================================
ODS_RYB                                     PROCEDURE(STRING sMSG)
!==============================================================================
szMSG  &CSTRING
  CODE
  szMSG &= NEW CSTRING( SIZE(sMSG) + 3 )   ! 3 = LEN(<13,10,0>)
  szMSG  =                   sMSG & '<13,10,0>'
  OutputDebugString(szMSG)
  DISPOSE(szMSG)
  RETURN
!EndRegion Local Procedures

!Region ThreadDebugCore
!==============================================================================
!!! <summary>
!!! ClassObject.RawODS(STRING DebugMessage)
!!! Output a DebugMessage to the console viewer and track posible recursions.
!!! </summary>
!!! The message is ready to output to the console viewer. Everything has been prefixed
!!! to the message. After you output to console viewer track posible recursions.
ThreadDebugCore.RawODS                      PROCEDURE(STRING xMsg)
!==============================================================================
szMSG         &CSTRING
  CODE
  szMSG &= NEW CSTRING( SIZE(xMSG) + 3 )   ! 3 = LEN(<13,10,0>)
  szMSG  =                   xMSG  & '<13,10,0>'
  OutputDebugString(szMSG)
  DISPOSE(szMSG)

  ! RA.2019.01.08.TUE: Had a conversation with Mark Goldberg about the Duplicates code
  ! to check recursion disappearing from the code base. This might be a useful removal for
  ! cleaning up and simplplifying the code. Only here, consider a compile time option.
  !
  ! RA.2019.01.08.TUE: Prefix the EXE, MOD, and PRC to the message for the recursion message.
  ! If the message is comming from .DebugOut() you will get double prefixes when recursing.
  !
  ! RA.2018.12.30.SUN: ADDITIONAL TESTING REQUIRED HERE! (How to prove the code?)
  !---------------------------------------------------------------------------------------!
  ! THIS SHOULD BE TREATED AS EXPERIMENTAL CODE. NO EXTENSIVE TESTING DONE AT THIS TIME.  !
  ! Need to extensively test this by FORCING some recursive calls to happen to trap them. !
  !---------------------------------------------------------------------------------------!
  ! RA.2018.12.30.SUN: Programmers make mistakes just like everyone else.
  ! So if you are not doing some type of testing or peer validation, good luck.
  ! Recursion is one of those things that sometimes is easy to do, hard to debug.
  !
  SELF.ThisMsg  = SELF.DebugPrefix & SELF.LOW_WHERE()                           & ' '     & |
                  CHOOSE(SELF.DelayActive=TRUE , CLIP(SELF.DelayBody), '')      & xMsg    & |
                  CHOOSE(SELF.AppendCRLF =FALSE, '', '<13,10>')
  !
  ! RECURSION 1: Tight loop. CPU 100% busy. Termination condition not quite right?
  ! TRAP       : The same debug message is showing up continuously.
  !
  IF SELF.Duplicates <> 0               ! MG Policy: only track lastmsg,NumberSame if going to do something with them....
    IF  SELF.LastMsg <> SELF.ThisMsg
        SELF.LastMsg  = SELF.ThisMsg
        SELF.NumberSame  = 0
    ELSE
        SELF.NumberSame += 1
        ! if SELF.duplicates <> 0 and SELF.NumberSame % SELF.duplicates = 0
        IF  SELF.NumberSame % SELF.Duplicates = 0
            SELF.ShowMessageWindow('A series of ' & SELF.NumberSame & ' duplicate debug messages have been issued' & |
              'Message      ['& CLIP(SELF.ThisMsg) &']', 'Recursion 1: ThreadDebugCore.RawODS')
        END
    END
  END
  !
  ! RECURSION 2: Cycle loop. PROCEDURE A calls PROCEDURE B calls PROCEDURE C calls PROCEDURE A, and so on...  
  ! TRAP       : The nesting level gets exceedingly high.
  !
  IF SELF.LevelWarn > 0 AND SELF.gLevels > 0
    IF  SELF.gLevels % SELF.LevelWarn = 0
        SELF.ShowMessageWindow('Your nesting level of ' & SELF.gLevels & ' has reached an exceedingly high number' & |
              'Message      ['& CLIP(SELF.ThisMsg) &']', 'Recursion 2: ThreadDebugCore.RawODS')
    END
  END
  !---------------------------------------------------------------------------------------!
  ! RA.2018.12.30.SUN: While it might not be posible to prevent recursion from happening, !
  ! you can at least give a hint to the programmer that it might be happening.            !
  !---------------------------------------------------------------------------------------!
  RETURN

!==============================================================================
ThreadDebugCore.Construct                   PROCEDURE
!==============================================================================
  CODE
  SELF.lThread                  = THREAD()                  ! RA.2018.11.23 - Thread Id
  SELF.gLevels                  = 0                         ! RA.2018.11.27 - Nesting Level
  SELF.szEXE                    = '<0>'                     ! RA.2018.11.27 - Executable
  SELF.szMOD                    = '<0>'                     ! RA.2018.11.27 - Module 
  SELF.szPRC                    = '<0>'                     ! RA.2018.11.27 - Procedure
  SELF.DebugTrace               = FALSE                     ! RA.2019.02.06 - .LOW_TRACE
   
  SELF.UserEventNameQ &= NEW qtUserEventNameMG
  SELF.SetEventOffset()

  SELF.ShowAll                  = FALSE
  SELF.ShowField                = FALSE
  SELF.ShowFocus                = FALSE
  SELF.ShowSelected             = FALSE
  SELF.ShowSelStart             = FALSE
  SELF.ShowSelEnd               = FALSE
  SELF.ShowKeyCode              = FALSE
  SELF.ShowError                = FALSE
  SELF.ShowThread               = FALSE
  SELF.ShowContents             = FALSE
  SELF.ShowScreenText           = FALSE
  SELF.ShowAcceptAll            = FALSE

  SELF.DebugPrefix              = '!'
  SELF.ASCIIFileName            = 'DebugLog.txt'

  SELF.LevelWarn                = 20
  SELF.osMode                   = FALSE
  SELF.Duplicates               = 50                        ! MG Policy: only track lastmsg,NumberSame if going to do something with them....
  SELF.NumberSame               = 0
  SELF.ThisMsg                  = ''
  SELF.LastMsg                  = ''
  SELF.AppendCRLF               = FALSE
  SELF.ShowMessageWindow_Beep   = FALSE
  SELF.DelayActive              = FALSE
  SELF.DelayBody                = ''
  SELF.DelayHeader              = ''

  SELF.DebugMe                  = FALSE
  SELF.DebugOff                 = FALSE
  SELF.DebugNoCR                = FALSE
  SELF.SaveToFile               = FALSE
  IF (SELF.DebugMe)
    SELF.Debug(':DEBUGME ThreadDebugCore.Construct')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.DebugTrace=( '               & SELF.DebugTrace               & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.DebugMe=( '                  & SELF.DebugMe                  & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.DebugOff=( '                 & SELF.DebugOff                 & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.DebugNoCR=( '                & SELF.DebugNoCR                & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.SaveToFile=( '               & SELF.SaveToFile               & ' )')	
    SELF.Debug(':DEBUGME:LOG ThreadDebugCore.Construct LONGPATH=( '                       & LONGPATH()                    & ' )')
    SELF.Debug(':DEBUGME:LOG ThreadDebugCore.Construct SELF.ASCIIFileName=( '             & CLIP(SELF.ASCIIFileName)      & ' )')
    ! RA.2019.01.08: Add additional starting debug flag output
    SELF.Debug(':DEBUGME ThreadDebugCore.Construct SELF.LevelWarn=( '                     & SELF.LevelWarn                & ' )')
    SELF.Debug(':DEBUGME ThreadDebugCore.Construct SELF.Duplicates=( '                    & SELF.Duplicates               & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.osMode=( '                   & SELF.osMode                   & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.AppendCRLF=( '               & SELF.AppendCRLF               & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.ShowMessageWindow_Beep=( '   & SELF.ShowMessageWindow_Beep   & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Construct SELF.DelayActive=( '              & SELF.DelayActive              & ' )')
  END 
  RETURN
!==============================================================================
ThreadDebugCore.Destruct                    PROCEDURE
!==============================================================================
  CODE
  IF ~(SELF.UserEventNameQ &= NULL)
    FREE(SELF.UserEventNameQ)
    DISPOSE(SELF.UserEventNameQ)
  END
 
  ! If we find a name here, we have put a name with an "Enter" method but we have not used an "Exits" method.
  ! So we just take care of it when we destruct. So if the "Exits" never gets done, destruct cleans itself up.  
  ! For the hand coders this saves time. You will have always have one "Enter" in. You might have several
  ! "Exits" out, and the class Destruct will catch them all. But the local debug class should always exist.
  ! For the ABC template generation you will have one call to the "Enter" method and one call the "Exits"
  ! method generated. This is probably best to be done around the first Run method that calls the program.
  ! That eliminates all the different paths that the ABC class object might exit and the need to catch them.
  ! You can use a debug global a debug local or both types of debug objects. Just use different names.
  ! For the Clarion code before ABC. You will have to determine the correct usage according to your needs.
  IF (SELF.szEXE <> '') THEN
    SELF.Debug(':EXITS (' & SELF.szEXE & '=' & SELF.szMOD & '=' & SELF.szPRC & ') DESTRUCT')
    SELF.gLevels       -= 1
    SELF.szEXE          = '<0>'
    SELF.szMOD          = '<0>'
    SELF.szPRC          = '<0>'
    SELF.DebugTrace     = FALSE
  END

  IF (SELF.DebugMe)
    SELF.Debug(':DEBUGME ThreadDebugCore.Destruct')
  END
  RETURN

!==============================================================================
!!! <summary>
!!! ClassObject.LOW_RYB(STRING DebugString)
!!! This outputs a DebugMessage<13,10> directly to "OutputDebugString".
!!! There are no prefixes and the only ouput is to the console viewer.
!!! </summary>
ThreadDebugCore.LOW_RYB                     PROCEDURE(STRING pMSG)
!==============================================================================
  CODE
  ODS_RYB(CLIP(pMSG))
  RETURN

!==============================================================================
! RA.2019.01.24.THU: Let's go see about testing a possible future option.
! RA.2019.01.24.THU: This fully identifies each debug line as where it comes from.
!------------------------------------------------------------------------------
! All three parameters are setup by the Tread Debug method being ABC generated.
! The program name (SELF.szEXE) is setup by the debuger method of initialization.
! If nothing is set up, you will still get and output that will make some sense.
! A transition code moment helping UltimateDebug and Debuger methods to merge.
!==============================================================================
!!! <summary>
!!! ClassObject.LOW_WHERE(),STRING
!!! Returns a location (Application=Module=Procedure) for identification purposes
!!! of debug messages.
!!! </summary>
ThreadDebugCore.LOW_WHERE                   PROCEDURE()             !,STRING
!==============================================================================
  CODE
  IF (SELF.szEXE <> '' AND SELF.szMOD <> '' AND SELF.szPRC <> '')
    RETURN '(' & SELF.szEXE & '=' & SELF.szMOD & '=' & SELF.szPRC & ')'
  ELSIF (SELF.szEXE <> '')  
    RETURN '(' & SELF.szEXE & ')'
  ELSE
    RETURN ''
  END
!==============================================================================
!!! <summary>
!!! ClassObject.LOW_TRACE(STRING DebugMessage),STRING
!!! Returns (Application=Module=Procedure) & DebugMessage 
!!! </summary>
ThreadDebugCore.LOW_TRACE                   PROCEDURE(STRING pMSG)  !,STRING
!==============================================================================
  CODE
  RETURN SELF.LOW_WHERE() & pMSG

!==============================================================================
!!! <summary>
!!! ClassObject.LOW_THREAD(STRING DebugMessage),STRING
!!! Returns [T<Thread Number>] & DebugMessage 
!!! The <thread number> will be the THREAD() of the current procedure.
!!! </summary>
ThreadDebugCore.LOW_THREAD                  PROCEDURE(STRING pMSG)  !,STRING
!==============================================================================
  CODE
  RETURN '[T' & SELF.lThread & ']' & pMSG
  
!==============================================================================
!!! <summary>
!!! ClassObject.LOW_LEVEL(STRING DebugMessage),STRING
!!! Returns [T<Thread Number>][L<Nesting Level] & DebugMessage 
!!! The <thread number> will be the THREAD() of the current procedure.
!!! The <nesting level> will be the number of the procedure that has been
!!! called on this thread without a return. A procedure stack number.
!!! </summary>
ThreadDebugCore.LOW_LEVEL                   PROCEDURE(STRING pMSG)  !,STRING
!==============================================================================
  CODE
  RETURN SELF.LOW_THREAD('[L' & FORMAT(SELF.gLevels,@N03) & ']' & pMSG)

!==============================================================================
!!! <summary>
!!! ClassObject.LOW_DEBUG(STRING DebugMessage),STRING
!!! Returns [T<Thread Number>][L<Nesting Level] & ' ' & DebugMessage 
!!! The <thread number> will be the THREAD() of the current procedure.
!!! The <nesting level> will be the number of the procedure that has been
!!! called on this thread without a return. A procedure stack number.
!!! </summary>
ThreadDebugCore.LOW_DEBUG                   PROCEDURE(STRING pMSG)  !,STRING
!==============================================================================
  CODE
  IF pMSG[1] = ':' OR pMSG[1] = ' '
    RETURN SELF.LOW_LEVEL(      pMSG)
  ELSE 
    RETURN SELF.LOW_LEVEL(' ' & pMSG)
  END

!==============================================================================
!!! <summary>
!!! ClassObject.LOW_DEBUG(STRING DebugMessage),STRING
!!! Returns <Debug Prefix>[T<Thread Number>][L<Nesting Level] & ' ' & DebugMessage 
!!! The <thread number> will be the THREAD() of the current procedure.
!!! The <nesting level> will be the number of the procedure that has been
!!! called on this thread without a return. A procedure stack number.
!!! The <debug prefix> is defined as a parameter when you initialize the class.
!!! </summary>
ThreadDebugCore.LOW_PREFIX                  PROCEDURE(STRING pMSG)  !,STRING
!==============================================================================
  CODE
  ! RA.2019.02.06: Added DebugTrace to output the location of each Debug statement.
  ! Improve output for those folks that are not familiar with what they are tracing. 
  IF SELF.DebugTrace THEN
    RETURN SELF.DebugPrefix & SELF.LOW_WHERE() & SELF.LOW_DEBUG(pMSG)
  ELSE 
    RETURN SELF.DebugPrefix & SELF.LOW_DEBUG(pMSG)
  END

!==============================================================================
!!! <summary>
!!! ClassObject.Debug(DebugMessage)
!!! </summary>
!!! The Debug class method is the main entry point that sends DebugMessage to the
!!! console viewer or to an output file depending on the option settings chosen.
ThreadDebugCore.Debug                       PROCEDURE(STRING pMSG)
!==============================================================================
  CODE
  IF NOT SELF.DebugOff
    SELF.DebugToODS(pMSG)
  END
  IF     SELF.SaveToFile
    SELF.DebugToFile(pMSG)
  END
  RETURN	 

!==============================================================================
ThreadDebugCore.Enter                       PROCEDURE(STRING psEXE, STRING psMOD, STRING psPRC)
!==============================================================================
  CODE
  ! When you enter you save these for when you might have to use them when/if the destructor fires
  ! The SELF.szEXE (SELF.pgmname in debuger) is used by some of the internal methods.
  SELF.szEXE           = CLIP(psEXE) & '<0>' 
  SELF.szMOD           = CLIP(psMod) & '<0>'
  SELF.szPRC           = CLIP(psPRC) & '<0>'
  SELF.gLevels        += 1
  SELF.Debug(':ENTER (' & CLIP(psEXE) & '=' & CLIP(psMOD) & '=' & CLIP(psPRC) & ')')
  RETURN
!==============================================================================
ThreadDebugCore.Enter                       PROCEDURE(STRING psMSG)
!==============================================================================
  CODE
  SELF.gLevels        += 1
  SELF.Debug(':ENTER ' & CLIP(psMSG) & '')
  RETURN
!==============================================================================
ThreadDebugCore.Level                       PROCEDURE(STRING psMSG)
!==============================================================================
  CODE
  SELF.Debug(' ' & CLIP(psMSG) & '')
  RETURN
!==============================================================================
ThreadDebugCore.Exits                       PROCEDURE(STRING psMSG)
!==============================================================================
  CODE
  SELF.Debug(':EXITS ' & CLIP(psMSG) & '')
  SELF.gLevels        -= 1
  RETURN
!==============================================================================
ThreadDebugCore.Exits                       PROCEDURE(STRING psEXE, STRING psMOD, STRING psPRC)
!==============================================================================
  CODE
  ! When you exits here you clear these so they will not get used by the destructor.
  ! See the desctruct comments about how it is used when you do not use and Exits(...).
  SELF.Debug(':EXITS (' & CLIP(psEXE) & '=' & CLIP(psMOD) & '=' & CLIP(psPRC) & ')')
  SELF.gLevels        -= 1
  SELF.szEXE           = '<0>'
  SELF.szMOD           = '<0>'
  SELF.szPRC           = '<0>'
  RETURN

!==============================================================================
!!! <summary>
!!! ClassObject.Show(DebugMessage,ShowMessageOnScreenFlag)
!!! Send a message to debug with an option to display it on the screen.
!!! </summary>
ThreadDebugCore.Show                        PROCEDURE(STRING pMSG, BOOL pShow)
!==============================================================================
! RA.2019.01.27.SUN: Added this additional method for error message traps.
! The location of the error will be on the title of the message.
  CODE
  SELF.Debug(pMSG)
  IF pShow = TRUE THEN
    SELF.ShowMessageWindow(pMSG, SELF.LOW_WHERE())
  END
  RETURN
!==============================================================================
!!! <summary>
!!! ClassObject.Show(DebugMessage,MessageTitle,ShowMessageOnScreenFlag)
!!! Send a message to debug with an option to display it on the screen with a title.
!!! </summary>
ThreadDebugCore.Show                        PROCEDURE(STRING pMSG, STRING pTitle, BOOL pShow)
!==============================================================================
! RA.2019.01.27.SUN: Added this additional method for error message traps.
! The location of the error will be on the title of the message if it is not passed.
  CODE
  SELF.Debug(pMSG)
  IF pShow = TRUE THEN
    SELF.ShowMessageWindow(CHOOSE(pTitle='', pMSG, pMSG & '|' & SELF.LOW_WHERE()), CHOOSE(pTitle='', SELF.LOW_WHERE(), pTitle))
  END
  RETURN

!==============================================================================
ThreadDebugCore.TableTraceEnable            PROCEDURE(*FILE pTable)             ! Mark Riffey contribution
!==============================================================================
! Mr. Mark Riffey leaves the trace table code in his program and turns it on/off by placing
! the text file name at the directory location of the executable. This allows him to trace
! at the client site with minimal of dificulty. The 'DEBUG:' should go thru DebugView++
  CODE
  !IF (SELF.DebugMe) THEN  
    SELF.Debug(':DEBUGME ThreadDebugCore.TableTraceEnable(<39>' & NAME(pTable) & '<39>)')
  !END
  IF   EXISTS(CLIP(LONGPATH()) & '\trace.txt') = TRUE
  ELSE RETURN
  END 
 
  IF   SYSTEM{PROP:DriverTracing} = '1'
  ELSE SYSTEM{PROP:DriverTracing} = '1'
  END 

  SELF.TablesBeingTraced         += 1
 
  pTable{PROP:TraceFile}          = 'DEBUG:'
  pTable{PROP:Details}            = 1 
  pTable{PROP:Profile}            = 'DEBUG:' 
  pTable{PROP:LogSQL}             = 1 
  RETURN 
!==============================================================================
ThreadDebugCore.TableTraceDisable           PROCEDURE(*FILE pTable)             ! Mark Riffey contribution
!==============================================================================
  CODE 
  !IF (SELF.DebugMe) THEN  
    SELF.Debug(':DEBUGME ThreadDebugCore.TableTraceDisable(<39>' & NAME(pTable) & '<39>)')
  !END
  SELF.TablesBeingTraced         -= 1
 
  IF SELF.TablesBeingTraced = 0 
    SYSTEM{PROP:DriverTracing}    = ''
  END 
 
  pTable{PROP:TraceFile}          = ''
  pTable{PROP:Details}            = 0 
  pTable{PROP:Profile}            = '' 
  pTable{PROP:LogSQL}             = 0
  RETURN 

!==============================================================================
ThreadDebugCore.DebugEntry                  PROCEDURE(STRING pMSG)
!==============================================================================
   CODE
   SELF.Enter('===ENTRY=== ' & CLIP(pMSG))
   !SELF.Enter(CLIP(pMSG))
   RETURN
!==============================================================================
ThreadDebugCore.DebugLevel                  PROCEDURE(STRING pMSG)
!==============================================================================
   CODE
   SELF.Level(CLIP(pMSG))
   RETURN
!==============================================================================
ThreadDebugCore.DebugExit                   PROCEDURE(STRING pMSG)
!==============================================================================
  CODE
  !SELF.Exits(CLIP(pMSG))
  SELF.Exits('===EXIT=== ' & CLIP(pMSG))
  RETURN

!==============================================================================
!!! <summary>
!!! ClassObject.DebugToODS(DebugMessage)
!!! </summary>
!!! This method outputs to the console viewer.
ThreadDebugCore.DebugToODS                  PROCEDURE(STRING pDebugString)
!==============================================================================
  CODE
  !---------------------------------------------------------------------------------------!
  ! RA.2019.01.31.THU: This prove to be a bit too much debugme comming thru the console.  !
  ! It does give a sample of the style of DEBUGME messages as opposed to normal messages. !
  !---------------------------------------------------------------------------------------!
  !IF (SELF.DebugMe) THEN  
  !  SELF.RawODS(SELF.LOW_PREFIX(':DEBUGME ThreadDebugCore.DebugToODS(.)'))
  !END
  !IF (SELF.DebugMe) THEN  
  !  SELF.RawODS(SELF.LOW_PREFIX(':DEBUGME:RAWODS ThreadDebugCore.Debug(<39>' & CLIP(pDebugString) & '<39>)'))
  !END
  IF SELF.DebugNoCR THEN
    SELF.RawODS(SELF.LOW_PREFIX(pDebugString))
    RETURN
  END
  IF INSTRING('<13><10>',pDebugString,1,1)>0 THEN
    IF (SELF.DebugMe) THEN  
      SELF.RawODS(SELF.LOW_PREFIX(':DEBUGME ThreadDebugCore.DebugToODS.Find(<39><<13><<10><39>)'))
    END
    DO Debug:SplitByCR
    RETURN
  END
  IF INSTRING('<13>',pDebugString,1,1)>0 THEN
    IF (SELF.DebugMe) THEN  
      SELF.RawODS(SELF.LOW_PREFIX(':DEBUGME ThreadDebugCore.DebugToODS.Find(<39><<13><39>)'))
    END
    DO Debug:SplitByCR
    RETURN
  END
  IF INSTRING('|',pDebugString,1,1)>0 THEN
    IF (SELF.DebugMe) THEN  
      SELF.RawODS(SELF.LOW_PREFIX(':DEBUGME ThreadDebugCore.DebugToODS.Find(<39>|<39>)'))
    END
    DO Debug:SplitByCR
    RETURN
  END
  SELF.RawODS(SELF.LOW_PREFIX(pDebugString))
  RETURN
!------------------------------------------------------------------------------
Debug:SplitByCR                             ROUTINE
DATA
UString      UltimateString
LineNum      LONG,AUTO
  CODE
  UString.Assign(pDebugString)
  UString.Replace('|','<13>')
  UString.Replace('<10>','')
  UString.Split('<13>')
  LOOP LineNum = 1 TO UString.Records()
    SELF.RawODS(SELF.LOW_PREFIX(' ' & UString.GetLine(LineNum)))
  END

!==============================================================================
ThreadDebugCore.DebugToFile_StartEachRow    PROCEDURE()
!==============================================================================
  CODE
  RETURN '[' & FORMAT(TODAY(),@D010) & '-' & FORMAT(CLOCK(),@T07) & '] '

!==============================================================================
!!! <summary>
!!! ClassObject.DebugToFile(DebugMessage)
!!! </summary>
!!! This method outputs to a log file everything output to the console viewer.
!!! Please note that doing output to the file slows down all debugging.
ThreadDebugCore.DebugToFile                 PROCEDURE(STRING pDebugString)
!==============================================================================
ASC1:ASCIIFile   FILE,PRE(ASC1),DRIVER('ASCII'),CREATE
RECORD              RECORD,PRE()
STRING                  STRING(MAXMSGLEN2)
                    END
                END
  CODE
  DO DebugToFile:Open !<-- inefficient

  IF SELF.DebugNoCR OR ((INSTRING('<13>',pDebugString,1,1)=0) AND (INSTRING('|',pDebugString,1,1)=0)) THEN
    ASC1:String = SELF.DebugToFile_StartEachRow() & SELF.LOW_DEBUG(pDebugString)
    ADD(ASC1:ASCIIFile)
  ELSE
     DO Debug:SplitByCR
  END

  CLOSE(ASC1:ASCIIFile) !<-- inefficient
!------------------------------------------------------------------------------
Debug:SplitByCR                             ROUTINE
DATA
UString      UltimateString
LineNum      LONG,AUTO
  CODE
  UString.Assign(pDebugString)
  UString.Replace('|','<13>')
  UString.Replace('<10>','')
  UString.Split('<13>')
  LOOP LineNum = 1 TO UString.Records()
    ASC1:String = SELF.DebugToFile_StartEachRow() & SELF.LOW_DEBUG(UString.GetLine(LineNum))
    ADD(ASC1:ASCIIFile)
  END
!------------------------------------------------------------------------------
DebugToFile:Open ROUTINE
    ASC1:ASCIIFile{PROP:Name} = SELF.ASCIIFileName
    IF SELF.SaveToFile
      IF ~EXISTS(ASC1:ASCIIFile{PROP:Name})
        CREATE(ASC1:ASCIIFile)
      END
      OPEN(ASC1:ASCIIFile)
    END

!==============================================================================
ThreadDebugCore.ClearDebugView              PROCEDURE()   !Requires Debugview 4.3 or greater
!==============================================================================
!From:  http://www.sysinternals.com/ntw2k/freeware/debugview.shtml
!       Clear-output string: When DebugView sees the special debug output string "DBGVIEWCLEAR" it clears the output.

!Note: If this doesn't appear to work, then you are either:
!          a) using an older version of debugview
!       OR b) you are filtering the message
  CODE
  SELF.LOW_RYB('DBGVIEWCLEAR') !Written this way to get the prefix, so the message won't be filtered

!==============================================================================
ThreadDebugCore.GPF                         PROCEDURE()
!==============================================================================
  CODE
  DebugBreak()

!==============================================================================
ThreadDebugCore.SetEventOffSet              PROCEDURE()
!==============================================================================
  CODE
                                    COMPILE('**++** _C60_Plus_',_C60_)
  SELF.EventOffset = 0A000h
                                 !  **++** _C60_Plus_
                                    OMIT   ('**--** _PRE_C6_',_C60_)
  SELF.EventOffset = 01400h
                                 !  **--** _PRE_C6_

  IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) <> 'EVENT:ACCEPTED'
     DO SetEventOffSet:HuntForOffSet
  END
!==============================================================================
SetEventOffSet:HuntForOffSet                ROUTINE
!==============================================================================
  DATA
  !This method will rarely be called.
  !After some major version of CW, we may discover a new offset
EventNum                                LONG
Pass                                    BYTE
Lo                                      LONG
Hi                                      LONG
  code

  SELF.Debug('SELF.EventOffset is not correct, trying to find a correct value')

  SELF.EventOffset = CHOOSE(SELF.EventOffset = 01400h, 0A000h, 01400h)
  if UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED' THEN RETURN END

  SELF.EventOffset = GETINI('Debuger','EventOffset', -1)
  CASE SELF.EventOffset
    OF -2 ; SELF.Debug('Stored value for EventOffset indicates no valid offset to be found, not searching')
    OF -1 ; SELF.Debug('SELF.EventOffset is not correct, searching for correct value')
  ELSE    ; IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
               SELF.Debug('Using stored value for SELF.EventOffset')
               RETURN
            END
  END

  !The loops are split out to search more likely ranges first
  !for efficiency it makes more sense to check offsets incrementing by 100, searching for a result that starts with 'EVENT'
  LOOP Pass = 1 TO 4
    EXECUTE Pass
      BEGIN; Lo = 0A000h; Hi = 0AFFFh END
      BEGIN; Lo = 01000h; Hi = 01FFFh END
      BEGIN; Lo = 00000h; Hi = 00FFFh END
      BEGIN; Lo = 0B000h; Hi = 0FFFFh END
    END
    LOOP EventNum = Lo TO Hi
      SELF.EventOffset = EventNum
      IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
        PUTINI('Debuger','EventOffset',SELF.EventOffset)
        RETURN
      END
    END
  END

  SELF.Debug('Could not find a working offset for .EventOffset')
  SELF.EventOffset = -2
  PUTINI('Debuger','EventOffset',SELF.EventOffset)
  RETURN
!==============================================================================
ThreadDebugCore.AddUserEvent                PROCEDURE(STRING argEventName,LONG argEventEquate)
!==============================================================================
  CODE
  IF ~SELF.UserEventNameQ &= NULL
     IF SELF.GetUserEvent(argEventEquate)
       SELF.UserEventNameQ.EventName    = argEventName
       PUT(SELF.UserEventNameQ)
     ELSE
       SELF.UserEventNameQ.EventEquate  = argEventEquate
       SELF.UserEventNameQ.EventName    = argEventName
       ADD(SELF.UserEventNameQ)
     END
  END
!==============================================================================
ThreadDebugCore.GetUserEvent                PROCEDURE(LONG argEventEquate)!string
!==============================================================================
  CODE
  !IF (SELF.DebugMe) THEN
  !  SELF.RawODS('DEBUGME: ThreadDebugCore.GetUserEvent')
  !END
  IF ~SELF.UserEventNameQ &= NULL
     SELF.UserEventNameQ.EventEquate  = argEventEquate
     GET(SELF.UserEventNameQ, SELF.UserEventNameQ.EventEquate)
     RETURN CHOOSE( ERRORCODE()=0, SELF.UserEventNameQ.EventName, '')
  ELSE
     RETURN ''
  END
!==============================================================================
ThreadDebugCore.GetEventDescr_WM            PROCEDURE(LONG xEvent)!,string  !prototype set to default to -1
!==============================================================================
lcl:Retval                              LIKE(qtUserEventNameMG.EventName)
  CODE
  RTL:NameMessage(lcl:RetVal, xEvent) !<-- sets lcl:RetVal
  RETURN lcl:Retval
!==============================================================================
ThreadDebugCore.GetEventDescr               PROCEDURE()
!==============================================================================
  CODE
  RETURN SELF.GetEventDescr(EVENT())
!==============================================================================
ThreadDebugCore.GetEventDescr               PROCEDURE(LONG xEvent)!,string  !prototype set to default to -1
!==============================================================================
lcl:Retval                              LIKE(qtUserEventNameMG.EventName)
!lcl:EventNum                            UNSIGNED
lcl:Position                            LONG
  CODE
  lcl:RetVal = SELF.GetUserEvent( xEvent )
  IF ~lcl:RetVal
    CASE xEvent
      OF Event:User                      ; lcl:RetVal = 'EVENT:User'
      OF Event:User + 1 TO Event:Last    ; lcl:RetVal = 'EVENT:User + '& xEvent - Event:User
      OF Event:APP                       ; lcl:RetVal = 'EVENT:App'
      OF Event:APP  + 1 TO Event:APP_LAST; lcl:RetVal = 'EVENT:App + ' & xEvent - Event:APP
    ELSE
      IF SELF.EventOffset = -2   !indicates could not find a valid offset
        lcl:RetVal = 'EVENT['& xEvent &']'
      ELSE
        RTL:NameMessage(lcl:RetVal, xEvent + SELF.EventOffset) !<-- sets lcl:RetVal
      END
    END
  END

  RETURN lcl:RetVal   ![7:LEN(CLIP(lcl:RetVal))]
!==============================================================================
ThreadDebugCore.GetFEQDescr                 PROCEDURE(SIGNED argFEQ)!,string  !prototype set to default to -1
lcl:Retval    CSTRING(40) !<--- some arbitrary length
  CODE
  argFEQ     = CHOOSE(argFEQ = -1, FIELD(), argFEQ)
  lcl:RetVal = RTL:FieldName(argFEQ)

  RETURN lcl:RetVal !CLIP(lcl:RetVal)
!==============================================================================
ThreadDebugCore.DebugEvent                  PROCEDURE(<STRING pDebugProcedure>)
!==============================================================================
cs_DebugString       string(600) !beware of overflow if ShowAll and ColWidths is bigger then 600
cl_Offset            LONG(1)
!cb_DebugNoCR        BYTE(0)
  MAP
    DebugEvent:Append(LONG ColWidth, STRING Str)
  END
!------------------------------------------------------------------------------
  CODE
  IF pDebugProcedure                     THEN DebugEvent:Append(LEN(CLIP(pDebugProcedure))+1,  pDebugProcedure)                           END
	                                           DebugEvent:Append(24, SELF.GetEventDescr() ) ! EVENT() & SUB(SELF.GetEventDescr(),7,100)) !SUB() strip's off 'EVENT:'
  if self.ShowField      or self.ShowAll THEN DebugEvent:Append(34,      'Field('& SELF.GetFEQDescr()           &' = '& FIELD()    & ')') END
  if self.ShowFocus      or self.ShowAll THEN DebugEvent:Append(34,      'Focus('& SELF.GetFEQDescr(FOCUS())    &' = '& FOCUS()    & ')') END
  if self.ShowSelected   or self.ShowAll THEN DebugEvent:Append(34,   'Selected('& SELF.GetFEQDescr(SELECTED()) &' = '& SELECTED() & ')') END
  if self.ShowSelStart   or self.ShowAll THEN DebugEvent:Append(14,   'SelStart('& FOCUS(){prop:SelStart}                          & ')') END
  if self.ShowSelEnd     or self.ShowAll THEN DebugEvent:Append(14,     'SelEnd('& FOCUS(){prop:SelEnd}                            & ')') END
  if self.ShowKeyCode    or self.ShowAll THEN DebugEvent:Append(14,    'KeyCode('& KEYCODE()                                       & ')') END
  if self.ShowError      or self.ShowAll THEN DebugEvent:Append(24,      'Error('& ERRORCODE() & ': ' & CLIP(ERROR())              & ')') END
  if self.ShowThread     or self.ShowAll THEN DebugEvent:Append(14,     'Thread('& THREAD()                                        & ')') END
  if self.ShowAcceptAll  or self.ShowAll THEN DebugEvent:Append(14,  'AcceptAll('& 0{prop:AcceptAll}                               & ')') END
  if self.ShowContents   or self.ShowAll THEN DebugEvent:Append(24,   'Contents('& CONTENTS(FOCUS())                               & ')') END
  if self.ShowScreenText or self.ShowAll THEN DebugEvent:Append(50, 'ScreenText('& FOCUS(){prop:ScreenText}                        & ')') END
                                                              !294 + pDebugProcedure width
  !cb_DebugNoCR   = SELF.DebugNoCR
  !SELF.DebugNoCR = TRUE                        !<-- Threading bug
     SELF.Debug(cs_DebugString[1:cl_Offset])
  !SELF.DebugNoCR = cb_DebugNoCR

DebugEvent:Append  PROCEDURE(LONG ColWidth, STRING Str)	!Local to .DebugEvent
  CODE
!? ASSERT(ColWidth > 0,'DebugEvent:Append ColWidith['& ColWidth &'] is must be positive')
 ! cs_DebugString = cs_DebugString[1:cl_Offset] & Str !<--- InEfficient
  cs_DebugString[cl_Offset : cl_Offset + ColWidth - 1] = Str
  cl_Offset += ColWidth


!Region DEBUGER Methods 
!==============================================================================
ThreadDebugCore.DebugBreak                  PROCEDURE   ! only for 32 bit...doesnt work in 16 bit mode
!==============================================================================
  CODE
      COMPILE('***',_width32_)
  IF SELF.DebugOff THEN RETURN END
  DebugBreak()                                          ! asm ROUTINE
      !END-COMPILE('***',_width32_)

!==============================================================================
!ThreadDebugCore.DebugOut                   PROCEDURE(STRING pDebugString)
!  CODE
!  SELF.Debug(pDebugString)
!  RETURN

!==============================================================================
! RA.2018.12.30.SUN: Added this method from the Mark Golberg debuger.clw file.
ThreadDebugCore.ShowMessageWindow           PROCEDURE(string argBody, string argHeader)
!==============================================================================
                                                     COMPILE('**32bit**',_width32_)
                                                        COMPILE('*debug*',_debug_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt|&Debug')
                                                        !END-COMPILE('*debug*',_debug_)
                                                        OMIT('*debug*',_debug_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt')
                                                        !END-OMIT('*debug*',_debug_)
                                                     !END-COMPILE('**32bit**',_width32_)
                                                     OMIT('**32bit**',_width32_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt')
                                                     !END-OMIT('**32bit**',_width32_)
  CODE
  IF SELF.ShowMessageWindow_Beep
    BEEP(BEEP:SystemExclamation)
  END

  !CASE MESSAGE(argBody, SELF.szEXE & CHOOSE(LEN(CLIP(argHeader))=0,'', ' - ' & argHeader), ICON:Exclamation, DEBUGER::BUTTONLIST)
  CASE MESSAGE(argBody, CHOOSE(LEN(CLIP(argHeader))=0, '', argHeader), ICON:Exclamation, DEBUGER::BUTTONLIST)
    OF 1 ; !do nothing                                ! Name: &OK  (Default)
    OF 2 ; SELF.Debug('Program Halted -');HALT()      ! Name: &Abort
    OF 3 ; SELF.DebugBreak()                          ! Name: Debug
  END !CASE
  RETURN

!==============================================================================
! RA.2018.12.30.SUN: Added this method from the Mark Golberg debuger.clw file.
ThreadDebugCore.Message                     PROCEDURE(STRING argBody,<STRING argHeader>,BYTE argShowMessage,BYTE argForceDebug)   !string body,<string> header,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE
!==============================================================================
PARAM:DEBUGOUT:HEADER EQUATE(3) !omittable parameter number, remember to add 1 for SELF
  CODE
  IF OMITTED(PARAM:DEBUGOUT:HEADER)
     SELF.debugout( argBody,          , argShowMessage, argForceDebug)
  ELSE
     SELF.debugout( argBody, argHeader, argShowMessage, argForceDebug)
  END
  RETURN

!==============================================================================
! RA.2018.12.30.SUN: Added this method from the Mark Golberg debuger.clw file.
ThreadDebugCore.DebugOut                    PROCEDURE(STRING argBody,<STRING argHeader>,BYTE argShowMessage,BYTE argForceDebug)   !string body,<string> header,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE
!==============================================================================
PARAM:DEBUGOUT:HEADER EQUATE(3) !omittable parameter number, remember to add 1 for SELF
!lcl:Altmsg LIKE(SELF.Thismsg) !side-effect argument into ShowMessageWindow
  CODE
  IF SELF.DebugOff THEN RETURN END

  !IF (SELF.DebugMe)
    IF OMITTED(PARAM:DEBUGOUT:HEADER)
      !SELF.Debug(':DEBUGME ThreadDebugCore.DebugOut(' & CLIP(argBody) & ','                   & ',' & argShowMessage & ',' & argForceDebug & ')')
      !STOP(':DEBUGME ThreadDebugCore.DebugOut(' & CLIP(argBody) & ','                   & ',' & argShowMessage & ',' & argForceDebug & ')')
    ELSE
      !SELF.Debug(':DEBUGME ThreadDebugCore.DebugOut(' & CLIP(argBody) & ',' & CLIP(argHeader) & ',' & argShowMessage & ',' & argForceDebug & ')')
      !STOP(':DEBUGME ThreadDebugCore.DebugOut(' & CLIP(argBody) & ',' & CLIP(argHeader) & ',' & argShowMessage & ',' & argForceDebug & ')')
    END
  !END
  !STOP(':DEBUGME:FLAG ThreadDebugCore.DebugOut[ TESTING ] argForceDebug=( ' & argForceDebug & ' ),[ ' & CLIP(argBody) & ' ]')

  !IF SELF.debugActive OR argForceDebug
  IF ~SELF.DebugOff OR argForceDebug
    IF argShowMessage
      SELF.ShowMessageWindow(argBody,argHeader)
    END

    !consider swapping logic around to give running counts of duplicates in SELF.thismsg

    !
    ! RA.2019.01.08.TUE: Prefix the EXE, MOD, and PRC to the message for the recursion message.
    ! For now the prefix is done here and at the .RawODS Method. AFTER some testing just pass
    ! the message without any prefixing along and let the .RawODS deal with the recursion.
    ! For NOW: IF you get two EXE, MOD, and PRC in parenthesis, THEN you know you came thru .DebugOut().
    ! RA.2018.12.30.SUN: NOTE: There are differences on how "ThisMsg" gets output HERE as to the method ".RawODS".
    ! The "SELF.ThisMsg" is built slightly different. 
    ! Combining the methods from Debuger and UltimateDebug today. Getting everything to compile and co-exist.
    ! At a FUTURE time ponder about unifing everything at one location.
    ! 
    SELF.ThisMsg =  SELF.DebugPrefix & SELF.LOW_WHERE()                           & ' '    & |
                    CHOOSE(OMITTED(PARAM:DEBUGOUT:HEADER)=TRUE,'',CLIP(argHeader) & ' - ') & |
                    CHOOSE(SELF.DelayActive=TRUE, CLIP(SELF.DelayBody), '') & argBody      & |
                    CHOOSE(SELF.AppendCRLF =FALSE,'','<13,10>')

    !
    ! RA.2018.12.31.MON: Lets go thru the ".Debug" method. Test out that interface and the logic
    ! RA.2018.12.30.SUN: NOTE: The code below might be better being sent do the ".RawODS" method,
    ! since the method ".RawODS" checks for both types of recursion.
    ! The ".Debug" method also has to be considered since that goes thru the ".DebugToODS", which then
    ! goes thru the ".RawODS". The message also gets sent to the ASCII trace file.
    ! === All of the code being built today needs severe testing to check for the best outcome. ===
    !
 !   OutputDebugString (SELF.ThisMsg)              ! Send the debug message to the viewer (Code originally here)
    SELF.Debug(SELF.ThisMsg)                       ! 2018.12.31.MON: Lets just test this (RA.2019.01.08: Works!)

 !    ! RA.2018.12.31.MON: Block out this code, to test out the inteface
 !    IF SELF.Duplicates <> 0                       ! MG Policy: only track lastmsg,NumberSame if going to do something with them....
 !      IF  SELF.LastMsg <> SELF.ThisMsg
 !          SELF.LastMsg  = SELF.ThisMsg
 !          SELF.NumberSame  = 0
 !      ELSE
 !          SELF.NumberSame += 1
 !        !if SELF.duplicates <> 0 and SELF.NumberSame % SELF.duplicates = 0
 !        IF  SELF.NumberSame % SELF.Duplicates = 0
 !            SELF.ShowMessageWindow('A series of ' & SELF.NumberSame & ' duplicate debug messages have been issued' & |
 ! |             '||'                                     & |
 ! |             'Body         ['&argBody           &']|' & |
 ! |             'DebugPrefix  ['& SELF.DebugPrefix &']|' & |
 ! |             'Program Name ['& SELF.szEXE       &']|' & |
 ! |             'header       ['& CLIP(argHeader)  &'] Omitted?['& OMITTED(PARAM:DEBUGOUT:HEADER) &']|' &|
 ! |             'CRLF Status  ['& SELF.AppendCRLF  &']|' & |
 ! |             'Delay Status ['& SELF.DelayActive &']|' & |
 !              'Message      ['& CLIP(SELF.ThisMsg) &']', argHeader)
 !        END
 !      END
 !    END
    SELF.DelayActive = FALSE
  END
  RETURN

!==============================================================================
ThreadDebugCore.DumpGroup                   PROCEDURE(*GROUP xaGroup)
!==============================================================================
  CODE
  IF SELF.DebugOff THEN RETURN END
  SELF.DumpGroup(xaGroup, '')
  RETURN
!==============================================================================
ThreadDebugCore.DumpGroup                   PROCEDURE(*GROUP xaGroup, STRING xLinePrefix)
!==============================================================================
_What      ANY
_Where     LONG(1)
!CurrDim   LONG         !<--- C61 + support HOWMANY
! We limit the amount of bytes that we dump for each field to a maximum number to not display
! too much data that is really not necessary to identify what is actually in the field.
! The last thing you really need is to display a string field that is 10,000 characters long.
! You get the type and length of the field and that should allow you to identify everything.
_Type      LONG(0)
_Size      LONG(0)
_Data      CSTRING(31)  ! Limit the amount of data to show
  CODE
  IF SELF.DebugOff THEN RETURN END
  LOOP
    _What  &= WHAT(xaGroup,_Where)
    IF _What &= NULL
       BREAK
    END

    _Type = SELF.GetVarType(_What)
    _Size = SELF.GetVarSize(_What)

    IF ISGROUP(xaGroup,_Where)
      ! RA.2019.01.07: Output a better and improved message.
      !SELF.Debug(xLinePrefix & ' ' & WHO(xaGroup,_Where) & ' (' & _Type & ') ' & SELF.DescribeDataType(_Type) & '(' & _Size & ')')
      SELF.Debug(xLinePrefix & ' ' & WHO(xaGroup,_Where) & '')
      _Where += 1
      CYCLE
    END
	
    ! These are dealt with automatically
                          _Data = _What
    ! Handle the special cases				 
    CASE _Type
    OF DataType:DATE   ;  _Data = CLIP(FORMAT(_What,@D010-B))
    OF DataType:TIME   ;  _Data = CLIP(FORMAT(_What,@T04B))
    OF DataType:STRING ;  _Data = CLIP       (_What)
    END
    ! RA.2019.01.07: Output a better and improved message.
    !SELF.Debug(xLinePrefix & ' ' & WHO(xaGroup,_Where) & ' (' & _Type & ') ' & SELF.DescribeDataType(_Type) & '(' & _Size & ') = [<39>' & _Data & '<39>]')
    SELF.Debug(xLinePrefix & ' ' & WHO(xaGroup,_Where) & ' ' & SELF.DescribeDataTypeShort(_Type,_Size) & ' = [ <39>' & _Data & '<39> ]')

    _Where += 1
  END
  RETURN
!==============================================================================
ThreadDebugCore.GetVarSize                  PROCEDURE(*? xaVar) !,LONG
!==============================================================================
UFO                             &TUFO_CallInterface
  CODE
  UFO &= ADDRESS(xaVar)
  RETURN UFO._Size( ADDRESS(xaVar) ) 
!==============================================================================
ThreadDebugCore.GetVarType                  PROCEDURE(*? xaVar) !,LONG
!==============================================================================
!--- see softvelocity.public.clarion6 "Variable Data Type" Sept,12,2006 (code posted by dedpahom) -----!
!--- the following links are in Clarion and RUSSIAN -- can we get a translation please!
!http://www.clarionlife.net/content/view/159/29/
!http://www.clarionlife.net/content/view/153/29/
!translated pages seem to say that this will GPF in C50, but is ok in C55
UFO                             &TUFO_CallInterface
  CODE 
  ! COMPILE('***',_C70_) 
  !   MESSAGE('debuger.GetVarType has not been implemented')
  !   RETURN 0
  ! !END-COMPILE('***',_C70_)
    
  ! OMIT('***',_C70_)
  UFO &= ADDRESS(xaVar)
  RETURN UFO._Type( ADDRESS(xaVar) ) 
  !RETURN UFO._Type() !(42) !<-- my guess is that this arg. is ignored. -- confirmed
  !END-OMIT('***',_C70_)
!==============================================================================
ThreadDebugCore.DescribeDataType            PROCEDURE(LONG xType) !,STRING  !<-- assumes a DATATYPE:*  (see equates.clw)
!==============================================================================
RetVal CSTRING(25) !DataType:PDECIMAL has a length of 17
  CODE
  CASE xType                                                ! ITEMIZE(1),PRE(DataType)
    OF DataType:BYTE     ; RetVal = 'DataType:BYTE'         ! EQUATE
    OF DataType:SHORT    ; RetVal = 'DataType:SHORT'        ! EQUATE
    OF DataType:USHORT   ; RetVal = 'DataType:USHORT'       ! EQUATE
    OF DataType:DATE     ; RetVal = 'DataType:DATE'         ! EQUATE
    OF DataType:TIME     ; RetVal = 'DataType:TIME'         ! EQUATE
    OF DataType:LONG     ; RetVal = 'DataType:LONG'         ! EQUATE
    OF DataType:ULONG    ; RetVal = 'DataType:ULONG'        ! EQUATE
    OF DataType:SREAL    ; RetVal = 'DataType:SREAL'        ! EQUATE
    OF DataType:REAL     ; RetVal = 'DataType:REAL'         ! EQUATE
    OF DataType:DECIMAL  ; RetVal = 'DataType:DECIMAL'      ! EQUATE
    OF DataType:PDECIMAL ; RetVal = 'DataType:PDECIMAL'     ! EQUATE
    OF DataType:BFLOAT4  ; RetVal = 'DataType:BFLOAT4'      ! EQUATE(13)
    OF DataType:BFLOAT8  ; RetVal = 'DataType:BFLOAT8'      ! EQUATE
    OF DataType:STRING   ; RetVal = 'DataType:STRING'       ! EQUATE(18)
    OF DataType:CSTRING  ; RetVal = 'DataType:CSTRING'      ! EQUATE
    OF DataType:PSTRING  ; RetVal = 'DataType:PSTRING'      ! EQUATE
    OF DataType:MEMO     ; RetVal = 'DataType:MEMO'         ! EQUATE
    OF DataType:BLOB     ; RetVal = 'DataType:BLOB'         ! EQUATE(27)
  ELSE                   ; RetVal = 'DataType:Unknown['& xType &']'
                           !31 seems to be returned for references (and ANY)  (c6.9056)
  END
  RETURN RetVal
!==============================================================================
! RA.2018.12.30.SUN: Give me a DataTyep(Lenght) without a prefix. 
ThreadDebugCore.DescribeDataTypeShort       PROCEDURE(LONG xType, LONG xSize) !,STRING  !<-- assumes a DATATYPE:*  (see equates.clw)
!==============================================================================
RetVal  CSTRING(25) !DataType:PDECIMAL has a length of 17
iDo     LONG(0)
  CODE
  RetVal = SELF.DescribeDataType(xType)
  RetVal = SUB(RetVal, 10, LEN(RetVal) - 9) ! SIZE('DataType:') = 9
  CASE xType
    OF DataType:STRING
  OROF DataType:CSTRING
  OROF DataType:PSTRING
  OROF DataType:MEMO
  OROF DataType:BLOB
    RetVal = RetVal & '(' & xSize & ')'
  END
  RETURN RetVal
!==============================================================================
ThreadDebugCore.DescribeType                PROCEDURE(LONG xType)
!==============================================================================
RetVal CSTRING(25)
CREATE:_ComboButton   EQUATE(CREATE:combo + 0100H)  !see NG:  Softvelocity.clarion.documentation  Thread:"What is {prop:type}=271"  Sept-18-2006
  CODE
  CASE xType
    OF CREATE:sstring      ; RetVal = 'CREATE:SSTRING'
    OF CREATE:string       ; RetVal = 'CREATE:STRING'
    OF CREATE:image        ; RetVal = 'CREATE:IMAGE'
    OF CREATE:region       ; RetVal = 'CREATE:REG' & 'ION' !hopefully R E G I O N won't trigger the code folding bugs
    OF CREATE:line         ; RetVal = 'CREATE:LINE'
    OF CREATE:box          ; RetVal = 'CREATE:BOX'
    OF CREATE:ellipse      ; RetVal = 'CREATE:ELLIPSE'
    OF CREATE:entry        ; RetVal = 'CREATE:ENTRY'
    OF CREATE:button       ; RetVal = 'CREATE:BUTTON'
    OF CREATE:prompt       ; RetVal = 'CREATE:PROMPT'
    OF CREATE:option       ; RetVal = 'CREATE:OPTION'
    OF CREATE:check        ; RetVal = 'CREATE:CHECK'
    OF CREATE:group        ; RetVal = 'CREATE:GROUP'
    OF CREATE:list         ; RetVal = 'CREATE:LIST'
    OF CREATE:combo        ; RetVal = 'CREATE:COMBO'
    OF CREATE:spin         ; RetVal = 'CREATE:SPIN'
    OF CREATE:text         ; RetVal = 'CREATE:TEXT'
    OF CREATE:custom       ; RetVal = 'CREATE:CUSTOM'
    OF CREATE:menu         ; RetVal = 'CREATE:MENU'
    OF CREATE:item         ; RetVal = 'CREATE:ITEM'
    OF CREATE:radio        ; RetVal = 'CREATE:RADIO'
    OF CREATE:menubar      ; RetVal = 'CREATE:MENUBAR'
    OF CREATE:application  ; RetVal = 'CREATE:APPLICATION'
    OF CREATE:window       ; RetVal = 'CREATE:WINDOW'
    OF CREATE:report       ; RetVal = 'CREATE:REPORT'
    OF CREATE:header       ; RetVal = 'CREATE:HEADER'
    OF CREATE:footer       ; RetVal = 'CREATE:FOOTER'
    OF CREATE:break        ; RetVal = 'CREATE:BREAK'
    OF CREATE:form         ; RetVal = 'CREATE:FORM'
    OF CREATE:detail       ; RetVal = 'CREATE:DETAIL'
    OF CREATE:ole          ; RetVal = 'CREATE:OLE'
    OF CREATE:droplist     ; RetVal = 'CREATE:DROPLIST'
    OF CREATE:dropcombo    ; RetVal = 'CREATE:DROPCOMBO'
    OF CREATE:progress     ; RetVal = 'CREATE:PROGRESS'
    OF CREATE:sheet        ; RetVal = 'CREATE:SHEET'
    OF CREATE:tab          ; RetVal = 'CREATE:TAB'
    OF CREATE:panel        ; RetVal = 'CREATE:PANEL'
                                                   COMPILE('**++** _C60_Only_',_C60_)
    OF CREATE:rtf          ; RetVal = 'CREATE:RTF'
                                              !END-COMPILE('**++** _C60_Only_',_C60_)
    OF CREATE:sublist      ; RetVal = 'CREATE:SUBLIST'     ! found via  ?Combo_or_List_WithDrop{prop:ListFEQ)
    OF CREATE:toolbar      ; RetVal = 'CREATE:TOOLBAR'
    OF CREATE:_ComboButton ; RetVal = 'CREATE:COMBOBUTTON' ! UN-OFFICIAL the control found via: ?Combo{prop:ButtonFEQ}
  ELSE                     ; RetVal = 'CREATE:Unknown['& xType &']'
  END
  RETURN RetVal
!==============================================================================
! RA.2019.01.07: Added this method.
ThreadDebugCore.DumpRecord                  PROCEDURE(*FILE pFile)
!==============================================================================
  CODE
  IF SELF.DebugOff THEN RETURN END
  SELF.DumpRecord(pFile, '')
  RETURN
!==============================================================================
ThreadDebugCore.DumpRecord                  PROCEDURE(*FILE pFile,STRING xLinePrefix)
!==============================================================================
G           &GROUP
  CODE
  IF SELF.DebugOff THEN RETURN END
  G &= pFile{PROP:Record}
  SELF.Debug('DumpRecord( ' & NAME(pFile) & ' )')
  SELF.DumpGroup(G, xLinePrefix)
  RETURN
!==============================================================================
!==============================================================================
!==============================================================================
ThreadDebugCore.DumpGroupWindow             PROCEDURE(*GROUP pGroup)
!==============================================================================
  CODE
  IF SELF.DebugOff THEN RETURN END
  SELF.DumpGroupWindow(pGroup,'')
  RETURN
!==============================================================================
! RA.2019.01.07: Added this method.
ThreadDebugCore.DumpRecordWindow            PROCEDURE(*FILE pFile)
!==============================================================================
  CODE
  IF SELF.DebugOff THEN RETURN END
  SELF.DumpRecordWindow(pFile,'')
  RETURN
!==============================================================================	
ThreadDebugCore.DumpRecordWindow            PROCEDURE(*FILE pFile,STRING xMsg,<STRING pStructureType>)
!==============================================================================
G           &GROUP
	CODE
	IF SELF.DebugOff THEN RETURN END
	G &= pFile{PROP:Record}
	SELF.DumpGroupWindow(G, xMsg, CHOOSE(pStructureType='', NAME(pFile), NAME(pFile) & ' ' & pStructureType))
	RETURN
!==============================================================================
! RA.2018.12.28: This was modified from the original Mike Hanson code and moved to core.
ThreadDebugCore.DumpGroupWindow             PROCEDURE(*GROUP pGroup,STRING pMsg,<STRING pStructureType>)
!==============================================================================
SavePointer   LONG,AUTO
NumFields     SHORT(0)
!NumFields2    SHORT(0)
FieldQ        QUEUE
Name            CSTRING(101)
Type            CSTRING(256) !(1000) !Shortened to match PROP:Format
Value           CSTRING(256) !(1000) !Shortened to match PROP:Format
              END
MsgLineQ      QUEUE
Text            STRING(100)
              END
!------------------------------------------------------------------------------
Window        WINDOW('Dump'),AT(,,676,416),FONT('Segoe UI',10,,),CENTER,SYSTEM,GRAY,DOUBLE
                LIST,AT(4,4,668,356),USE(?DebugList),VSCROLL,FROM(FieldQ),FORMAT(|
                  '200L(2)|M~Field Name~S(1)@S100@'   & |
                  '80L(2)|M~Type~S(1)@S255@'          & |
                  '1000L(2)|M~Value~S(1)@S255@')
                LIST,AT(4,364,668,48),USE(?MessageList),VSCROLL,FROM(MsgLineQ)
              END
!------------------------------------------------------------------------------
	CODE
                                    	     COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	DO LoadFieldQ
	!--- Prepare window
	OPEN(Window)
	0{PROP:Text} = 0{PROP:Text} &' '& CHOOSE(pStructureType='', 'Group', pStructureType) &' ('& NumFields &' Fields)'
	IF pMsg
		SELF.FormatMessageList(pMsg, MsgLineQ)
	ELSE
		HIDE(?MessageList)
		?DebugList{PROP:Height} = ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
	END
	!--- Display window
	ACCEPT
	END
                                            !ENDCOMPILE
	RETURN
!------------------------------------------------------------------------------
LoadFieldQ                                  ROUTINE
	DATA
X       SHORT,AUTO
M       SHORT(0)
_What   ANY                  !Field reference for value assignment
_Type   LONG(0)
_Size   LONG(0)
_Data   CSTRING(31)
  CODE
  LOOP X = 10000 TO 1 BY -1
    IF NumFields = 0 AND WHO(pGroup, X) <> ''
      NumFields = X
      IF M = 0
        M = NumFields
      END
    END
    IF M = 0 AND NumFields
      M = X
    END
    IF NumFields
      BREAK
    END
  END
  LOOP X = 1 TO M
    CLEAR(FieldQ)
    IF NumFields >= X
      FieldQ.Name   = WHO(pGroup, X)
      _What        &= WHAT(pGroup, X, 1)
      _Type         = SELF.GetVarType(_What)
      _Size         = SELF.GetVarSize(_What)
      IF NOT ISGROUP(pGroup, X)
        ! RA.2019.01.07: Move the description here so groups would not show a type
        !FieldQ.Type   = SELF.DescribeDataType(_Type) & '(' & _Size & ')'
        FieldQ.Type   = SELF.DescribeDataTypeShort(_Type,_Size)             ! RA.2018.12.30.SUN: Changed
                              _Data = _What
        CASE _Type
        OF DataType:DATE   ;  _Data = CLIP(FORMAT(_What,@D010-B))
        OF DataType:TIME   ;  _Data = CLIP(FORMAT(_What,@T04B))
        OF DataType:STRING ;  _Data = CLIP       (_What)
        END
        FieldQ.Value        = _Data
      END
    END
    ADD(FieldQ)
    ASSERT(ERRORCODE()=0)
  END
  EXIT
!==============================================================================
ThreadDebugCore.FormatMessageList           PROCEDURE(STRING pMsg,*QUEUE pMsgQueue)
!==============================================================================
StartPos                            LONG(1)
Pos                                 LONG,AUTO
  CODE
	IF pMsg
		LOOP WHILE StartPos
			Pos = INSTRING('|', pMsg, 1, StartPos)
			pMsgQueue = CHOOSE(Pos=0, pMsg[StartPos : LEN(pMsg)], pMsg[StartPos : Pos-1])
			ADD(pMsgQueue)
			ASSERT(ERRORCODE()=0)
			StartPos = Pos+1
		UNTIL Pos = 0
	END
  RETURN
!==============================================================================
!==============================================================================
!==============================================================================
! RA.2018.12.29: This are was modified from the original Mike Hanson code and moved to core.
ThreadDebugCore.DumpQueueWindow             PROCEDURE(*QUEUE pQueue,<STRING pMsg>,<BYTE pNoTouch>)
!==============================================================================
                MAP
DebugQueue:LoadFieldQ                       PROCEDURE
                END
!------------------------------------------------------------------------------
SavePointer                         LONG,AUTO
NumFields                           SHORT,AUTO
                  COMPILE('***---***', E_StripPrefix)
!StripPrefixLength   BYTE,AUTO
                  ***---***
FieldQ                              QUEUE
Header                                  CSTRING(100)    ! Field name <13,10>TYPE(Size)
Width                                   LONG            ! Display size on screen
IsNumeric                               BYTE                               
IsDataUpper                             BYTE
IsGroup                                 BYTE
Type                                    LONG            ! TUFO: Field Data Type
Size                                    LONG            ! TUFO: Field Byte Size
!Data                                   CSTRING(31)     ! Limit the display shown
                  END
MsgLineQ                            QUEUE
Text                                    STRING(1000)
                  END
!------------------------------------------------------------------------------
Window    WINDOW('Dump Queue'),SYSTEM,AT(,,676,416),CENTER,FONT('Segoe UI',10),GRAY,DOUBLE
                    LIST, AT(4,4,668,356), USE(?DebugList), HVSCROLL
                    LIST, AT(4,364,668,48), USE(?MessageList), VSCROLL, FROM(MsgLineQ)
                  END
!------------------------------------------------------------------------------
  CODE
                                                                                            COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
  IF SELF.DebugOff THEN RETURN END
  IF pQueue &= NULL
    MESSAGE('Queue passed to ST::DebugQueue was a NULL pointer!', 'Dump Queue')
  ELSE
    !--- Save current queue pointer
    SavePointer = CHOOSE(RECORDS(pQueue)=0, 0, POINTER(pQueue))
    !--- Scan passed queue
    DO FindLastField
    IF NumFields = 0
      MESSAGE('Queue passed to ST::DebugQueue has no fields!', 'Dump Queue')
    ELSE
                              COMPILE('***---***', E_StripPrefix)
      DO CheckStripPrefix
                                       ***---***
      DebugQueue:LoadFieldQ()
      !--- Prepare window
      OPEN(Window)
      0{PROP:Text} = 0{PROP:Text} &' ('& NumFields &' Fields, '& RECORDS(pQueue) &' Records)'
      DO FormatFieldList
      IF pMsg
        SELF.FormatMessageList(pMsg, MsgLineQ)
      ELSE
        HIDE(?MessageList)
        ?DebugList{PROP:Height} = ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
      END
      !--- Display window
      ACCEPT
      END
      !--- Restore queue pointer
      IF SavePointer <> 0 AND pNoTouch <> 1
        GET(pQueue, SavePointer)
      END
    END
  END                                                                                                       !ENDCOMPILE
  RETURN
!------------------------------------------------------------------------------
FindLastField                               ROUTINE
!------------------------------------------------------------------------------
  NumFields = 5000
  LOOP WHILE NumFields > 0  |
      AND   WHO(pQueue, NumFields) = ''
    NumFields -= 1
  END
  EXIT

! Consider this instead: (Also consider Binary searches, vs. += 1)
! NumFields = 0
! LOOP
!   NumFields += 1
!   IF WHAT(pQueue, NumFields) &= NULL THEN BREAK END
! END
! NumFields -= 1

!------------------------------------------------------------------------------
  COMPILE('***---***', E_StripPrefix)
!------------------------------------------------------------------------------
CheckStripPrefix                            ROUTINE
!------------------------------------------------------------------------------
  DATA
FieldNo         SHORT,AUTO
PrefixFound     CSTRING(20)
!------------------------------------------------------------------------------
  CODE
  LOOP FieldNo = 1 TO NumFields
    FieldQ.Header = WHO(pQueue, FieldNo)
    StripPrefixLength = INSTRING(':', FieldQ.Header)
    IF StripPrefixLength
      IF FieldNo = 1
        PrefixFound = FieldQ.Header[1:S]
      ELSIF FieldQ.Header[1:S] <> PrefixFound
        StripPrefixLength = 0
        BREAK
      END
    ELSIF PrefixFound
      StripPrefixLength = 0
      BREAK
    END
  END
  EXIT
  ***---***
!------------------------------------------------------------------------------
FormatFieldList                             ROUTINE
!------------------------------------------------------------------------------
! RA.2019.01.05.SAT: Verified calculations are only displaying a maximum of 30 bytes
! RA.2018.12.29.SAT: TODO [???]: Limit the data displayed to 30 bytes for large string type fields.
! RA.2018.12.29.SAT: The field formatting for DATE and TIME is working. Had to remove the groups.
  DATA
FieldNo         SHORT,AUTO
ColumnNo        SHORT(0)
  CODE
  ?DebugList{PROP:From}     = pQueue
  ?DebugList{PROP:Selected} = SavePointer
  LOOP FieldNo = 1 TO NumFields
    GET(FieldQ, FieldNo)
    IF FieldQ.IsGroup
      CYCLE
    END
    ColumnNo += 1
    ! RA.2018.12.29: The adding of the <13,10>TYPE(Size) to the WHO(WHAT()) just works out.
    !?DebugList{PROPLIST:Header                       , ColumnNo} = FieldQ.Header
    ?DebugList{PROPLIST:Header                        , ColumnNo} = FieldQ.Header & '<13><10>' & SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size)
    !SELF.Debug('FieldQ.Type=[' & FieldQ.Type & ']=' & SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size) & ' ====( ' & FieldQ.Header &  ' )====')
    !!SELF.Debug('FieldQ: '& FieldQ.Header &' - W:'& FieldQ.Width &' - N:'& FieldQ.IsNumeric &' - U:'& FieldQ.IsDataUpper &' - G:'& FieldQ.IsGroup &' - T:'& SELF.DescribeDataType(FieldQ.Type) &' - S:'& FieldQ.Size &' - D:'& FieldQ.Data&' - DS:' & SIZE(FieldQ.Data)-1 &' - DL:' & LEN(FieldQ.Data))
    CASE FieldQ.Type
    OF DataType:DATE   ; ?DebugList{PROPLIST:Picture  , ColumnNo} = '@D010-B'
    OF DataType:TIME   ; ?DebugList{PROPLIST:Picture  , ColumnNo} = '@T04B'
    ELSE               ; ?DebugList{PROPLIST:Picture  , ColumnNo} = '@S' & FieldQ.Width & ''
    END
    !!SELF.Debug('FieldQ.Type=[' & FieldQ.Type & ']=' & SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size) & ' ====( ' & ?DebugList{PROPLIST:Picture, ColumnNo} &  ' )====')
    !?DebugList{PROPLIST:Picture    +PROPLIST:Group, ColumnNo} = '@S'& FieldQ.Width
    !?DebugList{PROPLIST:RightBorder+PROPLIST:Group, ColumnNo} = 1
    ?DebugList{PROPLIST:RightBorder                   , ColumnNo} = 1
    ?DebugList{PROPLIST:Resize                        , ColumnNo} = 1
    ?DebugList{PROPLIST:Width                         , ColumnNo} = FieldQ.Width
    ?DebugList{PROPLIST:HeaderCenter                  , ColumnNo} = True
!   ?DebugList{PROPLIST:HeaderLeft                    , ColumnNo} = True
!   ?DebugList{PROPLIST:HeaderLeftOffset              , ColumnNo} = 1
    IF FieldQ.IsNumeric
      ?DebugList{PROPLIST:Right                       , ColumnNo} = True
      ?DebugList{PROPLIST:RightOffset                 , ColumnNo} = 1
    ELSE
      ?DebugList{PROPLIST:Left                        , ColumnNo} = True
      ?DebugList{PROPLIST:LeftOffset                  , ColumnNo} = 1
    END
    ?DebugList{PROPLIST:FieldNo                       , ColumnNo} = FieldNo
    !SELF.Debug('COLUMN[ ' & ColumnNo & ' ]=' & ?DebugList{PROPLIST:Picture, ColumnNo} & '=' & ?DebugList{PROPLIST:Format, ColumnNo} & '=')
    !STOP('COLUMN[ ' & ColumnNo & ' ]=' & ?DebugList{PROPLIST:Picture, ColumnNo} & '=' & ?DebugList{PROPLIST:Format, ColumnNo} & '=')
  END
  !SELF.Debug(?DebugList{PROP:Format})
  !STOP(?DebugList{PROP:Format})
  !ST::Debug(?DebugList{PROP:Format})
  EXIT
!------------------------------------------------------------------------------
DebugQueue:LoadFieldQ                       PROCEDURE
!------------------------------------------------------------------------------
! RA.2018.12.29.SAT: Added the TUFO interface to this.
! This goes thru each column and check all the records to approximate the width that we have
! to display to get all the rows of data. There is an arbitrary maximum for this since we do
! not want to display 2000 bytes of data on a queue display for just one field of data.
FieldRef                            ANY
_What                               ANY
_Type                               LONG,AUTO
_Size                               LONG,AUTO
_Data                               CSTRING(31),AUTO
FieldNo                             SHORT,AUTO
RecNo                               LONG,AUTO
SampleLength                        LONG,AUTO
HeaderLength                        LONG,AUTO
DataLength                          LONG,AUTO
IsDataUpper                         BOOL,AUTO
!------------------------------------------------------------------------------
  CODE
  !ST::Debug('ST::DebugQueue/LoadFieldQ/IN: NumFields='& NumFields)
  LOOP FieldNo = 1 TO NumFields
    CLEAR(FieldQ)
    !ST::Debug('ST::DebugQueue/LoadFieldQ: FieldNo='& FieldNo)
    FieldQ.Header                   = LOWER(WHO(pQueue, FieldNo))
    COMPILE('***---***', E_StripPrefix)
    IF StripPrefixLength
      HeaderLength                  = LEN(FieldQ.Header) - StripPrefixLength
      FieldQ.Header                 = SUB(FieldQ.Header, StripPrefixLength+1, HeaderLength)
    ELSE
    ***---***
      HeaderLength                  = LEN(FieldQ.Header)
    COMPILE('***---***', E_StripPrefix)
    END
    ***---***
    IF HeaderLength < 1
      HeaderLength                  = 1
    END
    COMPILE('***---***', _C60_)
    FieldRef                       &= WHAT(pQueue, FieldNo, 1)
    _What                          &= WHAT(pQueue, FieldNo, 1)
    ***---***
    OMIT('***---***', _C60_)
    FieldRef                       &= WHAT(pQueue, FieldNo)
    _What                          &= WHAT(pQueue, FieldNo)
    ***---***
!
    _Type                           = SELF.GetVarType(_What)
    _Size                           = SELF.GetVarSize(_What)
    _Data                           =                 _What
    FieldQ.Type                     = _Type
    FieldQ.Size                     = _Size
    !FieldQ.Data                     = _Data
!
    FieldQ.IsGroup                  = ISGROUP(pQueue, FieldNo)
    FieldQ.IsNumeric                = TRUE
    FieldQ.IsDataUpper              = FALSE
    IsDataUpper                     = FALSE
  !SELF.Debug('ST::DebugQueue/LoadFieldQ: RECORDS(pQueue)='& RECORDS(pQueue))
    IF RECORDS(pQueue) > 0 AND pNoTouch <> 1
      ! RA.2019.01.05.SAT: We have records, lets look at the data.
      DataLength                    = 0
      SampleLength                  = 0
      LOOP RecNo = 1 TO RECORDS(pQueue)
        GET(pQueue, RecNo)
  !SELF.Debug('ST::DebugQueue/LoadFieldQ: RecNo='& RecNo &'; '& SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size) &'; _What='& _What)
        IF FieldRef <> ''
          ! RA.2019.01.05.SAT: Improve with TUFO interface if posible. Not today though.
          !IF NOT NUMERIC(FieldRef)
          IF NOT NUMERIC(_What)
            FieldQ.IsNumeric        = FALSE
          END
          !IF NOT FieldQ.IsNumeric AND UPPER(FieldRef) = FieldRef
          IF NOT FieldQ.IsNumeric AND UPPER(_What) = _What
            FieldQ.IsDataUpper      = TRUE
            IsDataUpper             = TRUE
          END
          ! RA.2019.01.05.SAT: Improve with TUFO interface if posible. Not today though.
          !SampleLength             = LEN(CLIP(FieldRef))
          IF SampleLength < LEN(CLIP(_What))                                                    ! RA.2019.01.05.SAT: Keep bigest sample length
            SampleLength            = LEN(CLIP(_What))
            DataLength              = SampleLength
          END
          ! RA.2019.01.31.THU: Adjust specific data types for a better display length
          CASE(FieldQ.Type)
          OF DataType:DATE          ; DataLength = 8
          OF DataType:TIME          ; DataLength = 8
          END
  !SELF.Debug('Begin: '& FieldQ.Header &' - HL:'& HeaderLength &' - DL:'& DataLength &' - SL:'& SampleLength &' - W:'& FieldQ.Width & '; 001')
          ! RA.2019.01.05.SAT: Constraint what we display
          ! RA.2019.01.02.WED: Work on this !!!
          IF SampleLength > (SIZE(_Data) - 1)
            SampleLength            = (SIZE(_Data) - 1)
            DataLength              = SampleLength
          END
  !SELF.Debug('Begin: '& FieldQ.Header &' - HL:'& HeaderLength &' - DL:'& DataLength &' - SL:'& SampleLength &' - W:'& FieldQ.Width & '; 002')
          !
          IF DataLength < LEN(SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size))              ! Accomodate TYPE(LENGTH)
            DataLength              = LEN(SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size))
          END
  !SELF.Debug('Begin: '& FieldQ.Header &' - HL:'& HeaderLength &' - DL:'& DataLength &' - SL:'& SampleLength &' - W:'& FieldQ.Width & '; 003')
          ! RA.2019.01.05.SAT: Constraint what we display
        END
      END
    ELSE
      ! RA.2019.01.05.SAT: NO RECORDS: Estimate the column display size based on header size.
      FieldQ.IsNumeric              = FALSE
      FieldQ.IsDataUpper            = TRUE
      IsDataUpper                   = TRUE
      !DataLength                   = LEN(FieldRef)
      DataLength                    = LEN(FieldQ.Header)                                        ! Header length at minimum
      IF DataLength < LEN(SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size))                  ! Accomodate TYPE(LENGTH)
        DataLength                  = LEN(SELF.DescribeDataTypeShort(FieldQ.Type,FieldQ.Size))
      END
      IF DataLength > (SIZE(_Data) - 1)                                                         ! Limit the display size
        DataLength                  = (SIZE(_Data) - 1)
      END
    END
    DO CalculateColumnWidth
    ADD(FieldQ)
  END
  !ST::Debug('ST::DebugQueue/LoadFieldQ/OUT')
!------------------------------------------------------------------------------
! RA.2019.01.05.SAT: Calculations are estimating widths in a correct way.
! There are a lot of considerations for the functionality expressed here.
! Understanding comes with some looking/thinking about the code debug/trace.
! Please check out the prefixes to the queue fields to explain what they are.                 
! RA.2019.01.05.SAT: Constraint what we display
! RA.2019.01.02.WED: Work on this !!!
CalculateColumnWidth                        ROUTINE
  DATA
HeaderWidth     SHORT,AUTO
DataWidth       SHORT,AUTO
  CODE
  !SELF.Debug('FieldQ: '& FieldQ.Header &' - W:'& FieldQ.Width &' - N:'& FieldQ.IsNumeric &' - U:'& FieldQ.IsDataUpper &' - G:'& FieldQ.IsGroup &' - T:'& SELF.DescribeDataType(FieldQ.Type) &' - S:'& FieldQ.Size &' - D:'& FieldQ.Data&' - DS:' & SIZE(FieldQ.Data)-1 &' - DL:' & LEN(FieldQ.Data))
  !SELF.Debug('Begin: '& FieldQ.Header &' - HL:'& HeaderLength &' - DL:'& DataLength &' - HW:'& HeaderWidth &' - DW:'& DataWidth &' - SL:'& SampleLength &' - W:'& FieldQ.Width)
  HeaderWidth  = HeaderLength * WidthMultiplier:Narrow
  DataWidth    = DataLength * CHOOSE(~FieldQ.IsDataUpper, WidthMultiplier:Narrow, WidthMultiplier:Wide)
  FieldQ.Width = CHOOSE(DataWidth > HeaderWidth, DataWidth, HeaderWidth)
  !SELF.Debug('Ended: '& FieldQ.Header &' - HL:'& HeaderLength &' - DL:'& DataLength &' - HW:'& HeaderWidth &' - DW:'& DataWidth &' - SL:'& SampleLength &' - W:'& FieldQ.Width)

!==============================================================================
!==============================================================================
!==============================================================================
! RA.2018.12.31.MON: Copied from Mark Golberg's debuger. (needs some work)
ThreadDebugCore.mg_init                     PROCEDURE(STRING ProgramName)
!==============================================================================
  CODE
  SELF.Init(ProgramName,DEBUGER::ENABLED,0,DEBUGER::CLEAR_CMDLINE,DEBUGER::ASCII_NEVER) !DEBUGER::ASCII_PROMPT
  !SELF.AddAssertMsg(0,'^PrintEvent',AssertMsgAction::PrintEvent, Match:Regular + Match:NoCase )
  RETURN
!==============================================================================
! RA.2018.12.31.MON: Copied from Mark Golberg's debuger. (needs some work)
ThreadDebugCore.Init                        PROCEDURE(STRING argPgmname,BYTE argOSmode,SHORT argDuplicates,BYTE argClear, BYTE argAsciiByDefault)
!==============================================================================
  CODE
  !IF (SELF.DebugMe)
    SELF.Debug(':DEBUGME ThreadDebugCore.Init(...)')
  !END
  !SELF.DebugFilter   = '`'
  SELF.DebugPrefix    = '!'                 ! RA.2018.12.31: Notice the difference. MG's filtering is '`'
  SELF.AppendCRLF     = TRUE                ! mg Feb/5/04 -- may wish to turn off "Options->Force Carrigage Returns" in debugView
  !SELF.pgmname       = argPgmname
  SELF.szEXE          = argPgmname
  SELF.osMode         = argOSmode           ! this parameter if true, turns on debug regaredless of project setting
  SELF.Duplicates     = argDuplicates       ! default number of same debug messages in a row before a warning message is issued

  ! RA.2019.01.08: Reverse the LOGIC here (SELF.DebugOff vs SELF.DebugActive)
  CASE argOSMode
    OF DEBUGER::DISABLED       ; SELF.DebugOff = TRUE
    OF DEBUGER::ENABLED        ; SELF.DebugOff = FALSE
    OF DEBUGER::ENABLE_CMDLINE ; SELF.DebugOff = CHOOSE( COMMAND('/Debuger') = '' )
  ELSE                         ; SELF.DebugOff = FALSE
  END

  ! RA.2019.01.08: Add additional starting debug flag output
  IF (SELF.DebugMe)
    SELF.Debug(':DEBUGME ThreadDebugCore.Init SELF.DebugPrefix =( ' & CLIP(SELF.DebugPrefix) & ' )')
    SELF.Debug(':DEBUGME ThreadDebugCore.Init SELF.szEXE =( ' & CLIP(SELF.szEXE) & ' )')
    SELF.Debug(':DEBUGME ThreadDebugCore.Init SELF.LevelWarn=( ' & SELF.LevelWarn & ' )')
    SELF.Debug(':DEBUGME ThreadDebugCore.Init SELF.Duplicates=( ' & SELF.Duplicates & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Init SELF.AppendCRLF=( ' & SELF.AppendCRLF & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Init SELF.DebugOff=( ' & SELF.DebugOff & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Init SELF.osMode=( ' & SELF.osMode & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Init SELF.ShowMessageWindow_Beep=( ' & SELF.ShowMessageWindow_Beep & ' )')
    SELF.Debug(':DEBUGME:FLAG ThreadDebugCore.Init SELF.DelayActive=( ' & SELF.DelayActive & ' )')
  END 

!  SELF.DebugOut('Debuger  MOD:ClearedInInit['&MOD:ClearedInInit&'] argClear['&argClear&'] command(''/clear'')['&command('/clear')&'] SELF.debugactive['&SELF.debugactive&']')
  ! IF ~MOD:ClearedInInit
  !     CASE argClear
  !       OF DEBUGER::CLEAR_ALWAYS ;                           SELF.ClearLog(); MOD:ClearedInInit=TRUE
  !       OF DEBUGER::CLEAR_CMDLINE; IF COMMAND('/Clear') THEN SELF.ClearLog(); MOD:ClearedInInit=TRUE END
  !     END !case
  ! END

  ! IF SELF.DebugActive
  !   IF Verbose > 0
  !                          SELF.delayOut('Program['& CLIP(COMMAND(0)) &'] ')
  !     IF SELF.osMode  THEN SELF.debugout('DebugerOn[Always]')           ! force debug on regardless of debug in project
  !                     ELSE SELF.debugout('DebugerOn[Command line]')     ! force debug on a production app
  !     END
  !   END
  !   IF Verbose > 3
  !     SELF.debugout('Debuger Class last modified['& CLIP(FORMAT(DATE(12,11,2009),@D18)) &']')
  !     SELF.debugout('Debuger Class Updates: http://www.monolithcc.com/clarion/debuger.zip')
  !     SELF.DebugOut ('-{90}')
  !     SELF.debugout('')
  !   END
  ! END

  ! SELF.Set_dumpQue_DefaultFileName('ExportQ.csv')
  ! SELF.Set_dumpQue_AsciiByDefault(argAsciiByDefault)

  ! DO Init::SetEventOffset

  ! SELF.Set_Module_Debuger() !moved .Construct
  ! SELF.Set_AssertHook2()

  ! SELF.UserEventNameQ &= NEW qtUserEventName; CLEAR(SELF.UserEventNameQ)
  ! SELF.AddUserEvent('EVENT:DoResize',EVENT:User-1)

  ! SELF.AssertMessagesQ &= NEW qtAssertMessages; CLEAR(SELF.AssertMessagesQ)
  ! IF Verbose > 3 THEN ODS(SELF.DebugFilter &'Debuger.init [end]') END
  RETURN
!==============================================================================
! RA.2018.12.31.MON: Copied from Mark Golberg's debuger. (needs some work)
ThreadDebugCore.Kill                      PROCEDURE()
!==============================================================================
  CODE
  ! IF Verbose > 5 THEN ODS('`Debuger.kill (start) pgmname['& SELF.pgmname &'] SELF['& ADDRESS(SELF) &'] Records(DebugersQ)['& RECORDS(DebugersQ) &']') END

  ! IF SELF.debugactive
  !   SELF.debugout('Program['& CLIP(COMMAND(0)) &'] Ended ['& CLIP(FORMAT(TODAY(),@D18)) &' '& CLIP(FORMAT(CLOCK(),@T8)) &']')
  !   SELF.debugactive = FALSE
  ! END

  ! IF ~(SELF.UserEventNameQ &= NULL)
  !   FREE   (SELF.UserEventNameQ)
  !   DISPOSE(SELF.UserEventNameQ)
  ! END

  ! IF ~(SELF.AssertMessagesQ &= NULL)
  !   SELF.FreeAssertMsg()
  !   DISPOSE(SELF.AssertMessagesQ)
  ! END

  ! SELF.Clear_Module_Debuger()

  ! IF Verbose > 5 THEN ODS('`Debuger.kill (end) SELF['& ADDRESS(SELF) &']') END
  RETURN
!==============================================================================
!==============================================================================
!==============================================================================
!EndRegion DEBUGER Methods

  
!EndRegion ThreadDebugCore  
  
!Region UltimateDebug

!==============================================================================
ThreadDebugGlobal.Init                    PROCEDURE()
	CODE
  RETURN
!==============================================================================
ThreadDebugGlobal.Kill                    PROCEDURE()
	CODE
	RETURN
!==============================================================================
ThreadDebugGlobal.GetCategory             PROCEDURE(STRING pCategory)!,BOOL
	CODE
  IF (SELF.DebugMe) THEN
    SELF.RawODS('DEBUGME: ThreadDebugGlobal.GetCategory')
  END

	SELF.CategoryQueue.Category = pCategory
	GET(SELF.CategoryQueue,SELF.CategoryQueue.Category)
  RETURN CHOOSE( ERRORCODE() = 0 )
!==============================================================================
ThreadDebugGlobal.AddCategoryToDebug      PROCEDURE(STRING pCategory) !Used to Filter Messages not belonging to active Categories
	CODE
   IF ~ SELF.GetCategory(pCategory)
      SELF.CategoryQueue.Category = pCategory
	   ADD(SELF.CategoryQueue,SELF.CategoryQueue.Category)
	END
!==============================================================================
ThreadDebugGlobal.Debug                   PROCEDURE(STRING pCategory,STRING pDebugString)
	CODE
   IF SELF.GetCategory(pCategory)
		SELF.Debug('[' & CLIP(pCategory) & ']' & pDebugString)
	END
!==============================================================================
ThreadDebugGlobal.DebugEntry              PROCEDURE(STRING pCategory,STRING pDebugString)
!==============================================================================
    CODE
   IF SELF.GetCategory(pCategory)
		SELF.DebugEntry('[' & CLIP(pCategory) & ']' & pDebugString)
	END
    RETURN
!==============================================================================
ThreadDebugGlobal.DebugLevel              PROCEDURE(STRING pCategory,STRING pDebugString)
!==============================================================================
    CODE
   IF SELF.GetCategory(pCategory)
		SELF.DebugLevel('[' & CLIP(pCategory) & ']' & pDebugString)
	END
    RETURN
!==============================================================================
ThreadDebugGlobal.DebugExit               PROCEDURE(STRING pCategory,STRING pDebugString)
!==============================================================================
    CODE
   IF SELF.GetCategory(pCategory)
		SELF.DebugExit('[' & CLIP(pCategory) & ']' & pDebugString)
	END
    RETURN
!==============================================================================
!!! <summary>
!!! DEPRECATED (2019.01.27)
!!! </summary>
ThreadDebugGlobal.DebugRecord             PROCEDURE(*FILE pFile,STRING pMsg)
G                                   &GROUP
	CODE
	                                       COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	G &= pFile{PROP:Record}
	SELF.DebugGroup(G,, pMsg, 'Record')
	                                       !ENDCOMPILE
	RETURN
!==============================================================================
!!! <summary>
!!! DEPRECATED (2019.01.27)
!!! </summary>
ThreadDebugGlobal.DebugRecord             PROCEDURE(*FILE pFile,*FILE pFile2,STRING pMsg)
G1                                  &GROUP
G2                                  &GROUP
	CODE
                                    	      COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	G1 &= pFile {PROP:Record}
	G2 &= pFile2{PROP:Record}
	SELF.DebugGroup(G1, G2, pMsg, 'Record')
	                                         !ENDCOMPILE
	RETURN
!==============================================================================
!!! <summary>
!!! DEPRECATED (2019.01.27)
!!! </summary>
ThreadDebugGlobal.DebugGroup              PROCEDURE(*GROUP pGroup,STRING pMsg,<STRING pStructureType>)
	CODE
	                                       COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	SELF.DebugGroup(pGroup,, pMsg)
	                                       !ENDCOMPILE
	RETURN
!==============================================================================
!!! <summary>
!!! DEPRECATED (2019.01.27)
!!! </summary>
ThreadDebugGlobal.DebugGroup              PROCEDURE(*GROUP pGroup,<*GROUP pGroup2>,STRING pMsg,<STRING pStructureType>)
epGroup2                            EQUATE(2)
SavePointer                         LONG,AUTO
NumFields                           SHORT(0)
NumFields2                          SHORT(0)
FieldQ                     QUEUE
Name                                CSTRING(101)
Value                               CSTRING(256) !(1000) !Shortened to match PROP:Format
Value2                              CSTRING(256) !(1000) !Shortened to match PROP:Format
									END
MsgLineQ                   QUEUE
Text                                STRING(100)
									END
!------------------------------------------------------------------------------
Window    WINDOW('Debug'),AT(,,676,416),FONT('Tahoma',8,,),CENTER,SYSTEM,GRAY,DOUBLE
				LIST,AT(4,4,668,356),USE(?DebugList),VSCROLL,FROM(FieldQ),FORMAT(|
				      '125L(2)|M~Field Name~S(1)@S100@' & |
				      '180L(2)|M~Value~S(1)@S255@'      & |
				     '1000L(2)|M~Value2~S(1)@S255@')
				LIST,AT(4,364,668,48),USE(?MessageList),VSCROLL,FROM(MsgLineQ)
			 END
!------------------------------------------------------------------------------
	CODE
                                    	COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	DO LoadFieldQ
	!--- Prepare window
	OPEN(Window)
	IF OMITTED(epGroup2)
		?DebugList{PROPLIST:Width, 2} = 1000
	END
	0{PROP:Text} = 0{PROP:Text} &' '& CHOOSE(pStructureType='', 'Group', pStructureType) &' ('& NumFields &' Fields)'
	IF pMsg
		SELF.FormatMessageList(pMsg, MsgLineQ)
	ELSE
		HIDE(?MessageList)
		?DebugList{PROP:Height} = ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
	END
	!--- Display window
	ACCEPT
	END
	                                          !ENDCOMPILE
	RETURN
!======================================
LoadFieldQ                                  ROUTINE
	DATA
F       ANY                  !Field reference for value assignment
X       SHORT,AUTO
M       SHORT(0)
	CODE
!!	OMIT('!+++!!!+++!')
	LOOP X = 10000 TO 1 BY -1
		IF NumFields = 0 AND WHO(pGroup, X) <> ''
			NumFields = X
			IF M = 0
				M = NumFields
			END
		END
		IF NOT OMITTED(epGroup2)  |
				AND NumFields2 = 0 AND WHO(pGroup2, X) <> ''
			NumFields2 = X
			IF M = 0
				M = NumFields2
			END
		END
		IF M = 0 AND (NumFields OR NumFields2)
			M = X
		END
		IF NumFields AND (OMITTED(epGroup2) OR NumFields2)
			BREAK
		END
	END
	LOOP X = 1 TO M
		CLEAR(FieldQ)
		IF NumFields >= X
			FieldQ.Name   = WHO(pGroup, X)
			F            &= WHAT(pGroup, X, 1)
	IF NOT ISGROUP(pGroup, X)
			FieldQ.Value  = F
	END
			IF NumFields2 >= X
				DO AssignValue2
			END
		ELSE
			FieldQ.Name   = WHO(pGroup2, X)
			DO AssignValue2
		END
		ADD(FieldQ)
		ASSERT(ERRORCODE()=0)
	END
!!	!+++!!!+++!
	EXIT
!------------------------------------------------------------------------------
AssignValue2                    ROUTINE
!------------------------------------------------------------------------------
	                                       OMIT('!+++!!!+++!')
	F &= WHAT(pGroup2, X, 1)
	FieldQ.Value2 = F
	                                       !+++!!!+++!
	EXIT
!==============================================================================
!!! <summary>
!!! DEPRECATED (2019.01.27)
!!! </summary>
ThreadDebugGlobal.DebugQueue              PROCEDURE(*QUEUE pQueue,<STRING pMsg>,<BYTE pNoTouch>)
								MAP
DebugQueue:LoadFieldQ                       PROCEDURE
								END
!------------------------------------------------------------------------------
SavePointer                         LONG,AUTO
NumFields                           SHORT,AUTO
									COMPILE('***---***', E_StripPrefix)
!StripPrefixLength   BYTE,AUTO
									***---***
FieldQ                              QUEUE
Header                                  CSTRING(100)
Width                                   LONG
IsNumeric                               BOOL
IsGroup                                 BOOL
									END
MsgLineQ                            QUEUE
Text                                    STRING(1000)
									END
!------------------------------------------------------------------------------
Window    WINDOW('Debug Queue'),SYSTEM,AT(,,676,416),CENTER,FONT('Tahoma', 8),GRAY,DOUBLE
										LIST, AT(4,4,668,356), USE(?DebugList), HVSCROLL
										LIST, AT(4,364,668,48), USE(?MessageList), VSCROLL, FROM(MsgLineQ)
									END
!------------------------------------------------------------------------------
	CODE
	                                                                                          COMPILE('!ENDCOMPILE', ST::DEBUG:Debugging=1)
	IF SELF.DebugOff THEN RETURN END
	IF pQueue &= NULL
		MESSAGE('Queue passed to ST::DebugQueue was a NULL pointer!', 'Debug Queue')
	ELSE
		!--- Save current queue pointer
		SavePointer = CHOOSE(RECORDS(pQueue)=0, 0, POINTER(pQueue))
		!--- Scan passed queue
		DO FindLastField
		IF NumFields = 0
			MESSAGE('Queue passed to ST::DebugQueue has no fields!', 'Debug Queue')
		ELSE
			                        COMPILE('***---***', E_StripPrefix)
			DO CheckStripPrefix
			                                 ***---***
			DebugQueue:LoadFieldQ()
			!--- Prepare window
			OPEN(Window)
			0{PROP:Text} = 0{PROP:Text} &' ('& NumFields &' Fields, '& RECORDS(pQueue) &' Records)'
			DO FormatFieldList
			IF pMsg
				SELF.FormatMessageList(pMsg, MsgLineQ)
			ELSE
				HIDE(?MessageList)
				?DebugList{PROP:Height} = ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
			END
			!--- Display window
			ACCEPT
			END
			!--- Restore queue pointer
			IF SavePointer <> 0 AND pNoTouch <> 1
				GET(pQueue, SavePointer)
			END
		END
	END
	                                                                                                           !ENDCOMPILE
	RETURN
!======================================
FindLastField                               ROUTINE
!------------------------------------------------------------------------------
	NumFields = 5000
	LOOP WHILE NumFields > 0  |
			AND   WHO(pQueue, NumFields) = ''
		NumFields -= 1
	END
	EXIT

! Consider this instead: (Also consider Binary searches, vs. += 1)
!	NumFields = 0
!	LOOP
!	  NumFields += 1
!	  IF WHAT(pQueue, NumFields) &= NULL THEN BREAK END
!	END
!	NumFields -= 1

!**************************************
	COMPILE('***---***', E_StripPrefix)
!======================================
CheckStripPrefix                ROUTINE
!------------------------------------------------------------------------------
	DATA
FieldNo SHORT,AUTO
PrefixFound     CSTRING(20)
!------------------------------------------------------------------------------
	CODE
	LOOP FieldNo = 1 TO NumFields
		FieldQ.Header = WHO(pQueue, FieldNo)
		StripPrefixLength = INSTRING(':', FieldQ.Header)
		IF StripPrefixLength
			IF FieldNo = 1
				PrefixFound = FieldQ.Header[1:S]
			ELSIF FieldQ.Header[1:S] <> PrefixFound
				StripPrefixLength = 0
				BREAK
			END
		ELSIF PrefixFound
			StripPrefixLength = 0
			BREAK
		END
	END
	EXIT
	***---***
!======================================
FormatFieldList                             ROUTINE
!------------------------------------------------------------------------------
	DATA
FieldNo SHORT,AUTO
ColumnNo        SHORT(0)
!------------------------------------------------------------------------------
	CODE
	?DebugList{PROP:From} = pQueue
	?DebugList{PROP:Selected} = SavePointer
	LOOP FieldNo = 1 TO NumFields
		GET(FieldQ, FieldNo)
		IF FieldQ.IsGroup
			CYCLE
		END
		ColumnNo += 1
		?DebugList{PROPLIST:Header                    , ColumnNo} = FieldQ.Header
		?DebugList{PROPLIST:Picture    +PROPLIST:Group, ColumnNo} = '@S'& FieldQ.Width
		?DebugList{PROPLIST:RightBorder+PROPLIST:Group, ColumnNo} = 1
		?DebugList{PROPLIST:Resize                    , ColumnNo} = 1
		?DebugList{PROPLIST:Width                     , ColumnNo} = FieldQ.Width
		?DebugList{PROPLIST:HeaderCenter              , ColumnNo} = True
!   ?DebugList{PROPLIST:HeaderLeft                , ColumnNo} = True
!   ?DebugList{PROPLIST:HeaderLeftOffset          , ColumnNo} = 1
		IF FieldQ.IsNumeric
			?DebugList{PROPLIST:Right                   , ColumnNo} = True
			?DebugList{PROPLIST:RightOffset             , ColumnNo} = 1
		ELSE
			?DebugList{PROPLIST:Left                    , ColumnNo} = True
			?DebugList{PROPLIST:LeftOffset              , ColumnNo} = 1
		END
		?DebugList{PROPLIST:FieldNo                   , ColumnNo} = FieldNo
	END
	!ST::Debug(?DebugList{PROP:Format})
	EXIT
!==============================================================================
DebugQueue:LoadFieldQ                       PROCEDURE
!------------------------------------------------------------------------------
FieldNo                             SHORT,AUTO
FieldRef                            ANY
RecNo                               LONG,AUTO
SampleLength                        LONG,AUTO
HeaderLength                        LONG,AUTO
DataLength                          LONG,AUTO
IsDataUpper                         BOOL,AUTO
!------------------------------------------------------------------------------
	CODE
	!ST::Debug('ST::DebugQueue/LoadFieldQ/IN: NumFields='& NumFields)
	LOOP FieldNo = 1 TO NumFields
		CLEAR(FieldQ)
		!ST::Debug('ST::DebugQueue/LoadFieldQ: FieldNo='& FieldNo)
		FieldQ.Header                  = LOWER(WHO(pQueue, FieldNo))
		COMPILE('***---***', E_StripPrefix)
		IF StripPrefixLength
			HeaderLength                 = LEN(FieldQ.Header) - StripPrefixLength
			FieldQ.Header                = SUB(FieldQ.Header, StripPrefixLength+1, HeaderLength)
		ELSE
			***---***
			HeaderLength                 = LEN(FieldQ.Header)
			COMPILE('***---***', E_StripPrefix)
		END
		***---***
		IF HeaderLength < 1
			HeaderLength                 = 1
		END
		COMPILE('***---***', _C60_)
		FieldRef                      &= WHAT(pQueue, FieldNo, 1)
		***---***
		OMIT('***---***', _C60_)
		FieldRef                      &= WHAT(pQueue, FieldNo)
		***---***
		FieldQ.IsGroup                 = ISGROUP(pQueue, FieldNo)
		FieldQ.IsNumeric               = TRUE
		IsDataUpper                    = FALSE
		!ST::Debug('ST::DebugQueue/LoadFieldQ: RECORDS(pQueue)='& RECORDS(pQueue))
		IF RECORDS(pQueue) > 0 AND pNoTouch <> 1
			DataLength                   = 0
			LOOP RecNo = 1 TO RECORDS(pQueue)
				GET(pQueue, RecNo)
				!ST::Debug('ST::DebugQueue/LoadFieldQ: RecNo='& RecNo &'; FieldRef='& FieldRef)
				IF FieldRef <> ''
					IF NOT NUMERIC(FieldRef)
						FieldQ.IsNumeric       = FALSE
					END
					SampleLength = LEN(CLIP(FieldRef))
					IF NOT FieldQ.IsNumeric AND UPPER(FieldRef) = FieldRef
						IsDataUpper            = TRUE
					END
					IF SampleLength > 25
						DataLength             = 25
					ELSIF DataLength < SampleLength
						DataLength             = SampleLength
					END
				END
			END
		ELSE
			IsDataUpper                  = TRUE
			FieldQ.IsNumeric             = FALSE
			DataLength                   = LEN(FieldRef)
		END
		DO CalculateColumnWidth
		ADD(FieldQ)
	END
	!ST::Debug('ST::DebugQueue/LoadFieldQ/OUT')
!------------------------------------------------------------------------------
CalculateColumnWidth                        ROUTINE
	DATA
HeaderWidth     SHORT,AUTO
DataWidth       SHORT,AUTO
	CODE
	HeaderWidth  = HeaderLength * WidthMultiplier:Narrow
	DataWidth    = DataLength * CHOOSE(~IsDataUpper, WidthMultiplier:Narrow, WidthMultiplier:Wide)
	FieldQ.Width = CHOOSE(DataWidth > HeaderWidth, DataWidth, HeaderWidth)
	!ST::Debug(FieldQ.Header &' - '& HeaderLength &' - '& DataLength &' - '& HeaderWidth &' - '& DataWidth &' - '& FieldQ.Width)

!==============================================================================
ThreadDebugGlobal.Message                 PROCEDURE(STRING pDebugString)
!==============================================================================
    CODE
    SELF.Debug(pDebugString)
!==============================================================================
ThreadDebugGlobal.SetApplicationName      PROCEDURE(STRING pApplicationName,STRING pProgramExtension)    !,STRING
!==============================================================================
ApplicationName   STRING(50)
  CODE
  CASE UPPER(CLIP(pProgramExtension))
    OF 'EXE' ;  ApplicationName = 'Application  <9>' & pApplicationName & '.EXE'
    OF 'DLL' ;  ApplicationName = 'DLL          <9>' & pApplicationName & '.DLL'
    OF 'LIB' ;  ApplicationName = 'Library      <9>' & pApplicationName & '.LIB'
  END
  RETURN ApplicationName
!==============================================================================
ThreadDebugGlobal.SetShortApplicationName PROCEDURE(STRING pApplicationName,STRING pProgramExtension)    !,STRING
!==============================================================================
ApplicationName   STRING(50)
  CODE
  CASE UPPER(CLIP(pProgramExtension))
    OF 'EXE' ;  ApplicationName = pApplicationName & '.EXE'
    OF 'DLL' ;  ApplicationName = pApplicationName & '.DLL'
    OF 'LIB' ;  ApplicationName = pApplicationName & '.LIB'
  END
  RETURN ApplicationName
  !Consider   !RETURN pApplicationName & '.' & pProgramExtension
!==============================================================================
ThreadDebugGlobal.ShowProcedureInfo       PROCEDURE(STRING pProcedure,STRING pApplication,STRING pHelpID,STRING pCreated,STRING pModified,STRING pCompiled)
!==============================================================================
!TheStats                               ANY			! RA.2014.06.04 - This does not work
TheStats				STRING(2048)            ! RA.2014.06.04 - This does work
Window                                  WINDOW('Procedure Information'),AT(,,275,121),CENTER,GRAY
                                          GROUP('Procedure Information'),AT(3,8,190,102),USE(?GROUP1),BOXED
                                          END
                                          TEXT,AT(11,20,176,84),USE(TheStats),SKIP,TRN
                                          BUTTON('Send To Clipboard'),AT(198,61,72),USE(?BUTTONToClipboard)
                                          BUTTON('Send To Debug'),AT(198,78,72),USE(?BUTTONToDebug)
                                          BUTTON('Close'),AT(198,96,72),USE(?BUTTONClose)
                                        END
  CODE
  DO ClearKeyStrokes

  TheStats = ('Procedure:<9>'  & CLIP(pProcedure)   & '<13,10>'          & |
                                 CLIP(pApplication) & '<13,10><13,10>'   & |
              'Help ID    <9>' & CLIP(pHelpID)      & '<13,10>'          & |
              'Created  On<9>' & CLIP(pCreated)     & '<13,10>'          & |
              'Modified On<9>' & CLIP(pModified)    & '<13,10>'          & |
              'Compiled On<9>' & CLIP(pCompiled))
  OPEN(Window)
  ACCEPT
    CASE ACCEPTED()
      OF ?BUTTONClose         ; BREAK
      OF ?BUTTONToClipboard   ; SETCLIPBOARD(TheStats)
      OF ?BUTTONToDebug       ; Self.Debug(CLIP(TheStats))      ! RA.2014.06.04 - CLIP the string
    END
  END
  DO ClearKeyStrokes

ClearKeyStrokes                             ROUTINE
    LOOP WHILE KEYBOARD() !Empty the keyboard buffer
      ASK                 !without processing keystrokes
    END
    SETKEYCODE(0)

!==============================================================================
ThreadDebugGlobal.FormatMessageList       PROCEDURE(STRING pMsg,*QUEUE pMsgQueue)
!==============================================================================
   !See UltimateString.Split('|')
StartPos                            LONG(1)
Pos                                 LONG,AUTO
	CODE
	IF pMsg
		LOOP WHILE StartPos
			Pos = INSTRING('|', pMsg, 1, StartPos)
			pMsgQueue = CHOOSE(Pos=0, pMsg[StartPos : LEN(pMsg)], pMsg[StartPos : Pos-1])
			ADD(pMsgQueue)
			ASSERT(ERRORCODE()=0)
			StartPos = Pos+1
		UNTIL Pos = 0
	END
	RETURN
!==============================================================================
ThreadDebugGlobal.Construct               PROCEDURE
!==============================================================================
   CODE
   !This is a dervied class, see parent constructor
   SELF.EventQ        &= NEW ST::DebugEventQueue
   SELF.IgnoreEventQ  &= NEW ST::DebugEventQueue
   SELF.CategoryQueue &= NEW DebugCategoryQueue

!    REGISTER(EVENT:AlertKey,ADDRESS(SELF.ShowProcedureInfo),ADDRESS(SELF))
   SELF.SetPurgeTime(5*TIME:Minute)
!==============================================================================
ThreadDebugGlobal.Destruct                PROCEDURE
!==============================================================================
	CODE
   IF ~(SELF.EventQ        &= NULL) THEN FREE(SELF.EventQ)       ; DISPOSE(SELF.EventQ)        END
   IF ~(SELF.IgnoreEventQ  &= NULL) THEN FREE(SELF.IgnoreEventQ) ; DISPOSE(SELF.IgnoreEventQ)  END
   IF ~(SELF.CategoryQueue &= NULL) THEN FREE(SELF.CategoryQueue); DISPOSE(SELF.CategoryQueue) END
!==============================================================================
ThreadDebugGlobal.SetDebugEvent           PROCEDURE(SIGNED Event)
!==============================================================================
	CODE
	SELF.DebugEvent = Event
!==============================================================================
ThreadDebugGlobal.SetHotKey               PROCEDURE(UNSIGNED HotKey)
!==============================================================================
	CODE
	0{PROP:Alrt, 255} = HotKey
	SELF.HotKey = HotKey
	SELF.SetDebugEvent(EVENT:AlertKey)
!==============================================================================
ThreadDebugGlobal.SetPurgeTime            PROCEDURE(LONG PurgeTime)
!==============================================================================
	CODE
	IF PurgeTime <= 0
		SELF.PurgeStarTime = 0
	ELSE
		SELF.PurgeStarTime = SELF.CalcStarDate(0, PurgeTime)
	END
!==============================================================================
ThreadDebugGlobal.IgnoreEvent             PROCEDURE(SIGNED Event)
!==============================================================================
X  LONG,AUTO
	CODE
  IF (SELF.DebugMe) THEN
    SELF.RawODS('DEBUGME: ThreadDebugGlobal.IgnoreEvent')
  END

	CLEAR(SELF.IgnoreEventQ)
	SELF.IgnoreEventQ.EventNo = Event
	ADD(SELF.IgnoreEventQ, SELF.IgnoreEventQ.EventNo)

	! Purge existing logged events
	LOOP X = RECORDS(SELF.EventQ) TO 1 BY -1
		GET(SELF.EventQ, X)
		IF SELF.EventQ.EventNo = Event
			DELETE(SELF.EventQ)
		END
	END
!==============================================================================
ThreadDebugGlobal.TakeEvent               PROCEDURE
!==============================================================================
	CODE
	SELF.LogEvent()

!                    !	? !ThreadDebugGlobal.Debug('ThreadDebugGlobal.TakeEvent: DebugEvent='& SELF.DebugEvent &'; Event='& EVENT() &'-'& SELF.GetEventName(EVENT()) &'; Keycode='& KEYCODE())
!                       CASE EVENT()
!                       OF   0
!                       OROF EVENT:Suspend
!                       OROF EVENT:Resume
!                    !		?   !Self.Debug('...Ignore It')
!                          !Ignore it
!
!                       OF SELF.DebugEvent
!                    !		?   !Self.Debug('...Debug Event  '& SELF.DebugEvent &'/'& EVENT:AlertKey &'  ' & KEYCODE() &'/'& SELF.HotKey)
!                          IF SELF.DebugEvent <> EVENT:AlertKey |
!                                OR KEYCODE() = SELF.HotKey
!                             SELF.Debug('')
!                          ELSE
!                             SELF.LogEvent()
!                          END
!
!                       ELSE
!                    !		?   !SELF.Debug('...Logger')
!                          IF SELF.DebugEvent <> EVENT:AlertKey     |
!                                OR EVENT()         <> EVENT:PreAlertKey  |
!                                OR KEYCODE()       <> SELF.HotKey
!                             SELF.LogEvent()
!                          END
!                       END
   RETURN
!==============================================================================
ThreadDebugGlobal.LogEvent                PROCEDURE
!==============================================================================
_Date LONG,AUTO
_Time LONG,AUTO
	CODE
  IF (SELF.DebugMe) THEN
    SELF.RawODS('DEBUGME: ThreadDebugGlobal.LogEvent')
  END

	SELF.IgnoreEventQ.EventNo = EVENT()
	GET(SELF.IgnoreEventQ, SELF.IgnoreEventQ.EventNo)
	IF ERRORCODE() <> 0
		CLEAR(SELF.EventQ)
		_Date = TODAY()
		_Time = CLOCK()
		SELF.EventQ.Date      = FORMAT(_Date, @D10)
		SELF.EventQ.Time      = FORMAT(_Time, @T6)
		SELF.EventQ.StarDate  = SELF.CalcStarDate(_Date,_Time)
		SELF.EventQ.EventNo   = EVENT()
		SELF.EventQ.EventName = SELF.GetEventName(EVENT())
		SELF.EventQ.FieldFeq  = FIELD()
		SELF.EventQ.FieldName = SELF.GetFEQDescr(FIELD())
		SELF.EventQ.Keycode   = KEYCODE()
		ADD(SELF.EventQ, SELF.EventQ.StarDate)
	END

	IF SELF.PurgeStarTime
	   SELF.PurgeUpTo(SELF.EventQ, SELF.CalcStarDate() - SELF.PurgeStarTime)
   END
!==============================================================================
ThreadDebugGlobal.PurgeUpTo               PROCEDURE(*ST::DebugEventQueue xaQ, REAL xPurgeUpTo)  !Assumes xaQ is sorted by StarDate
!==============================================================================
  CODE
  IF (SELF.DebugMe) THEN
    SELF.RawODS('DEBUGME: ThreadDebugGlobal.PurgeUpTo')
  END

  LOOP
    GET( xaQ, 1)
    IF ERRORCODE() OR xaQ.StarDate > xPurgeUpTo THEN BREAK END
    DELETE(xaQ)
  END
!==============================================================================
ThreadDebugGlobal.CalcStarDate            PROCEDURE(<LONG D>,<LONG T>)!,REAL
!==============================================================================
	CODE
	IF OMITTED(D) THEN D = TODAY() END
	IF OMITTED(T) THEN T = CLOCK() END
	RETURN D + (T-1)/TIME:DAY
!==============================================================================
ThreadDebugGlobal.GetEventName            PROCEDURE(SIGNED Event)!,STRING
!==============================================================================
	CODE
	RETURN SELF.GetEventDescr(Event)
!	CASE Event
!
!		! Field-dependent events
!
!	OF 01H;  RETURN 'Accepted'
!	OF 02H;  RETURN 'NewSelection'
!	OF 02H;  RETURN 'ScrollUp'
!	OF 04H;  RETURN 'ScrollDown'
!	OF 05H;  RETURN 'PageUp'
!	OF 06H;  RETURN 'PageDown'
!	OF 07H;  RETURN 'ScrollTop'
!	OF 08H;  RETURN 'ScrollBottom'
!	OF 09H;  RETURN 'Locate'
!
!	OF 01H;  RETURN 'MouseDown'
!	OF 0aH;  RETURN 'MouseUp'
!	OF 0bH;  RETURN 'MouseIn'
!	OF 0cH;  RETURN 'MouseOut'
!	OF 0dH;  RETURN 'MouseMove'
!	OF 0eH;  RETURN 'VBXevent'
!	OF 0fH;  RETURN 'AlertKey'
!	OF 10H;  RETURN 'PreAlertKey'
!	OF 11H;  RETURN 'Dragging'
!	OF 12H;  RETURN 'Drag'
!	OF 13H;  RETURN 'Drop'
!	OF 14H;  RETURN 'ScrollDrag'
!	OF 15H;  RETURN 'TabChanging'
!	OF 16H;  RETURN 'Expanding'
!	OF 17H;  RETURN 'Contracting'
!	OF 18H;  RETURN 'Expanded'
!	OF 19H;  RETURN 'Contracted'
!	OF 1AH;  RETURN 'Rejected'
!	OF 1BH;  RETURN 'DroppingDown'
!	OF 1CH;  RETURN 'DroppedDown'
!	OF 1DH;  RETURN 'ScrollTrack'
!	OF 1EH;  RETURN 'ColumnResize'
!
!	OF 101H;  RETURN 'Selected'
!	OF 102H;  RETURN 'Selecting'
!
!		! Field-independent events (FIELD() returns 0)
!
!	OF 201H;  RETURN 'CloseWindow'
!	OF 202H;  RETURN 'CloseDown'
!	OF 203H;  RETURN 'OpenWindow'
!	OF 204H;  RETURN 'OpenFailed'
!	OF 205H;  RETURN 'LoseFocus'
!	OF 206H;  RETURN 'GainFocus'
!
!	OF 208H;  RETURN 'Suspend'
!	OF 209H;  RETURN 'Resume'
!	OF 20AH;  RETURN 'Notify'
!
!	OF 20BH;  RETURN 'Timer'
!	OF 20CH;  RETURN 'DDErequest'
!	OF 20DH;  RETURN 'DDEadvise'
!	OF 20EH;  RETURN 'DDEdata'
!	OF 20FH;  RETURN 'DDEexecute'
!	OF 210H;  RETURN 'DDEpoke'
!	OF 211H;  RETURN 'DDEclosed'
!
!	OF 220H;  RETURN 'Move'
!	OF 221H;  RETURN 'Size'
!	OF 222H;  RETURN 'Restore'
!	OF 223H;  RETURN 'Maximize'
!	OF 224H;  RETURN 'Iconize'
!	OF 225H;  RETURN 'Completed'
!	OF 230H;  RETURN 'Moved'
!	OF 231H;  RETURN 'Sized'
!	OF 232H;  RETURN 'Restored'
!	OF 233H;  RETURN 'Maximized'
!	OF 234H;  RETURN 'Iconized'
!	OF 235H;  RETURN 'Docked'
!	OF 236H;  RETURN 'Undocked'
!
!	OF 240H;  RETURN 'BuildFile'
!	OF 241H;  RETURN 'BuildKey'
!	OF 242H;  RETURN 'BuildDone'
!
!		! User-definable events
!
!	OF 3FFH;  RETURN 'DoResize'
!	OF 400H;  RETURN 'User'
!	END
!	RETURN '???'
!==============================================================================
!-----------------------------------------------------------------------
! NOTES:
! Construct procedure executes automatically at the beginning of each procedure
! Destruct procedure executes automatically at the end of each procedure
! Construct/Destruct Procedures are implicit under the hood but don't have to be declared in the class as such if there is no need.
! It's ok to have them there for good measure, although some programmers only include them as needed.
! Normally some prefer Init() and Kill(),  but Destruct() can be handy to DISPOSE of stuff (to avoid mem leak)
!-----------------------------------------------------------------------
!EndRegion UltimateDebug



!!!!!!!!!!!!!



!Region UltimateDebugTraceSQL
!==============================================================================
!==============================================================================
!==============================================================================
UltimateDebugTraceSQL.Init                  PROCEDURE(File aFile)
Retval    BYTE
  CODE
  SELF.UD &= NEW UltimateDebug
  SELF.UD.DebugPrefix = '@'
  SELF.MyFile &= aFile
  SELF.MyFileName = NAME(SELF.MyFile)
  SQLCALLBACK(SELF.MyFile, SELF.SQLCallBackInterface)
  RETURN
!==============================================================================
UltimateDebugTraceSQL.Kill                  PROCEDURE
RetVal    BYTE
  CODE
  SQLCALLBACK(SELF.MyFile, SELF.SQLCallBackInterface, TRUE)
  RETURN
!==============================================================================
UltimateDebugTraceSQL.SQLCallBackInterface.ExecutingCode PROCEDURE(CONST *CSTRING inStr, *BYTE Err, *CSTRING FileErrCode, *CSTRING FileErrMsg)!, STRING
  CODE
  RETURN SELF.ExecutingCode(inStr, Err, FileErrCode,  FileErrMsg)!, STRING
!==============================================================================
UltimateDebugTraceSQL.ExecutingCode         PROCEDURE(CONST *CSTRING inStr, *BYTE Err, *CSTRING FileErrCode, *CSTRING FileErrMsg)!,STRING,VIRTUAL
  CODE
!!     Self.errcode = Errorcode()
!!     Self.errmsg  = choose(errorcode() = 90,FileError(),Error())
!!     Self.GetLastAction(opCode)
!!     Self.Opcode = opCode
  SELF.UD.Message('CALLBACKSQL(' & Err & '): ' & inStr)
  IF (Err) THEN
    SELF.UD.Message('ERROR(' & FileErrCode & '): ' & FileErrMsg & '')
  END
  RETURN(inStr)
!==============================================================================
!EndRegion UltimateDebugTraceSQL


!Region UltimateDebugTraceFile
!==============================================================================
!==============================================================================
!==============================================================================
UltimateDebugTraceFile.Init                 PROCEDURE(FILE aFile)
!==============================================================================
  CODE
  SELF.UD &= NEW UltimateDebug
  !SELF.UD.DebugPrefix = '@'
  self.myfile &= afile
  self.myfilename = NAME(SELF.MyFile)

  SELF.UD.LOW_RYB('UltimateDebugTraceFile.Init(' & self.myfilename & ')')

  CALLBACK(self.myfile, Self.FileCallBackInterface)
  ! RA.2014.11.07 - This call is returning an error (30-Entry Not Found) when function is done.
  ! So the callback is never activated and no driver trace entries ever show up.
  ! At this point a new set of eyes needs to look at this and it's cause.
  RETURN
!==============================================================================
UltimateDebugTraceFile.Kill                 PROCEDURE
!==============================================================================
  CODE
  SELF.UD.LOW_RYB('UltimateDebugTraceFile.Kill()')

  CALLBACK(self.myfile, Self.FileCallBackInterface, TRUE)
  DISPOSE(SELF.UD)
  RETURN
!==============================================================================
UltimateDebugTraceFile.FileCallBackInterface.FunctionCalled   PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING errmsg)
!==============================================================================
  CODE
  !SELF.UD.LOW_RYB('UltimateDebugTraceFile.FileCallBackInterface.FunctionCalled(' & opCode & ')')

  STOP('000: UltimateDebugTraceFile.FileCallBackInterface.FunctionCalled<13,10>' & |
       'opCode=' & opCode & '<13,10>ErrCode="' & errcode & '"<13,10>' & |
       'ErrMsg="' & errmsg & '"' |
      )

!  SELF.Direction = 'BEFORE:'                                      ! RA.2014.08.01 - Save direction
!  Self.Opcode = opCode
!  Self.GetLastAction(opCode)
!  SELF.UD.Debug('FUNCTION-CALLED: ' & CLIP(SELF.MyFileName) & ': ' & CLIP(SELF.Direction) & CLIP(SELF.FileAction) & |
!                '[' & SELF.OpCode & '] - ' & CLIP(SELF.ErrCode) & ' - ' & CLIP(SELF.ErrMsg) & ' (' & ERRORCODE() & '-' & ERROR() & ')')

  RETURN TRUE
!==============================================================================
UltimateDebugTraceFile.FileCallBackInterface.FunctionDone     PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING errcode, *CSTRING errmsg)
!==============================================================================
  CODE
  !SELF.UD.LOW_RYB('UltimateDebugTraceFile.FileCallBackInterface.FunctionDone(' & opCode & ')')

  STOP('001: UltimateDebugTraceFile.FileCallBackInterface.FunctionDone<13,10>' & |
       'opCode=' & opCode & '<13,10>ErrCode="' & errcode & '"<13,10>' & |
       'ErrMsg="' & errmsg & '"' |
      )

!  SELF.Direction = 'AFTER:'                                       ! RA.2014.08.01 - Save direction
!  Self.Opcode = opCode
!  Self.GetLastAction(opCode)
!  SELF.UD.Debug('FUNCTION-DONE: ' & CLIP(SELF.MyFileName) & ': ' & CLIP(SELF.Direction) & CLIP(SELF.FileAction) & |
!                '[' & SELF.OpCode & '] - ' & CLIP(SELF.ErrCode) & ' - ' & CLIP(SELF.ErrMsg) & ' (' & ERRORCODE() & '-' & ERROR() & ')')

  RETURN TRUE
!==============================================================================
! RA.2014.08.01 - Added the SELF.FileAction's literals that were not there (only the ADD existed) .
!                 Do not know what the SELF.Actionlevel means and what to set it to so I left that alone.
!                 Maybe Mike Hanson can explain what that is used for and the reason it's needed.
UltimateDebugTraceFile.GetLastAction        PROCEDURE(SIGNED opCode)
!==============================================================================
  CODE

  CASE opCode
  OF DriverOp:Add                       ; SELF.FileAction = 'ADD'                       ; SELF.ActionLevel = 3
  OF DriverOp:AddLen                    ; SELF.FileAction = 'ADDLEN'
  OF DriverOp:Append                    ; SELF.FileAction = 'APPEND'
  OF DriverOp:AppendLen                 ; SELF.FileAction = 'APPENDLEN'
  OF DriverOp:BOF                       ; SELF.FileAction = 'BOF'
  OF DriverOp:BUILDFile                 ; SELF.FileAction = 'BUILDFILE'
  OF DriverOp:BUILDdyn                  ; SELF.FileAction = 'BUILDDYN'
  OF DriverOp:BUILDdynfilter            ; SELF.FileAction = 'BUILDDYNFILTER'
  OF DriverOp:BUILDevent                ; SELF.FileAction = 'BUILDEVENT'
  OF DriverOp:BUILDkey                  ; SELF.FileAction = 'BUILDKEY'
  OF DriverOp:BUFFER                    ; SELF.FileAction = 'BUFFER'
  OF DriverOp:BYTES                     ; SELF.FileAction = 'BYTES'
  OF DriverOp:BLOBSIZE                  ; SELF.FileAction = 'BLOBSIZE'
  OF DriverOp:CALLBACK                  ; SELF.FileAction = 'CALLBACK'
  OF DriverOp:CLEARfile                 ; SELF.FileAction = 'CLEARFILE'
  OF DriverOp:CLOSE                     ; SELF.FileAction = 'CLOSE'
  OF DriverOp:COMMIT                    ; SELF.FileAction = 'COMMIT'
  OF DriverOp:COPY                      ; SELF.FileAction = 'COPY'
  OF DriverOp:CREATE                    ; SELF.FileAction = 'CREATE'
  OF DriverOp:Delete                    ; SELF.FileAction = 'DELETE'
  OF DriverOp:DESTROY                   ; SELF.FileAction = 'DESTROY'
  OF DriverOp:DOblobproperty            ; SELF.FileAction = 'DOBLOBPROPERTY'
  OF DriverOp:DOkeyproperty             ; SELF.FileAction = 'DOKEYPROPERTY'
  OF DriverOp:DOproperty                ; SELF.FileAction = 'DOPROPERTY'
  OF DriverOp:DUPLICATE                 ; SELF.FileAction = 'DUPLICATE'
  OF DriverOp:DUPLICATEkey              ; SELF.FileAction = 'DUPLICATEKEY'
  OF DriverOp:EMPTY                     ; SELF.FileAction = 'EMPTY'
  OF DriverOp:ENDTRAN                   ; SELF.FileAction = 'ENDTRAN'
  OF DriverOp:EOF                       ; SELF.FileAction = 'EOF'
  OF DriverOp:FIXFORMAT                 ; SELF.FileAction = 'FIXFORMAT'
  OF DriverOp:FLUSH                     ; SELF.FileAction = 'FLUSH'
  OF DriverOp:FREESTATE                 ; SELF.FileAction = 'FREESTATE'
  OF DriverOp:GETBLOBDATA               ; SELF.FileAction = 'GETBLOBDATA'
  OF DriverOp:GETBLOBPROPERTY           ; SELF.FileAction = 'GETBLOBPROPERTY'
  OF DriverOp:GetFileKey                ; SELF.FileAction = 'GETFILEKEY'
  OF DriverOp:GetFilePtr                ; SELF.FileAction = 'GETFILEPTR'
  OF DriverOp:GetFilePtrLen             ; SELF.FileAction = 'GETFILEPTRLEN'
  OF DriverOp:GetKeyProperty            ; SELF.FileAction = 'GETKEYPROPERTY'
  OF DriverOp:GetKeyPtr                 ; SELF.FileAction = 'GETKEYPTR'
  OF DriverOp:GetNULLS                  ; SELF.FileAction = 'GETNULLS'
  OF DriverOp:GetProperty               ; SELF.FileAction = 'GETPROPERTY'
  OF DriverOp:GetState                  ; SELF.FileAction = 'GETSTATE'
  OF DriverOp:HOLD                      ; SELF.FileAction = 'HOLD'
  OF DriverOp:LOCK                      ; SELF.FileAction = 'LOCK'
  OF DriverOp:LOGOUT                    ; SELF.FileAction = 'LOGOUT'
  OF DriverOp:NAME                      ; SELF.FileAction = 'NAME'
  OF DriverOp:NEXT                      ; SELF.FileAction = 'NEXT'
  OF DriverOp:NULL                      ; SELF.FileAction = 'NULL'
  OF DriverOp:OPEN                      ; SELF.FileAction = 'OPEN'
  OF DriverOp:PACK                      ; SELF.FileAction = 'PACK'
  OF DriverOp:POINTERfile               ; SELF.FileAction = 'POINTERFILE'
  OF DriverOp:POINTERkey                ; SELF.FileAction = 'POINTERKEY'
  OF DriverOp:POSITIONfile              ; SELF.FileAction = 'POSITIONfile'
  OF DriverOp:POSITIONKEY               ; SELF.FileAction = 'POSITIONKEY'
  OF DriverOp:PREVIOUS                  ; SELF.FileAction = 'PREVIOUS'
  OF DriverOp:Put                       ; SELF.FileAction = 'PUT'
  OF DriverOp:PutBLOBData               ; SELF.FileAction = 'PUTBLOBDATA'
  OF DriverOp:PutFilePtr                ; SELF.FileAction = 'PUTFILEPTR'
  OF DriverOp:PutFilePtrLen             ; SELF.FileAction = 'PUTFILEPTRLEN'
  OF DriverOp:PutBlobData               ; SELF.FileAction = 'PUTBLOBDATA'
  OF DriverOp:RECORDSfile               ; SELF.FileAction = 'RECORDSFILE'
  OF DriverOp:RECORDSkey                ; SELF.FileAction = 'RECORDSKEY'
  OF DriverOp:RELEASE                   ; SELF.FileAction = 'RELEASE'
  OF DriverOp:REMOVE                    ; SELF.FileAction = 'REMOVE'
  OF DriverOp:RENAME                    ; SELF.FileAction = 'RENAME'
  OF DriverOp:RegetFile                 ; SELF.FileAction = 'REGETFILE'
  OF DriverOp:RegetKey                  ; SELF.FileAction = 'REGETKEY'
  OF DriverOp:RESETFile                 ; SELF.FileAction = 'RESETFILE'
  OF DriverOp:RESETKey                  ; SELF.FileAction = 'RESETKEY'
  OF DriverOp:RESETViewFile             ; SELF.FileAction = 'RESETVIEWFILE'
? OF DriverOp:RestoreState              ; SELF.FileAction = 'RESTORESTATE'
  OF DriverOp:ROLLBACK                  ; SELF.FileAction = 'ROLLBACK'
  OF DriverOP:SEND                      ; SELF.FileAction = 'SEND'
  OF DriverOP:SetBlobProperty           ; SELF.FileAction = 'SETBLOBPROPERTY'
  OF DriverOP:Setfile                   ; SELF.FileAction = 'SETFILE'
  OF DriverOP:Setfilekey                ; SELF.FileAction = 'SETFILEKEY'
  OF DriverOP:Setfileptr                ; SELF.FileAction = 'SETFILEPTR'
  OF DriverOP:SetKey                    ; SELF.FileAction = 'SETKEY'
  OF DriverOP:SetKeyKey                 ; SELF.FileAction = 'SETKEYKEY'
  OF DriverOP:SetKeyKeyPtr              ; SELF.FileAction = 'SETKEYKEYPTR'
  OF DriverOP:SetKeyProperty            ; SELF.FileAction = 'SETKEYPROPERTY'
  OF DriverOP:SetKeyPtr                 ; SELF.FileAction = 'SETKEYPTR'
  OF DriverOP:SetNull                   ; SELF.FileAction = 'SETNULL'
  OF DriverOP:SetNullS                  ; SELF.FileAction = 'SETNULLS'
  OF DriverOP:SetNonNull                ; SELF.FileAction = 'SETNONNULL'
  OF DriverOP:SetProperty               ; SELF.FileAction = 'SETPROPERTY'
  OF DriverOP:SetViewFields             ; SELF.FileAction = 'SETVIEWFIELDS'
  OF DriverOP:SHARE                     ; SELF.FileAction = 'SHARE'
  OF DriverOP:SKIP                      ; SELF.FileAction = 'SKIP'
  OF DriverOP:STARTTRAN                 ; SELF.FileAction = 'STARTTRAN'
  OF DriverOP:STREAM                    ; SELF.FileAction = 'STREAM'
  OF DriverOP:UNLOCK                    ; SELF.FileAction = 'UNLOCK'
  OF DriverOP:UNFIXFORMAT               ; SELF.FileAction = 'UNFIXFORMAT'
  OF DriverOP:VIEWSTART                 ; SELF.FileAction = 'VIEWSTART'
  OF DriverOP:VIEWSTOP                  ; SELF.FileAction = 'VIEWSTOP'
  OF DriverOP:WATCH                     ; SELF.FileAction = 'WATCH'
  ELSE                                  ; SELF.FileAction = '*BAD DRIVER OP*'
  END
  !SELF.UD.LOW_RYB('UltimateDebugTraceFile.GetLastAction(' & opCode & ' = ' & SELF.FileAction & ')')
  RETURN
!==============================================================================
! RA.2014.08.01 - Reworked with the new fields and avoid duplication of the message.
! RA.2014.08.05 - Parameters are passed but nothing is done with them.
UltimateDebugTraceFile.DriverMessage        PROCEDURE(*Params Parameters)
!==============================================================================
  CODE
  SELF.UD.Debug('' & CLIP(SELF.MyFileName) & ': ' & CLIP(SELF.Direction) & CLIP(SELF.FileAction) & ' [' & SELF.OpCode & ']' & |
                ' - ' & CLIP(SELF.ErrCode) & ' - ' & CLIP(SELF.ErrMsg) & ' (' & ERRORCODE() & '-' & ERROR() & ')')
  RETURN
!==============================================================================
!EndRegion UltimateDebugTraceFile




!Region UltimateDebugTiming
!==============================================================================
StarDateClass.SetSimulatedTime              PROCEDURE(LONG TodayValue,LONG ClockValue)
!==============================================================================
  CODE
  SELF.SimulatedToday = TodayValue
  SELF.SimulatedClock = ClockValue

!==============================================================================
StarDateClass.GetClock                      PROCEDURE!,LONG
!==============================================================================
  CODE
  RETURN CHOOSE(SELF.SimulatedClock=0, CLOCK(), SELF.SimulatedClock)

!==============================================================================
StarDateClass.GetToday                      PROCEDURE!,LONG
!==============================================================================
  CODE
  RETURN CHOOSE(SELF.SimulatedToday=0, TODAY(), SELF.SimulatedToday)

!==============================================================================
!!! <summary>
!!! Return the Date+Time parameters as a "stardate".
!!! </summary>
StarDateClass.CalcStardate                  PROCEDURE(LONG D,LONG T)!,REAL
!==============================================================================
  CODE
  RETURN D + (T - TIME:Midnight) / TIME:Day

!==============================================================================
!!! <summary>
!!! Return the difference in stardate values between the stardate parameter and the current Date+Time.
!!! </summary>
StarDateClass.CalcStardateDelta             PROCEDURE(REAL STAR1)!,REAL                         ! RA.2015.01.06 - Startdate difference from now
!==============================================================================
vDate1                          LONG,AUTO
vTime1                          LONG,AUTO
  CODE
  SELF.SplitStardate(STAR1,vDate1,vTime1)
  RETURN SELF.CalcStardateDelta(vDate1,vTime1)

!==============================================================================
!!! <summary>
!!! Return the difference in stardate values between the Date+Time parameters and the current Date+Time.
!!! </summary>
StarDateClass.CalcStardateDelta             PROCEDURE(LONG EarlierD,LONG EarlierT)!,REAL
!==============================================================================
  CODE
  RETURN SELF.CalcStardateDelta(EarlierD, EarlierT, SELF.GetToday(), SELF.GetClock())

!==============================================================================
!!! <summary>
!!! Return the difference in stardate values between the Date+Time parameter pairs.
!!! The return is the number of days (on the date side)
!!! </summary>
StarDateClass.CalcStardateDelta             PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2)!,REAL
!==============================================================================
StarDate1                       REAL,AUTO
StarDate2                       REAL,AUTO
  CODE
  StarDate1 = SELF.CalcStardate(D1, T1)
  StarDate2 = SELF.CalcStardate(D2, T2)
  RETURN StarDate2 - StarDate1

!==============================================================================
!!! <summary>
!!! Return the current Date+Time as a "stardate".
!!! </summary>
StarDateClass.GetStardate                   PROCEDURE!,REAL
!==============================================================================
  CODE
  RETURN SELF.CalcStardate(SELF.GetToday(), SELF.GetClock())

!==============================================================================
!!! <summary>
!!! Split "stardate" into Date+Time
!!! </summary>
StarDateClass.SplitStardate                 PROCEDURE(REAL StarDate_IN,*? D_OUT,*? T_OUT)
!==============================================================================
  CODE
  D_OUT = INT(StarDate_IN)
  T_OUT = (StarDate_IN - D_OUT) * TIME:Day + 1

!==============================================================================
! RA.2015.01.03 - Add stardate together and return a startdate
!!! <summary>
!!! Add stardate together and return a startdate
!!! </summary>
StarDateClass.AddDaysTime                   PROCEDURE(REAL STAR1,REAL STAR2)!,REAL
!==============================================================================
vToday1                         LONG,AUTO
vClock1                         LONG,AUTO
vToday2                         LONG,AUTO
vClock2                         LONG,AUTO
  CODE
  SELF.SplitStardate(STAR1,vToday1,vClock1)
  SELF.SplitStardate(STAR2,vToday2,vClock2)
  RETURN SELF.AddDaysTime(vToday1,vClock1,vToday2,vClock2)

!==============================================================================
! RA.2015.01.03 - Add days+time together and return a startdate
!!! <summary>
!!! Add days+time together and return a startdate
!!! </summary>
StarDateClass.AddDaysTime                   PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2)!,REAL
!==============================================================================
vToday3                         LONG,AUTO
vClock3                         LONG,AUTO
  CODE
  vToday3    = D2 + D1
  vClock3    = T2 + T1
  LOOP WHILE(vClock3 >= TIME:Day)
    vToday3 += 1
    vClock3 -= TIME:Day
  END
  RETURN SELF.CalcStardate(vToday3,vClock3)

!==============================================================================
! RA.2015.01.03 - StarDate days:hours:minutes:seconds output format
!!! <summary>
!!! Format stardate into ddd:hh:mm:ss
!!! </summary>
StarDateClass.FormatOneDayTime              PROCEDURE(REAL STAR1)!,STRING
!==============================================================================
vToday1                         LONG,AUTO
vClock1                         LONG,AUTO
  CODE
  SELF.SplitStardate(STAR1,vToday1,vClock1)
  RETURN SELF.FormatOneDayTime(vToday1,vClock1)

!==============================================================================
! RA.2015.01.02 - StarDate days:hours:minutes:seconds output format
!!! <summary>
!!! Format date+time into ddd:hh:mm:ss
!!! </summary>
StarDateClass.FormatOneDayTime              PROCEDURE(LONG D1,LONG T1)!,STRING
!==============================================================================
  CODE
  RETURN FORMAT(D1, @N03) & ':' & FORMAT(T1, @T04)

!==============================================================================
! RA.2015.01.03 - StarDate days:hours:minutes:seconds output format
!!! <summary>
!!! Format stardate into ddd:hh:mm:ss
!!! </summary>
StarDateClass.FormatDaysTime                PROCEDURE(REAL STAR1,REAL STAR2)!,STRING
!==============================================================================
vToday1                         LONG,AUTO
vClock1                         LONG,AUTO
vToday2                         LONG,AUTO
vClock2                         LONG,AUTO
  CODE
  SELF.SplitStardate(STAR1,vToday1,vClock1)
  SELF.SplitStardate(STAR2,vToday2,vClock2)
  RETURN SELF.FormatDaysTime(vToday1,vClock1,vToday2,vClock2)

!==============================================================================
! RA.2015.01.02 - StarDate days:hours:minutes:seconds output format
!!! <summary>
!!! Format date+time into ddd:hh:mm:ss
!!! </summary>
StarDateClass.FormatDaysTime                PROCEDURE(LONG D1,LONG T1,LONG D2,LONG T2)!,STRING
!==============================================================================
vReal3        REAL,AUTO
vToday3                         LONG,AUTO
vClock3                         LONG,AUTO
  CODE
  vReal3   = SELF.CalcStardateDelta(D1,T1,D2,T2)
  SELF.SplitStardate(vReal3,vToday3,vClock3)
  vClock3 += 1
  RETURN FORMAT(vToday3, @N03) & ':' & FORMAT(vClock3, @T04)

!==============================================================================


!==============================================================================
TimingClass.CONSTRUCT                       PROCEDURE()
!==============================================================================
  CODE
  SELF.UD          &= NEW UltimateDebug
  SELF.oStarDate   &= NEW StarDateClass
  SELF.oLogFile    &= NEW ctAsciiLogger
  SELF.TimingQ     &= NEW TimingQueue

  SELF.BaseQ       &= SELF.TimingQ        ! RA.2019.03.02: Per MG BUG!!!!
  !SELF.UD.Debug('TimingClass.CONSTRUCT')
  RETURN

!==============================================================================
! RA.2015.01.18 - Making SURE that the DISPOSEs are working correctly.
TimingClass.DESTRUCT                        PROCEDURE()
!==============================================================================
  CODE
  !SELF.UD.Debug('TimingClass.DESTRUCT')
  !SELF.UD.Debug('TimingClass.DESTRUCT.DISPOSE(SELF.TimingQ)')
  !IF NOT    SELF.TimingQ &= NULL
    !FREE   (SELF.TimingQ)              ! 2019.03.02: Per MG: Not needed done by ctQueue.Destruct
    !DISPOSE(SELF.TimingQ)              ! 2019.03.02: Per MG: Not needed done by ctQueue.Destruct
  !END
  !SELF.UD.Debug('TimingClass.DESTRUCT.DISPOSE(SELF.oLogFile)')
  !IF NOT    SELF.oLogFile &= NULL
    DISPOSE(SELF.oLogFile)
  !END
  !SELF.UD.Debug('TimingClass.DESTRUCT.DISPOSE(SELF.oStarDate)')
  !IF NOT    SELF.oStarDate &= NULL
    DISPOSE(SELF.oStarDate)
  !END
  !SELF.UD.Debug('TimingClass.DESTRUCT.DISPOSE(SELF.UD)')
  !IF NOT    SELF.UD &= NULL
    DISPOSE(SELF.UD)
  !END
  RETURN

!==============================================================================
TimingClass.Init                            PROCEDURE(STRING pReportName)
!==============================================================================
  CODE
  SELF.ReportName                 = pReportName
  RETURN

!==============================================================================
TimingClass.Kill                            PROCEDURE()
!==============================================================================
  CODE
  SELF.Done(SELF.ReportName)
  RETURN

!==============================================================================
TimingClass.Description                     PROCEDURE()!,STRING,DERIVED
!==============================================================================
  CODE
  RETURN 'Procedure Timing Class'

!==============================================================================
TimingClass.Find                            PROCEDURE(STRING pAppProcName, REAL pStarDate)
!==============================================================================
  CODE
  !SELF.UD.Debug('--> Find(' & pAppProcName & ',' & pStarDate & ')')
  IF (SELF._Find(pAppProcName)   = TRUE) THEN
    IF (SELF.TimingQ.ExecuteCount < TIMING:COUNT:MAXIMUM) THEN
      SELF.TimingQ.ExecuteCount += 1                                        ! Times this procedure was executed (MAXIMUM is 4,294,967,295)
    END
    SELF.TimingQ.ExecuteTime     = SELF.oStarDate.AddDaysTime(SELF.TimingQ.ExecuteTime, pStarDate)
    SELF.PutRow()
    !SELF.UD.Debug('ADD Item=' & CLIP(pAppProcName) & ' Time=' & SELF.oStarDate.FormatOneDayTime(pStarDate))
    !SELF.UD.Debug('TOTAL=' & CLIP(pAppProcName) & ' Time=' & SELF.oStarDate.FormatOneDayTime(SELF.TimingQ.ExecuteTime))
  ELSE
    CLEAR(SELF.TimingQ)
    SELF.TimingQ.AverageTime     = 0.0                                      ! StarDate [days:hours:minutes:secods]
    SELF.TimingQ.AppProcName     = pAppProcName                             ! Application-Procedure Name
    SELF.TimingQ.ExecuteCount    = 1                                        ! Times this procedure was executed (MAXIMUM is 4,294,967,295)
    SELF.TimingQ.ExecuteTime     = pStarDate                                ! StarDate [days:hours:minutes:secods]
    SELF.AddRow()
    !SELF.UD.Debug('NEW Item=' & CLIP(pAppProcName) & ' Time=' & SELF.oStarDate.FormatOneDayTime(pStarDate))
  END
  RETURN

!==============================================================================
TimingClass._Find                           PROCEDURE(STRING pAppProcName)!,BOOL
!==============================================================================
Index                             LONG(0)
Maximum                           LONG(0)
Found                             BOOL(FALSE)
  CODE
  Found        = FALSE
  Maximum      = SELF.Count()
  !SELF.UD.Debug('Looking for=' & CLIP(pAppProcName) & ', Maximum=' & Maximum)
  IF (Maximum > 0) THEN
    LOOP Index = 1 TO Maximum
      SELF.GetRow(Index)
      !SELF.UD.Debug('Index=' & Index & ', Name=' & SELF.TimingQ.AppProcName)
      IF (SELF.TimingQ.AppProcName = pAppProcName) THEN
        Found  = TRUE
        BREAK
      END
    END
  END
  !SELF.UD.Debug('Found=' & Found)
  RETURN Found

!==============================================================================
TimingClass.Done                            PROCEDURE(STRING pLogFileName)
!==============================================================================
Index                             LONG,AUTO
Maximum                           LONG,AUTO
TotalTime                         REAL,AUTO
LogLine                           STRING(4096),AUTO                       !HACK: arbitrary limit of LogFile
FileName                          STRING(255),AUTO
 CODE
  ! Calculate average time and Total Time
  Maximum                         = SELF.Count()
  TotalTime                       = 0.0
  LOOP Index = 1 TO Maximum
    SELF.GetRow(Index)
    IF (SELF.TimingQ.ExecuteCount < TIMING:COUNT:MAXIMUM) THEN
      SELF.TimingQ.AverageTime    = SELF.TimingQ.ExecuteTime / SELF.TimingQ.ExecuteCount
    ELSE
      SELF.TimingQ.AverageTime    = 0.0
    END
    TotalTime                    += SELF.TimingQ.ExecuteTime
    SELF.PutRow
  END

  CLEAR(SELF.TimingQ)
  SELF.TimingQ.AverageTime     = 0.0                                      ! StarDate [days:hours:minutes:secods]
  SELF.TimingQ.AppProcName     = 'TOTAL'                                  ! Application-Procedure Name
  SELF.TimingQ.ExecuteCount    = 0                                        ! Times this procedure was executed (MAXIMUM is 4,294,967,295)
  SELF.TimingQ.ExecuteTime     = TotalTime                                ! StarDate [days:hours:minutes:secods]
  SELF.AddRow()

  SORT(SELF.TimingQ, -SELF.TimingQ.AverageTime, -SELF.TimingQ.ExecuteCount)

  !SELF.UD.DebugQueue(SELF.TimingQ,'Show timing queue')

  ! RA.2015.01.14 - Create log file output
  FileName                     = CLIP(pLogFileName) & '_' & FORMAT(TODAY(),@D010-) & '_' & FORMAT(CLOCK(),@T04-) & '.CSV'
  SELF.oLogFile.Start(FileName)
  SELF.oLogFile.ClearLog()

  SELF.oLogFile.Add('"Average Time","Application-Procedure","Execution Count","Execution Time"')

  Maximum                         = SELF.Count()
  LOOP Index = 1 TO Maximum
    SELF.GetRow(Index)
    !SELF.UD.Debug('Index=' & FORMAT(Index, @N03) & ', Name=' & CLIP(SELF.TimingQ.AppProcName) & '')

    LogLine                       = ''
    IF (SELF.TimingQ.AverageTime    <> 0.0) THEN
      LogLine                     = CLIP(LogLine) & '"' & SELF.oStarDate.FormatOneDayTime(SELF.TimingQ.AverageTime) & '",'
    ELSE
      LogLine                     = CLIP(LogLine) & '"",'
    END
    LogLine                       = CLIP(LogLine) & '"' & CLIP(SELF.TimingQ.AppProcName) & '",'
    IF (SELF.TimingQ.ExecuteCount <> 0) THEN
      LogLine                     = CLIP(LogLine) & '"' & SELF.TimingQ.ExecuteCount & '",'
    ELSE
      LogLine                     = CLIP(LogLine) & '"",'
    END
    LogLine                       = CLIP(LogLine) &'"' & SELF.oStarDate.FormatOneDayTime(SELF.TimingQ.ExecuteTime) & '"'

    !SELF.UD.Debug('LogLine=' & CLIP(LogLine) & '')
    SELF.oLogFile.Add(CLIP(LogLine))
  END

  SELF.oLogFile.Finish(FileName)
  RETURN

!==============================================================================
TimingClass.Count                           PROCEDURE()!,LONG    !Alias for .Records
!==============================================================================
   CODE
   RETURN SELF._Records()

!==============================================================================
TimingClass._Records                        PROCEDURE()!,LONG
!==============================================================================
   CODE
   RETURN RECORDS(SELF.TimingQ)

!==============================================================================
TimingClass.GetRow                          PROCEDURE(LONG xPointer)!,LONG,PROC
!==============================================================================
   CODE
   GET(SELF.TimingQ, xPointer)
   RETURN ErrorCode()

!==============================================================================
TimingClass.AddRow                          PROCEDURE()!,LONG,PROC
!==============================================================================
   CODE
   ADD(SELF.TimingQ)
   RETURN ErrorCode()

!==============================================================================
TimingClass.PutRow                          PROCEDURE()!,LONG,PROC
!==============================================================================
   CODE
   PUT(SELF.TimingQ)
   RETURN ErrorCode()

!==============================================================================
!EndRegion UltimateDebugTiming


!==============================================================================
!==============================================================================
!Region Local Procedures
!------------------------------------------------------------------------------
!EndRegion Local Procedures
!------------------------------------------------------------------------------
!ThreadDebugLocal                        CLASS(),TYPE,MODULE('UDRA_UltimateDebug.CLW'),LINK('UDRA_UltimateDebug.CLW')  !,_ABCLinkMode_),DLL(_ABCDllMode_)
!Construct                                   PROCEDURE()
!Destruct                                    PROCEDURE()
!                                        END
ThreadDebugLocal.Construct                  PROCEDURE
  CODE
  IF (SELF.DebugMe)
    PARENT.Debug(':DEBUGME ThreadDebugLocal.Construct')
  END
  RETURN
!------------------------------------------------------------------------------
ThreadDebugLocal.Destruct                   PROCEDURE
  CODE
  IF (SELF.DebugMe)
    PARENT.Debug(':DEBUGME ThreadDebugLocal.Destruct')
  END
  RETURN
!------------------------------------------------------------------------------

