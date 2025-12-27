#TEMPLATE(zUltimateDebug,'RA.2018.02.28: UltimateDebug Template (v2.01)'),FAMILY('ABC'),FAMILY('Clarion'),FAMILY('cw20')
#!-----------------------------------------------------------------------------------------------------
#SYSTEM
#EQUATE(%CLSkelTPLVersion,'v2.01, Released 2018-02-28')
#!
#! SystemIdle (Global Extension)
#!
#EXTENSION(UltimateDebugGlobal, 'UDRA_UltimateDebug (Global Extension)'),DESCRIPTION('UDRA_UltimateDebug (Global Extension) ' & %CLSkelTPLVersion),APPLICATION
#PREPARE
  #INSERT(%CalcFamily, %CLSkelFamily)
#ENDPREPARE
#!
#!#BOXED('Information')
#!#INSERT(%CopyrightInfo)
#!#ENDBOXED
#!
#DISPLAY
#PROMPT('Disable Ultimate Debug template',CHECK),%CLSkelAppDisable,AT(10),DEFAULT(0)
#DISPLAY
#!-------------------------------------------------------------------------
#! RA.2014.03.28 - Added Debuger code generation options prompts.
#! Ties in with the additional templates for mass debugging procedures.
#!-------------------------------------------------------------------------
#BUTTON('Ultimate Debug &Generation Options'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400),AT(,,180,20)
#DISPLAY('UltimateDebug'),AT(10,0),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('Version ' & %CLSkelTPLVersion),AT(10,10),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#SHEET,AT(,,288),HSCROLL
#TAB('General')
  #BOXED(' Ultimate Debug Generation Options '),AT(,,280)
    #ENABLE(~%CLSkelAppDisable)
      #BOXED(' Application Generation Options ')
        #DISPLAY('')
        #PROMPT('Create global information procedure? ',CHECK),%gDumpTpl,DEFAULT(0),AT(10)
        #ENABLE(%gDumpTpl)
          #PROMPT('Dump global information? '          ,CHECK),%zDumpTpl,DEFAULT(0),AT(25)
        #ENDENABLE
        #DISPLAY('')
        #PROMPT('Create global variables procedure? '  ,CHECK),%gDumpVar,DEFAULT(0),AT(10)
        #ENABLE(%gDumpVar)
          #PROMPT('Dump global variables? '            ,CHECK),%zDumpVar,DEFAULT(0),AT(25)
        #ENDENABLE
        #DISPLAY('')
      #ENDBOXED
      #!DISPLAY('')
      #!DISPLAY('')
      #BOXED(' John Hickey''s PROCEDURE template ')
        #DISPLAY('')
        #PROMPT('DISABLE John Hickey''s PROCEDURE template? '  ,CHECK),%DisableProcedureJH,DEFAULT(1),AT(10)
        #DISPLAY('')
      #ENDBOXED
    #ENDENABLE
  #ENDBOXED
  #!#DISPLAY('Copyright © 1999-2014 by Roberto Artigas')
#ENDTAB
#INSERT(%TabPurpose1)
#INSERT(%TabInstructions1)
#INSERT(%TabLimitations1)
#INSERT(%TabTesting1)
#ENDSHEET
#ENDBUTTON
#!-------------------------------------------------------------------------
#DISPLAY
#SHEET,AT(,,288),HSCROLL
#TAB('General')
  #DISPLAY
  #PROMPT('Global Class:',@S40),%CLSkelClass,AT(90,,95,10),REQ,DEFAULT('UD')
  #PROMPT('Multi DLL',CHECK),%CLSkelMultiDLL,AT(90),DEFAULT(0)
  #ENABLE(%CLSkelMultiDLL=1),ClEAR
    #PROMPT('Declaration:',DROP('Declared in another App[0]|Declared in this app[1]')),%CLSkelMultiDLLData,DEFAULT(0),AT(90,,95,10)
  #ENDENABLE
  #PROMPT('Prefix:',@S20),%CLDebugPrefix,DEFAULT('!')
  #PROMPT('Log File Name:',@S120),%CLLogFileName,DEFAULT('DebugLog.txt')
  #INSERT(%TabCopyright)
#ENDTAB
#INSERT(%TabPurpose)
#INSERT(%TabInstructions)
#INSERT(%TabContributors)
#INSERT(%TabClarionVer)
#INSERT(%TabTesting)
#ENDSHEET
#!
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#ATSTART
  #DECLARE(%CLSkelDataExternal)
  #IF(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=0)
    #!SET(%CLSkelDataExternal,',EXTERNAL,DLL')
    #SET(%CLSkelDataExternal,',EXTERNAL,DLL(dll_mode)')
  #ENDIF
#ENDAT

#AT(%AfterGlobalIncludes),WHERE(~%CLSkelAppDisable)
   INCLUDE('UDRA_UltimateDebug.INC'),ONCE #<! Ultimate Debug Classes
#ENDAT

#AT(%CustomGlobalDeclarations),WHERE(~%CLSkelAppDisable)
  #INSERT(%CalcFamily, %CLSkelFamily)
  #IF(%CLSkelFamily='LEGACY')
  #PROJECT('UDRA_UltimateDebug.CLW')
  #ENDIF
#ENDAT

#! RA.2014.08.05 - Some cosmetics changes to allign everything
#AT(%GlobalData),WHERE(~%CLSkelAppDisable)
UD_TplVersion %[26]NULL CSTRING('v%CLSkelTPLVersion')%CLSkelDataExternal
%[40]CLSkelClass CLASS(UltimateDebug)%CLSkelDataExternal
%[40]NULL END
#ENDAT

#! RA.2014.08.05 - Some cosmetics changes to allign everything
#AT(%DLLExportList),WHERE(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=1 AND ~%CLSkelAppDisable)
  $UD_TplVersion %[26]NULL @?
  $%[40]CLSkelClass @?
#ENDAT

#!-----------------------------------------------------!
#! RA.2014.03.28 - Change the location to the earliest !
#!-----------------------------------------------------!
#AT(%ProgramSetup),WHERE(~%CLSkelAppDisable),PRIORITY(0001)
%CLSkelClass.Init()                           #<! Ultimate Debug INIT class
%CLSkelClass.DebugPrefix = '%CLDebugPrefix'
%CLSkelClass.ASCIIFileName = '%CLLogFileName'
#!%CLSkelClass.Debug('INIT: Ultimate Debug Class INIT done: %Application')
#!
#COMMENT(90)
  #IF(%gDumpTpl AND %zDumpTpl)                                                #! Dump GLOBAL information - end
DebugABCGlobalInformation_%Application()      #<! Dump GLOBAL information
  #END                                                                        #! Dump GLOBAL information - begin
  #IF(%gDumpVar AND %zDumpVar)                                                #! Dump GLOBAL data - begin
DebugABCGlobalVariables_%Application()        #<! Dump GLOBAL variables
  #ENDIF                                                                      #! Dump GLOBAL data - end
#COMMENT(60)

#ENDAT

#!---------------------------------------------------!
#! RA.2014.03.28 - Change the location to the latest !
#!---------------------------------------------------!
#AT(%ProgramEnd),WHERE(~%CLSkelAppDisable),PRIORITY(9999)
#!%CLSkelClass.Debug('KILL: Ultimate Debug Class KILL done: %Application')
%CLSkelClass.Kill()                           #<! Ultimate Debug KILL class
#ENDAT

#!-------------------------------------------------------------------------
#! RA.2010.12.10 - Additional procedures to dump basic information.
#! Nice to know where everything is and what global templates are being used.
#!-------------------------------------------------------------------------
#AT (%GlobalMap),PRIORITY(9000)
#!  #IF(~%CLSkelAppDisable)
    #INDENT(-5)

#COMMENT(90)
DebugABCGlobalInformation_%Application PROCEDURE() #<! DEBUG Prototype
DebugABCGlobalVariables_%Application PROCEDURE() #<! DEBUG Prototype
#COMMENT(60)

    #INDENT(+5)
#!  #ENDIF
#ENDAT
#!-------------------------------------------------------------------------
#AT(%ProgramProcedures),PRIORITY(9000)

!BOE: DEBUG Global
DebugABCGlobalInformation_%Application PROCEDURE()
  CODE
 #IF(~%CLSkelAppDisable)
  #IF(%gDumpTpl)
  %CLSkelClass.Debug('----------------> APPLICATION INFORMATION')
  %CLSkelClass.Debug('Information Generated on: '& FORMAT(TODAY(),@D010) & ' - ' & FORMAT(CLOCK(),@T04))
    #IF(%ProgramExtension = 'EXE')
  %CLSkelClass.Debug('CW Version: Lib ' & system{prop:libversion} & ' Exe ' & system{prop:exeversion} & '')
    #ENDIF
  %CLSkelClass.Debug('Application Name: %Application ')
    #IF (%ApplicationDebug = %True)
  %CLSkelClass.Debug('Compiled in DEBUG mode.')
    #ENDIF
    #IF (%ApplicationLocalLibrary = %TRUE)
  %CLSkelClass.Debug('Compiled with LOCAL option.')
    #ENDIF
    #IF (%Target32 = %True)
  %CLSkelClass.Debug('Application is 32 bits.')
    #ELSE
  %CLSkelClass.Debug('Application is 16 bits.')
    #ENDIF
  %CLSkelClass.Debug('First procedure: %FirstProcedure')
  %CLSkelClass.Debug('Program Extension: %ProgramExtension')
  %CLSkelClass.Debug('Dictionary Name: %DictionaryFile')
  %CLSkelClass.Debug('Installation Path: ' & LONGPATH(PATH()))
    #IF(ITEMS(%ApplicationTemplate))
  %CLSkelClass.Debug('----------------> GLOBAL TEMPLATES')
      #FOR(%ApplicationTemplate)
  %CLSkelClass.Debug('Global Templates: %ApplicationTemplate ')
      #ENDFOR
    #ENDIF
  %CLSkelClass.Debug('----------------> ')
  #ENDIF
 #ENDIF
  RETURN

DebugABCGlobalVariables_%Application PROCEDURE()
  CODE
 #IF(~%CLSkelAppDisable)
  #IF(%gDumpVar)
    #DECLARE (%Prefix)
    #DECLARE (%VarName)
    #DECLARE (%PrefixStart)
    #DECLARE (%PrefixEnd)
    #DECLARE (%DataStmt)
  %CLSkelClass.Debug('----------------> GLOBAL VARIABLES')
    #FOR(%GlobalData)
      #SET(%DataStmt,QUOTE(%GlobalDataStatement))
      #IF (INSTRING('QUEUE',%GlobalDataStatement,1,1) OR INSTRING('GROUP',%GlobalDataStatement,1,1))
        #SET(%PrefixStart,INSTRING('PRE(',%GlobalDataStatement,1,1)+4)
        #SET(%PrefixEnd  ,INSTRING(')',%GlobalDataStatement,1,%PrefixStart))
        #IF (%PrefixStart)
          #SET(%Prefix,SUB(%GlobalDataStatement, %PrefixStart, %PrefixEnd-%PrefixStart) & ':')
          #IF (LEN(%Prefix) = 1)
            #SET(%Prefix,'')
          #ENDIF
        #ENDIF
  %CLSkelClass.Debug('Only the active record of a group or queue is displayed.')
#!  %CLSkelClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')  ! & ' Records: ' & RECORDS(%GlobalData))
  %CLSkelClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')
      #ELSE
        #IF (INSTRING('END',%GlobalDataStatement,1,1) OR INSTRING('FILE',%GlobalDataStatement,1,1))
          #SET (%Prefix,'')
  %CLSkelClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt')
        #ELSE
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported.
	  #IF(INSTRING(',DIM',UPPER(%DataStmt))>0)
  %CLSkelClass.Debug('Global Data: %[23]GlobalData is an ARRAY variable and NOT SUPPORTED.')
	  #ELSIF(INSTRING('&',UPPER(%DataStmt))>0) #! RA.2014.04.27 - Reference variable
  %CLSkelClass.Debug('Global Data: %[23]GlobalData is a REFERENCE variable and NOT SUPPORTED.')
          #ELSE
  %CLSkelClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt Value: ''' & CLIP(%Prefix%GlobalData) & '''')
          #ENDIF
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported.
        #ENDIF
      #ENDIF
    #ENDFOR
  %CLSkelClass.Debug('---------------->')
  #ENDIF
 #ENDIF
  RETURN
!EOE: DEBUG Global

#ENDAT
#!-------------------------------------------------------------------------
#INCLUDE('UDRA_UltimateDebug.TPW')
#INCLUDE('UDRA_UltimateDebug2.TPW') #! RA.2014.03.28 - Trace Code ALL procedures
#INCLUDE('UDRA_UltimateDebug3.TPW') #! RA.2014.04.12 - Tracing Source (*.CLW) files
#INCLUDE('UDRA_UltimateDebug4.TPW') #! RA.2014.06.04 - John Hickey's procedure tracker
#INCLUDE('UDRA_UltimateDebug5.TPW') #! RA.2015.01.02 - Timming Code ALL procedures
#!INCLUDE('UDRA_UltimateDebug6.TPW') #! RA.2015.01.02 - Callback and SQLCallback - Experimental - NOT WORKING (NOT NEEDED - RA.2018.02.28)
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
