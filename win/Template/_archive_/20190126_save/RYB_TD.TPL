#TEMPLATE(zThreadDebug,'RA.2019.01.01: Thread Debug Template Collection'),FAMILY('ABC')
#!-----------------------------------------------------------------------------------------------------
#SYSTEM
#EQUATE(%TDtplVersion,'0.01')
#EQUATE(%TDtplRelease,'2019.01.01')
#!-----------------------------------------------------------------------------------------------------
#EQUATE(%TDtplVerNumber   ,'Version ' & %TDtplVersion & ' [' & %TDtplRelease & ']'                    )
#EQUATE(%TDtplVerRelDate  ,'Thread Debug (TD) template collection.'                                   )
#EQUATE(%TDtplVerCpyRghts ,'Templates copyright © by each individual author.'                         )
#EQUATE(%TDtplVerRsrvAll  ,'Collection created by Roberto Artigas y Soler.'                           )
#EQUATE(%TDtplVerEmail    ,'E-MAIL: roberto.artigas.dev@gmail.com'                                    )
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#GROUP(%FamilyFind, * %FamilyFound)
#IF(VarExists(%AppTemplateFamily))
  #IF(%AppTemplateFamily='CLARION')
    #SET(%FamilyFound,'LEGACY')
  #ELSE
    #SET(%FamilyFound,'ABC')
  #ENDIF
#ELSIF(VarExists(%cwtemplateversion))
  #IF(%cwtemplateversion = 'v5.5')
    #IF(VarExists(%ABCVersion))
      #SET(%FamilyFound,'ABC')
    #ELSE
      #SET(%FamilyFound,'LEGACY')
    #ENDIF
  #ELSE
    #IF(%cwtemplateversion = 'v2.003')
      #SET(%FamilyFound,'LEGACY')
    #ELSE
      #SET(%FamilyFound,'ABC')
    #ENDIF
  #ENDIF
#ENDIF
#!-----------------------------------------------------------------------------------------------------
#GROUP(%ClarionFamily)
#PREPARE
  #IF(NOT VAREXISTS(%FamilyFound))
    #DECLARE(%FamilyFound)
  #ENDIF
  #INSERT(%FamilyFind, %FamilyFound)
  #IF(NOT VAREXISTS(%FamilyTextRYB))
    #DECLARE(%FamilyTextRYB)
  #ENDIF
  #SET(%FamilyTextRYB,                  'Template Family being used by this application.')
  #!SET(%FamilyTextRYB, %FamilyTextRYB & '<13,10><13,10>')
  #SET(%FamilyTextRYB, %FamilyTextRYB & ' This will be set automatically once a compile has been performed.')
  #SET(%FamilyTextRYB, %FamilyTextRYB & '')
#ENDPREPARE
#!BOXED(''),AT(,,278)
#BOXED(UPPER(%Application) & '.' & UPPER(%ProgramExtension)),AT(,,454),PROP(PROP:FontStyle,700)
  #DISPLAY(''),AT(,,,1)
  #!DISPLAY(%FamilyTextRYB),AT(,,260,34)
  #DISPLAY(%FamilyTextRYB),AT(,,436,14)
  #ENABLE(%FALSE)
    #!PROMPT('Template Family:',@S10),%FamilyFound,DEFAULT(''),AT(80,,100,10)
    #PROMPT('Template Family:',@S10),%FamilyFound,DEFAULT(''),AT(80,,100,10),PROP(PROP:READONLY,1),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
  #ENDENABLE
  #DISPLAY(''),AT(,,,1)
#ENDBOXED
#!-----------------------------------------------------------------------------------------------------
#GROUP(%TabClarionVersion)
#TAB('Family')
#INSERT(%ClarionFamily)
#ENDTAB
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#GROUP(%PreambleTD)
#PREPARE
  #IF(NOT VAREXISTS(%PreambleTextTD))
    #DECLARE(%PreambleTextTD)
  #ENDIF
  #SET(%PreambleTextTD,                   'Thread Debug (TD) template collection.')
  #SET(%PreambleTextTD, %PreambleTextTD & '<13,10>Templates copyright © by each individual author.')
  #SET(%PreambleTextTD, %PreambleTextTD & '<13,10>Collection created by Roberto Artigas y Soler.')
  #SET(%PreambleTextTD, %PreambleTextTD & '<13,10>E-MAIL: roberto.artigas.dev@gmail.com')
#ENDPREPARE
#BOXED(%TDtplVerNumber),AT(,,,55),PROP(PROP:FontStyle,700)
  #DISPLAY(''),AT(,,,1)
  #DISPLAY(%TDtplVerRelDate ),AT(10,10)
  #DISPLAY(%TDtplVerCpyRghts),AT(10,20)
  #DISPLAY(%TDtplVerRsrvAll ),AT(10,30)
  #DISPLAY(%TDtplVerEmail   ),AT(10,40)
  #!DISPLAY(%PreambleTextRYB),AT(,,260,44)
#ENDBOXED
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#GROUP(%MITLicenseText)
#PREPARE
  #IF(NOT VAREXISTS(%MITLicenseText01))
    #DECLARE(%MITLicenseText01)
  #ENDIF
  #IF(NOT VAREXISTS(%MITLicenseText02))
    #DECLARE(%MITLicenseText02)
  #ENDIF
  #IF(NOT VAREXISTS(%MITLicenseText03))
    #DECLARE(%MITLicenseText03)
  #ENDIF
  #SET(%MITLicenseText01, '')
  #SET(%MITLicenseText01, %MITLicenseText01 & 'Permission is hereby granted, free of charge, to any person')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' obtaining a copy of this software and associated documentation')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' files (the "Software"), to deal in the software without')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' restriction, including without limitation the rights to use,')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' copy, modify, merge, publish, distribute, sublicense, and/or sell')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' copies of the Software, and to permit persons to whom the Software')
  #SET(%MITLicenseText01, %MITLicenseText01 & ' is furnished to do so, subject to the following conditions:')
  #SET(%MITLicenseText02, '')
  #SET(%MITLicenseText02, %MITLicenseText02 & 'The above copyright notice and this permission notice shall be')
  #SET(%MITLicenseText02, %MITLicenseText02 & ' included in all copies or substantial portions of the Software.')
  #SET(%MITLicenseText03, '')
  #SET(%MITLicenseText03, %MITLicenseText03 & 'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER')
  #SET(%MITLicenseText03, %MITLicenseText03 & ' DEALINGS IN THE SOFTWARE.')
  #SET(%MITLicenseText03, %MITLicenseText03 & '')
#ENDPREPARE
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#GROUP(%MITLicense)
#INSERT(%MITLicenseText)
#!BOXED('MIT License'),AT(,,278),PROP(PROP:FontStyle,700)
#BOXED('MIT License'),AT(,,454),PROP(PROP:FontStyle,700)
  #DISPLAY(''),AT(,,,1)
  #DISPLAY('Copyright © ' & YEAR(TODAY()) & ' by Roberto Artigas y Soler.'),PROP(PROP:FontStyle,700)
  #DISPLAY(''),AT(,,,1)
  #DISPLAY(%MITLicenseText01),AT(,,436,36)
  #DISPLAY(%MITLicenseText02),AT(,,436,14),PROP(PROP:FontStyle,700)
  #DISPLAY(%MITLicenseText03),AT(,,436,44)
#ENDBOXED
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#GROUP(%CopyrightTD)
#PREPARE
  #IF(NOT VAREXISTS(%CopyrightTD01))
    #DECLARE(%CopyrightTD01)
  #ENDIF
  #SET(%CopyrightTD01, 'Thread Debug (TD) Template Collection for ABC.')
  #SET(%CopyrightTD01, %CopyrightTD01 & '<13,10>Copyright © 2018-' & YEAR(TODAY()) & ' by Roberto Artigas y Soler.')
  #SET(%CopyrightTD01, %CopyrightTD01 & '<13,10>All copyrights reserved world wide.')
  #SET(%CopyrightTD01, %CopyrightTD01 & '<13,10>E-MAIL: roberto.artigas.dev@gmail.com')
#ENDPREPARE
#BOXED('Version ' & %TDtplVersion & ' [' & %TDtplRelease & ']'),AT(,,454),PROP(PROP:FontStyle,700)
  #DISPLAY(''),AT(,,,1)
  #DISPLAY(%CopyrightTD01),AT(,,436,34)
#ENDBOXED
#!-----------------------------------------------------------------------------------------------------
#GROUP(%MITLicenseTD)
#TAB('TD'),PROP(PROP:FontStyle,700)
  #INSERT(%CopyrightTD) 
  #INSERT(%MITLicense)
  #INSERT(%ClarionFamily)
#ENDTAB
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#!
#! Tread Debug (TD) Template Collection (Global Extension)
#!
#EXTENSION(ThreadDebugGlobal, 'RA.2019.01.01: Thread Debug (Global Extension)'),DESCRIPTION('RYB | Thread Debug (Global Extension): Version: ' & %TDtplVersion & ', Release: ' & %TDtplRelease),APPLICATION
#!INSERT(%PrepareOptionsTD)
#PREPARE
  #INSERT(%FamilyFind, %FamilyFound)
#ENDPREPARE
#!-----------------------------------------------------------------------------
#! RA.2019.01.01.TUE      Roberto Artigas y Soler
#!  *   Begin creation of this template collection for the ABC chain.
#!-----------------------------------------------------------------------------
#INSERT(%PreambleTD)
#!
#DISPLAY(''),AT(,,,1)
#PROMPT('Disable Thread Debug Template Collection',CHECK),%TDtplDisable,AT(10),DEFAULT(0)
#DISPLAY(''),AT(,,,1)
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#!SHEET,AT(,,288),HSCROLL
#SHEET,AT(,,464),HSCROLL
#INSERT(%MITLicenseTD)
#INSERT(%ExplainTD)
#INSERT(%ContributeTD)
#TAB(UPPER(%Application) & '.' & UPPER(%ProgramExtension))
  #BOXED(UPPER(%Application) & '.' & UPPER(%ProgramExtension)),AT(,,454),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
    #DISPLAY(''),AT(,,,1)
    #PROMPT('Global Class (TDG):',@S40),%TDtplClassGlobal1Name,AT(100,,,10),REQ,DEFAULT('TDG')
    #PROMPT('Multi DLL',CHECK),%TDtplMultiDLL,AT(100),DEFAULT(0)
    #ENABLE(%TDtplMultiDLL=1),ClEAR
      #PROMPT('Declaration:',DROP('Declared in another App[0]|Declared in this app[1]')),%TDtplMultiDLLData,DEFAULT(0),AT(100,,,10)
    #ENDENABLE
    #PROMPT('Prefix:',@S20),%TDtplPrefix,DEFAULT('!'),AT(100)
    #PROMPT('Log File Name:',@S120),%TDtplLogFileName,DEFAULT('DebugLog.txt'),AT(100)
  #ENDBOXED
#!  #BOXED('Global Classes (Application Level)'),AT(,,454)
#!    #DISPLAY(''),AT(,,,1)
#!    #PROMPT('Global Class 1 Enable',CHECK),%TDtplClassGlobal1,AT(100),DEFAULT(1),PROP(PROP:READONLY,1),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
#!    #ENABLE(%TDtplClassGlobal1=1)
#!      #PROMPT('Global Class 1 (TDG):',@S40),%TDtplClassGlobal1Name,AT(100,,,10),REQ,DEFAULT('TDG')
#!    #ENDENABLE
#!    #PROMPT('Global Class 2 Enable',CHECK),%TDtplClassGlobal2,AT(100),DEFAULT(0)
#!    #ENABLE(%TDtplClassGlobal2=1)
#!      #PROMPT('Global Class 2 (UDG):',@S40),%TDtplClassGlobal2Name,AT(100,,,10),REQ,DEFAULT('UDG')
#!    #ENDENABLE
#!    #PROMPT('Global Class 3 Enable',CHECK),%TDtplClassGlobal3,AT(100),DEFAULT(0)
#!    #ENABLE(%TDtplClassGlobal3=1)
#!      #PROMPT('Global Class 3 (DBG):',@S40),%TDtplClassGlobal3Name,AT(100,,,10),REQ,DEFAULT('DBG')
#!    #ENDENABLE
#!  #ENDBOXED
  #BOXED('Local Classes (Procedure Level)'),AT(,,454),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
    #DISPLAY(''),AT(,,,1)
    #PROMPT('Local Class 1 Enable',CHECK),%TDtplClassLocal1,AT(100),DEFAULT(0)
    #ENABLE(%TDtplClassLocal1=1)
      #PROMPT('Local Class 1 (TD):',@S40),%TDtplClassLocal1Name,AT(100,,,10),REQ,DEFAULT('TD')
    #ENDENABLE
    #PROMPT('Local Class 2 Enable',CHECK),%TDtplClassLocal2,AT(100),DEFAULT(0)
    #ENABLE(%TDtplClassLocal2=1)
      #PROMPT('Local Class 2 (UD):',@S40),%TDtplClassLocal2Name,AT(100,,,10),REQ,DEFAULT('UD')
    #ENDENABLE
    #PROMPT('Local Class 3 Enable',CHECK),%TDtplClassLocal3,AT(100),DEFAULT(0)
    #ENABLE(%TDtplClassLocal3=1)
      #PROMPT('Local Class 3 (DB):',@S40),%TDtplClassLocal3Name,AT(100,,,10),REQ,DEFAULT('DB')
    #ENDENABLE
  #ENDBOXED
#ENDTAB
#TAB('Options')
  #BOXED('Global Options (Application Level)'),AT(,,454),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
    #DISPLAY(''),AT(,,,1)
    #PROMPT('Generate GLOBAL Information' ,CHECK),%TDtplGenerateGlobalInformation ,AT(100),DEFAULT(0)
    #PROMPT('Generate GLOBAL Variables'   ,CHECK),%TDtplGenerateGlobalVariables   ,AT(100),DEFAULT(0)
    #DISPLAY('') #!,AT(,,,1)
    #ENABLE(%TDtplGenerateGlobalInformation)
    #PROMPT('Dump GLOBAL Information'     ,CHECK),%TDtplDumpGlobalInformation     ,AT(100),DEFAULT(0)
    #ENDENABLE
    #ENABLE(%TDtplGenerateGlobalVariables)
    #PROMPT('Dump GLOBAL Variables'       ,CHECK),%TDtplDumpGlobalVariables       ,AT(100),DEFAULT(0)
    #ENDENABLE
    #PROMPT('Dump GLOBAL Templates'       ,CHECK),%TDtplDumpGlobalTemplates       ,AT(100),DEFAULT(0)
  #ENDBOXED
  #BOXED('Local Options (Procedure Level)'),AT(,,454),PROP(PROP:FontStyle,700),PROP(PROP:FontColor,0C79A3H)
    #DISPLAY(''),AT(,,,1)
    #PROMPT('Dump LOCAL Information'      ,CHECK),%TDtplDumpLocalInformation      ,AT(100),DEFAULT(0)
    #PROMPT('Dump LOCAL Variables'        ,CHECK),%TDtplDumpLocalVariables        ,AT(100),DEFAULT(0)
    #PROMPT('Dump LOCAL Templates'        ,CHECK),%TDtplDumpLocalTemplates        ,AT(100),DEFAULT(0)
  #ENDBOXED
#ENDTAB
#INSERT(%TabExplain)
#INSERT(%TabContribute)
#!INSERT(%TabPurpose)
#!INSERT(%TabInstructions)
#!INSERT(%TabContributors)
#!INSERT(%TabClarionVer)
#!INSERT(%TabTesting)
#ENDSHEET
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#INSERT(%PrepareVariablesTD)
#!-----------------------------------------------------------------------------------------------------
#! Procedure LOCAL generation
#!-----------------------------------------------------------------------------------------------------
#AT(%DataSection),WHERE(~%TDtplDisable),PRIORITY(20)
#INSERT(%GenDataSection)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'Run','(),BYTE'),WHERE(~%TDtplDisable),PRIORITY(0004)
#INSERT(%GenBeginRUN)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'Run','(),BYTE'),WHERE(~%TDtplDisable),PRIORITY(9996)
#INSERT(%GenEndedRUN)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#! Application GLOBAL generation
#!-----------------------------------------------------------------------------------------------------
#ATSTART
#INSERT(%GenATSTART)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#ATEND
#INSERT(%GenATEND)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%AfterGlobalIncludes),WHERE(~%TDtplDisable),PRIORITY(20)
  #INSERT(%GenAfterGlobalIncludes)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%CustomGlobalDeclarations),WHERE(~%TDtplDisable),PRIORITY(20)
  #INSERT(%GenCustomGlobalDeclarations)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%GlobalData),WHERE(~%TDtplDisable),PRIORITY(20)
#INSERT(%GenGlobalData)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%DLLExportList),WHERE(%TDtplMultiDLL=1 AND %TDtplMultiDLLData=1 AND ~%TDtplDisable),PRIORITY(10)
  #INSERT(%GenDLLExportList)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%ProgramSetup),WHERE(~%TDtplDisable),PRIORITY(0001)
#INSERT(%GenProgramSetup)
#INSERT(%GenProgramSetupDumpInfo)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%ProgramEnd),WHERE(~%TDtplDisable),PRIORITY(9999)
#INSERT(%GenProgramEnd)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT (%GlobalMap),PRIORITY(9000)
#INSERT(%GenGlobalMap)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#AT(%ProgramProcedures),PRIORITY(9000)
#INSERT(%GenProgramProcedures)
#ENDAT
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
#INCLUDE('RYB_TD001.TPW')               #! RA.2019.01.01: Code generation groups
#INCLUDE('RYB_TD010.TPW')               #! RA.2019.01.01: Useful helper groups
#INCLUDE('RYB_TD011.TPW')               #! RA.2019.01.24: Global #EMBED Markers
#!INCLUDE('UDRA_UltimateDebug.TPW')
#!INCLUDE('UDRA_UltimateDebug2.TPW') #! RA.2014.03.28 - Trace Code ALL procedures
#!INCLUDE('UDRA_UltimateDebug3.TPW') #! RA.2014.04.12 - Tracing Source (*.CLW) files
#!INCLUDE('UDRA_UltimateDebug4.TPW') #! RA.2014.06.04 - John Hickey's procedure tracker
#!INCLUDE('UDRA_UltimateDebug5.TPW') #! RA.2015.01.02 - Timming Code ALL procedures
#!INCLUDE('UDRA_UltimateDebug6.TPW') #! RA.2015.01.02 - Callback and SQLCallback - Experimental - NOT WORKING (NOT NEEDED - RA.2018.02.28)
#!-----------------------------------------------------------------------------------------------------
#!-----------------------------------------------------------------------------------------------------
