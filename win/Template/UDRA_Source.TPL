#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#TEMPLATE(zUDresources,'RA.2018.02.23: UD Experiment Template Resources'),FAMILY('ABC')
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#SYSTEM
#!-----------------------------------------------------------------------------
#EQUATE(%udeVersion     ,'Version 1.0 [ 2018.02.24 ]'                        )
#EQUATE(%udeVerName     ,'Ultimate Debug Experiment Template Resources.'     )
#EQUATE(%udeVerCpyRghts ,'Templates copyright © by individual authors.'      )
#EQUATE(%udeVerRsrvAll  ,'Collection created by Roberto Artigas y Soler.'    )
#EQUATE(%udeVerEmail    ,'E-MAIL: roberto.artigas.dev@gmail.com'             )
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#GROUP(%PreambleUDE)
#BOXED(%udeVersion),AT(,,,55),PROP(PROP:FontStyle,700)
  #DISPLAY(%udeVerName    ),AT(10,10)
  #DISPLAY(%udeVerCpyRghts),AT(10,20)
  #DISPLAY(%udeVerRsrvAll ),AT(10,30)
  #DISPLAY(%udeVerEmail   ),AT(10,40)
  #DISPLAY('')
#ENDBOXED
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#PROCEDURE(UD_Source,'UD Source Procedure'),HLP('~TPLProcSource.htm')#! Source procedure
#! RA.2018.02.23 - Created this template from the existing ABC source template.
#INSERT(%PreambleUDE)
#!
#COMMENT(60)
#BOXED('Source properties'),AT(10,,180)
#PROMPT('Generate Open/Close Files Routines',CHECK),%GenerateOpenClose,DEFAULT(%False),AT(10)
#ENABLE(%GenerateOpenClose=1),CLEAR
  #PROMPT('Generate Save/Restore Files Routines',CHECK),%GenerateSaveRestore,DEFAULT(%False),AT(10)
#ENDENABLE
#ENDBOXED
!!! <summary>
!!! Generated from procedure template - %ProcedureTemplate
#IF(%ProcedureDescription)
!!! %ProcedureDescription
#ENDIF
#IF(%ProcedureLongDescription)
#CALL(%SVInsertMultiLineSymbolPrefix,%ProcedureLongDescription)
#ENDIF
!!! </summary>
#IF(%Parameters)                                  #! IF Parameters used
%[20]Procedure PROCEDURE  %Parameters                           #<! Declare Procedure
#ELSE                                             #! ELSE (IF parameters not used)
%[20]Procedure PROCEDURE                                        #<! Declare Procedure
#ENDIF                                            #! END (IF parameters used)
#EMBED(%GatherSymbols,'Gather Template Symbols'),HIDE
#INSERT(%FileControlInitialize(ABC))
#!
#EMBED(%DataSection,'Data Section'),DATA,LABEL,TREE('Data Section{{PRIORITY(1000)}{{COLOR(0FFH)}')
#!
#SECTION
#CREATE('PBDY.$$$')
#EMBED(%BeforeCODEStatement),HIDE

  CODE
#INSERT(%FileUsedInProcDebug(ABC),1500),NOINDENT
#EMBED(%ProcessedCode,'Processed Code'),DATA,LABEL,TREE('Processed Code{{PRIORITY(2000)}{{COLOR(0FF0000H)}')     #! Embedded Source Code
  RETURN

#EMBED(%ProcedureRoutines,'Local Routines'),DATA,LABEL,TREE('Local Routines{{PRIORITY(3000)}{{COLOR(0FF0000H)}')

#EMBED(%LocalProcedures,'Local Procedures'),DATA,LABEL,TREE('Local Procedures{{PRIORITY(4000)}{{COLOR(0FF0000H)}')
#CLOSE('PBDY.$$$')
#ENDSECTION
#!
#EMBED(%LocalDataClasses),LABEL,HIDE
#EMBED(%LocalDataAfterClasses,'Local Data After Object Declarations'),LABEL,DATA,TREE('Other Declarations{{PRIORITY(1500)}{{COLOR(0FFH)}')
#!
#APPEND('PBDY.$$$'),SECTION
#REMOVE('PBDY.$$$')
#!
#EMBED(%AfterAPPENDStatement),LABEL,HIDE
#INSERT(%LocalMapCheck(ABC))
#!
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags)
#ENDAT
#!
#AT(%DataSection),PRIORITY(2500)
#CALL(%GenerateLocalData(ABC))
#EMBED(%LocalDataClassData),LABEL,HIDE
#ENDAT
#!
#AT(%DataSection),WHERE(%GenerateOpenClose)
FilesOpened     BYTE(0)
#ENDAT
#AT(%DataSection),WHERE(%GenerateSaveRestore)
  #FOR(%OtherFiles)
%OtherFiles::State  USHORT
  #ENDFOR
#ENDAT
#!
#AT(%ProcedureRoutines),WHERE(%GenerateSaveRestore)
SaveFiles  ROUTINE
#MESSAGE('File Control Save Code',3)
#EMBED(%BeforeFileSave,'SaveFiles ROUTINE, Before Saving Files'),TREE('SaveFiles ROUTINE, Before Saving Files{{PRIORITY(3500)}')
#FOR(%OtherFiles)
  %OtherFiles::State = Access:%OtherFiles.SaveFile()            #<! Save File referenced in 'Other Files' so need to inform its FileManager
#ENDFOR
#EMBED(%AfterFileSave,'SaveFiles ROUTINE, After Saving Files'),TREE('SaveFiles ROUTINE, After Saving Files{{PRIORITY(3600)}')
  EXIT
#!
RestoreFiles  ROUTINE
#MESSAGE('File Control Restore Code',3)
#EMBED(%BeforeFileRestore,'RestoreFiles ROUTINE, Before Restoring Files'),TREE('RestoreFiles ROUTINE, Before Restoring Files{{PRIORITY(3700)}')
#FOR(%OtherFiles)
  IF %OtherFiles::State <> 0
    Access:%OtherFiles.RestoreFile(%OtherFiles::State)            #<! Restore File referenced in 'Other Files' so need to inform its FileManager
  END
#ENDFOR
#EMBED(%AfterFileRestore,'RestoreFiles ROUTINE, After Restoring Files'),TREE('RestoreFiles ROUTINE, After Restoring Files{{PRIORITY(3800)}')
  EXIT
#ENDAT
#!
#AT(%ProcedureRoutines),WHERE(%GenerateOpenClose)
!--------------------------------------
OpenFiles  ROUTINE
#MESSAGE('File Control Open Code',3)
#EMBED(%BeforeFileOpen,'OpenFiles ROUTINE, Before Opening Files'),TREE('OpenFiles ROUTINE, Before Opening Files{{PRIORITY(3850)}')
#DECLARE(%CumFileOpen),MULTI
#DECLARE(%RelOpen),MULTI
#FOR(%ProcFilesUsed)
  #IF(~INLIST(%ProcFilesUsed,%CumFileOpen))
    #ADD (%CumFileOpen,%ProcFilesUsed)
    #INSERT(%AddRelatedFiles,%CumFileOpen,%ProcFilesUsed)
    #ADD(%RelOpen,%ProcFilesUsed)
#!Relate:%ProcFilesUsed.Open                                      #<! File %ProcFilesUsed used by this procedure, so make sure it's RelationManager is open
  #ENDIF
#ENDFOR
#FREE(%CumFileOpen)
#FOR(%RelOpen)
#ADD (%CumFileOpen,%RelOpen)
#IF(%IsOpeningFileSchematic(%RelOpen,%ProcFilesUsed,%CumFileOpen))
  Relate:%RelOpen.SetOpenRelated()
#ENDIF
  Relate:%RelOpen.Open()                                    #<! File %RelOpen used by this procedure, so make sure it's RelationManager is open
#ENDFOR
#FOR(%OtherFiles),WHERE(~INLIST(%OtherFiles,%RelOpen))
  Access:%OtherFiles.Open()                                       #<! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:%OtherFiles.UseFile()                                    #<! Use File referenced in 'Other Files' so need to inform it's FileManager
#ENDFOR
  FilesOpened = True
#EMBED(%AfterFileOpen,'OpenFiles ROUTINE, After Opening Files'),TREE('OpenFiles ROUTINE, After Opening Files{{PRIORITY(3860)}')
  EXIT
!--------------------------------------
CloseFiles ROUTINE
  #EMBED(%BeforeFileClose,'CloseFiles ROUTINE, Before Closing Files'),TREE('CloseFiles ROUTINE, Before Closing Files{{PRIORITY(3900)}')
  IF FilesOpened THEN
#DECLARE(%RelClose),MULTI
#FREE(%CumFileOpen)
#FOR(%ProcFilesUsed)
  #IF(~INLIST(%ProcFilesUsed,%CumFileOpen))
    #ADD (%CumFileOpen,%ProcFilesUsed)
    #INSERT(%AddRelatedFiles,%CumFileOpen,%ProcFilesUsed)
     #INSERT(%FileControlCloseFile,%ProcFilesUsed)
    #ADD (%RelClose,%ProcFilesUsed)
  #ENDIF
#ENDFOR
#!
#FOR(%OtherFiles),WHERE(~INLIST(%OtherFiles,%RelOpen))
     Access:%OtherFiles.Close()
#ENDFOR
     FilesOpened = False
  END
  #EMBED(%AfterFileClose,'CloseFiles ROUTINE, After Closing Files'),TREE('CloseFiles ROUTINE, After Closing Files{{PRIORITY(3950)}')
  EXIT
#ENDAT
#!
#!
#DEFAULT
NAME UD_DefaultABCSourceProcedure
[COMMON]
DESCRIPTION 'UD Source Procedure'
FROM ABC Source
#ENDDEFAULT
#!
#!