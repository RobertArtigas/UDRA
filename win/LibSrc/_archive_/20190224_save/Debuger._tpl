#TEMPLATE(Debuger,'RADFusion Debuger Template'),FAMILY('ABC')
#! Copyright 2006 - 2009 by RADFusion International, LLC.  Released under the Clarion Open Source License. (http://www.clarionmag.com/cmag/dospl.html)
#!
#! ABC compliant wrapper for debuger class originated by Skip Williams (http://www.clarionmag.com/cmag/v5/v5n01debuger.html) and extends the contributions 
#! by Mark Goldberg and Alan Telford.
#! This template was changed as of 2.0.4 to use a new base class and module name to avoid name
#! conflicts with other class definitions.
#!
#! In addition, the template provides support for Softvelocity's In-Memory driver and/or
#! the IP driver for C6 and C7 if either exists. (soon)
#!
#EXTENSION(DebugerABC,'Global Debuger Class'),APPLICATION
#DECLARE(%DebugerVersion)
#BOXED('Default prompts'),AT(0,0),WHERE(%False),HIDE
  #INSERT (%OOPHiddenPrompts(ABC))
#ENDBOXED
#!-------------------------------------------------------------------------
#!region Prepare structure
#PREPARE                                                                        #!Set class name in case developers never edits the name
  #EQUATE(%MemDriverInstalled,FILEEXISTS('C60MEM.DLL') OR FILEEXISTS('C70MEM.DLL'))  #!Check to see if memory driver is present
  #EQUATE(%IPDriverInstalled,FILEEXISTS('C60IPDX.LIB') OR FILEEXISTS('C70IP.DLL'))   #!Check to see if IP driver is present
  #SET(%DebugerVersion,'2.0.5')                                                 #!Current version update for existing installs
  #CALL(%ReadABCFiles(ABC))                                                     #!Read ABC class headers if needed
  #CALL(%SetClassDefaults(ABC),'DB','DB','DebugerClass')                        #!Set local name from libsrc class name
#ENDPREPARE                                                                     #! end #PREPARE 
#!end region
#!-------------------------------------------------------------------------
#ATSTART                                                                        #!Execute this code before #EXTENSION template generates its code
  #CALL(%ReadABCFiles(ABC))                                                     #!Read ABC class headers if needed
  #CALL(%SetClassDefaults(ABC),'DB','DB','DebugerClass')                        #!Set local name from libsrc class name
  #DECLARE(%DBInstance)
  #SET(%DBInstance,%ThisObjectName)
#ENDAT                                                                          #! end #ATSTART
#!-------------------------------------------------------------------------
#AT(%BeforeGenerateApplication)                                                 #!For exporting the class
  #CALL(%AddCategory(ABC),'DEBUGERCLASS')                                       #!The parameter in the !ABCIncludeFile comment in the INC file
  #CALL(%SetCategoryLocation(ABC),'DEBUGERCLASS','DebugerClass')                #!Used for DLLMode_ and LinkMode_ pragmas (see defines tab in project editor)
#ENDAT                                                                          #! end #AT(%BeforeGenerateApplication)
#!-------------------------------------------------------------------------
#INSERT(%RADFusionLogo)
#BUTTON('Global Debuger &Instance'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400),AT(,,90,20)                                   #!Global object dialog
  #INSERT(%RADFusionLogo)
  #BUTTON('&Debuger Class'),AT(4,,186,20),PROP(PROP:FontColor,0C79A3H),PROP(PROP:FontStyle,400) #!Display a button
    #WITH(%ClassItem,'DB')                                                      #!Show the global instance name
      #INSERT(%GlobalClassPrompts(ABC))                                         #!Add the class prompt dialog.
    #ENDWITH                                                                    #! end #WITH(%ClassItem,'DB')
  #ENDBUTTON                                                                    #! end #BUTTON('&Debuger Class')
  #BOXED(' Debuger Setup '),PROP(PROP:Bevel,1)
    #PROMPT('Debuger Version',@S10),%DebugerVersion,DEFAULT(%DebugerVersion),PROP(PROP:READONLY,1),PROP(PROP:FontColor,000FFFFH),PROP(PROP:Color,0FF0000H) #!Display only
    #PROMPT('Activate Debuger class ?',CHECK),%DebugTrue,DEFAULT(%True),AT(10),PROP(PROP:FontColor,0FF0000H),PROP(PROP:FontStyle,400),PROP(PROP:LEFT),PROP(PROP:Tip,'Check to turn on debuger')
    #PROMPT('Filter string',@s10),%DebugFilter,DEFAULT('`'),PROP(PROP:FontColor,0FF0000H),PROP(PROP:FontStyle,400),PROP(PROP:LEFT),PROP(PROP:Tip,'Enter a new string for filter')
    #BOXED(' Duplicate message behavior '),PROP(PROP:FontColor,0C79A3H),PROP(PROP:Bevel,-1)
      #DISPLAY('Number of duplicate messages before showing')
      #DISPLAY('a warning? Zero = never warn.')
      #PROMPT('Duplicates: ',SPIN(@N3,0,200,10)),%NumDups,DEFAULT(50),PROP(PROP:FontColor,0FF0000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:HScroll),PROP(PROP:VScroll),PROP(PROP:Tip,'Number of duplicate Debug messages in a row before <13,10> warning message. If zero, then do not track duplicates')
    #ENDBOXED
    #BOXED(' Clear behavior '),PROP(PROP:FontColor,0C79A3H),PROP(PROP:Bevel,-1)
      #DISPLAY('As of DebugView 4.3, you can clear the display.')
      #DISPLAY('Indicate the clear behavior below.')
      #PROMPT('Clear Behavior:',DROP('Never[DEBUGER:ClearNever]|Always[DEBUGER:ClearAlways]')),%SelfClear,PROP(PROP:FontColor,0FF0000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:Tip,'How to send message to DebugView to clear <13,10> itself (requires ver 4.3 or higher to work)')
    #ENDBOXED
    #BOXED(' Queue dumps '),PROP(PROP:FontColor,0C79A3H),PROP(PROP:Bevel,-1)
      #DISPLAY('Choose how you wish to dump queue contents.')
      #DISPLAY('Never: never dumps the queue to a file.')
      #DISPLAY('Prompt: Prompts for file name.')
      #DISPLAY('Always: Dumps to default file without prompting.')
      #PROMPT('Dump Queue Behavior:',DROP('Never[DEBUGER:ASCIINever]|Prompt[DEBUGER:ASCIIPrompt]|Always[DEBUGER:ASCIINoPrompt]')),%DumpQ,PROP(PROP:FontColor,0FF0000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:Tip,'Used to control default behavior of <13,10> dumpQueue when argFilename is omitted')
      #DISPLAY('')
      #ENABLE(%MemDriverInstalled=%True)
        #PROMPT('Use in-memory support?',CHECK),%UseMemory,DEFAULT(%False),AT(10),PROP(PROP:FontColor,0FF0000H),PROP(PROP:FontStyle,400),PROP(PROP:LEFT),PROP(PROP:Tip,'Check to use in-memory driver'),WHENACCEPTED(%MemDrivers())
      #ENDENABLE
      #ENABLE(%IPDriverInstalled=%True)
        #PROMPT('Use IP driver support?',CHECK),%UseIP,DEFAULT(%False),AT(10),PROP(PROP:FontColor,0FF0000H),PROP(PROP:FontStyle,400),PROP(PROP:LEFT),PROP(PROP:Tip,'Check to use IP driver'),WHENACCEPTED(%IPDrivers())
      #ENDENABLE
    #ENDBOXED
  #ENDBOXED
#ENDBUTTON                                                                         #! end #TAB('Local &Objects')
#!-------------------------------------------------------------------------
#BUTTON('De&buger Base Class'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400),AT(,,,20)                                           #!Global class dialog
  #INSERT(%RADFusionLogo)
  #PROMPT('&Default class:',FROM(%pClassName)),%ClassName,DEFAULT('DebugerClass'),REQ
  #DISPLAY()
  #BOXED(' Usage '),PROP(PROP:Bevel,-1)
    #DISPLAY('If you have another class you wish to use instead, select it from the list or use the default shown.'),AT(,,175,16),PROP(PROP:FontColor,0C79A3H)
  #ENDBOXED
#ENDBUTTON                                                                         #! end #TAB('C&lasses')
#!-------------------------------------------------------------------------
#AT(%GatherObjects)                                                             #!Ensure objects are known and loaded in memory
  #CALL(%ReadABCFiles(ABC))                                                     #!Read ABC class headers if needed
  #CALL(%AddObjectList(ABC),'DB')                                               #!Add the template object to object list 
  #ADD(%ObjectList,%ThisObjectName)                                             #!Add the object to the list of all objects
  #SET(%ObjectListType,'DebugerClass')                                          #!Set the base class name
#ENDAT                                                                          #! end #AT(%GatherObjects)
#!-------------------------------------------------------------------------
#AT(%GlobalDataClasses)                                                        #!At global class embed point
  #CALL(%SetClassItem(ABC), 'DB')                                               #!Set the current instance
  #INSERT(%GenerateClass(ABC), 'DB','Global instance and definition'),NOINDENT  #!and generate class instance into column 1, but preserve indent of template code
#ENDAT                                                                          #! end #AT(%GloballDataClasses)
#!-------------------------------------------------------------------------
#AT(%DebugerMethodCodeSection),PRIORITY(5000),DESCRIPTION('Parent Call'),WHERE(%ParentCallValid())  #!Add parent call embed point
  #CALL(%GenerateParentCall(ABC))                                               #!Generate the parent call
#ENDAT                                                                          #! end #AT(%DebugerClassMethodCodeSection,%ApplicationTemplateInstance)
#!-------------------------------------------------------------------------
#IF(%BaseClassToUse())                                                          #!If there is a base class
  #CALL(%FixClassName(ABC),%BaseClassToUse())                                   #!Assign the base class, cleaning up any errors
  #FOR(%pClassMethod)                                                           #!For every method in this class
    #FOR(%pClassMethodPrototype),WHERE(%MethodEmbedPointValid())                #!and the prototype is not private  
      #CALL(%SetupMethodCheck(ABC))                                             #!ensure the proper instance, any overrides, etc are generated
      #EMBED(%DebugerClassMethodDataSection,'DebugerClass Method Data Section'),%pClassMethod,%pClassMethodPrototype,LABEL,DATA,PREPARE(,%FixClassName(%FixBaseClassToUse('DebugerClass'))),TREE(%GetEmbedTreeDesc('DEBUG','DATA'))
      #?CODE                                                                    #<!Add CODE statement for method
      #EMBED(%DebugerClassMethodCodeSection,'DebugerClass Method Code Section'),%pClassMethod,%pClassMethodPrototype,PREPARE(,%FixClassName(%FixBaseClassToUse('DebugerClass'))),TREE(%GetEmbedTreeDesc('DEBUG','CODE'))
      #CALL(%CheckAddMethodPrototype(ABC),%ClassLines)                          #!Generate the prototype and structure for each method
    #ENDFOR                                                                     #! end #FOR(%pClassMethodPrototype),WHERE(%MethodEmbedPointValid())
  #ENDFOR                                                                       #! end #FOR(%pClassMethod)
  #CALL(%GenerateNewLocalMethods(ABC),'DEBUG',%True)                            #!Generate any new methods code from class dialog, if present
#ENDIF                                                                          #! end #IF(%BaseClassToUse())
#!-------------------------------------------------------------------------
#AT(%ProgramEnd),LAST                                                           #!Kill after ABC objects destroyed
%DBInstance.Kill()                                                                       #<! Kill the Debuger object
#ENDAT
#!-------------------------------------------------------------------------
#AT(%ProgramSetup)
%DBInstance.Init('%Application',%DebugTrue,%NumDups,%SelfClear,%DumpQ)                   #<!Intialize the Debuger class 
  #IF(%DebugFilter)
%DBInstance.SetFilter('%DebugFilter')
  #ENDIF
#ENDAT  
#!-------------------------------------------------------------------------
#AT(%GlobalData)
  #INSERT(%GenerateClass(ABC),'DB'),NOINDENT
#ENDAT 
#!-------------------------------------------------------------------------
#AT(%ProgramProcedures), WHERE(%ProgramExtension<>'DLL' OR ~%GlobalExternal)
  #CALL(%GenerateVirtuals(ABC), 'DB', 'Global Objects|Debuger Template', '%GlobalEmbedVirtuals(Debuger)', %TRUE)
#ENDAT
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#CODE(CallDebugBreak,'Insert code to call DebugBreak() here'),DESCRIPTION('Call DebugBreak (remove before shipping)'),REQ(DebugerABC)
#INSERT(%RADFusionLogo)
#BOXED(' DebugBreak Info '),PROP(PROP:Bevel,-1)
  #DISPLAY('There are no prompts for this template as it inserts a'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('DebugBreak() method call in your code. What this does '),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('is simulate a GPF. This is the fastest and easiest way'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('to set up the Clarion debugger ready for stepping '),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('through code.'),PROP(PROP:FontColor,0C79A3H),PROP(PROP:FontStyle,400)
#ENDBOXED
%DBInstance.DebugBreak()                                                       #<!Force "GPF" here.  REMOVE BEFORE SHIPPING
#!-------------------------------------------------------------------------
#CODE(DumpWindowControls,'Insert code to dump control info here'),DESCRIPTION('Code template to dump info about current controls'),SINGLE,REQ(DebugerABC)
#INSERT(%RADFusionLogo)
#BOXED(' DumpControls Info '),PROP(PROP:Bevel,-1)
  #DISPLAY('This template controls how you wish to call the '),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('DumpControls() method. The default shows all controls.'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY(' What this does is send info about each control on'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('a window. Info list is FEQ, AT(), text, type of control and'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('if its hidden. This code template is especially useful'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('when you create controls at runtime.'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY(''),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('If you want information about one FEQ, then enter the'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY('the same FEQ value for starting and ending FEQ.'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #DISPLAY(''),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #BOXED('')
    #PROMPT('Use current window?',CHECK),%CurrentWindow,DEFAULT(%True),PROP(PROP:FontColor,0FF0000H),PROP(PROP:FontStyle,400),PROP(PROP:LEFT),PROP(PROP:Tip,'Check to use current window')
    #BOXED(''),WHERE(%CurrentWindow=%False),CLEAR
      #PROMPT('Select &Window Label:',FROM(%Window)),%EnteredWindow,PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
      #VALIDATE(%EnteredWindow <> '','You must enter the label of a window')
    #ENDBOXED
  #ENDBOXED
  #PROMPT('Enter the &Starting FEQ Value:',SPIN(@n-14,-2147483647,2147483647)),%LowFEQ,DEFAULT(-2147483647),PROP(PROP:FontColor,0FF0000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:HScroll),PROP(PROP:VScroll)
  #PROMPT('Enter the &Ending FEQ Value:',SPIN(@n-14,-2147483647,2147483647)),%HiFEQ,DEFAULT(2147483647),PROP(PROP:FontColor,0FF0000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:HScroll),PROP(PROP:VScroll)
  #VALIDATE(%LowFEQ <= %HiFEQ,'Starting value MUST be less than equal to ending value!')
#ENDBOXED
#IF (%EnteredWindow <> '')
%DBInstance.DumpControls(%EnteredWindow,%LowFEQ,%HiFEQ)                         #<!Show controls on window
#ELSE
%DBInstance.DumpControls(,%LowFEQ,%HiFEQ)                                       #<!Show controls using default window
#ENDIF
#!-------------------------------------------------------------------------
#CODE(DumpQueue,'Dump the contents of a QUEUE'),DESCRIPTION('Code template to dump the contents of any QUEUE structure.'),SINGLE,REQ(DebugerABC)
#INSERT(%RADFusionLogo)
#BOXED(' DumpQueue Info '),PROP(PROP:Bevel,-1)
  #DISPLAY('This template controls how you wish to dump the contents of a QUEUE.'),AT(,,180,20),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400)
  #PROMPT('Enter message header (req)',@s20),%QueueMsgHdr,REQ,PROP(PROP:Tip,'Message header - enclose in single quotes')
  #PROMPT('Enter the label of a QUEUE (req)',@s20),%QueueLabel,REQ,PROP(PROP:Tip,'Label of Queue to be dumped')
  #PROMPT('Enter dumpfile name',@s20),%QueueDumpFile,PROP(PROP:Tip,'File name to be written to (if omitted, a prompt for filename will be issued)')
  #PROMPT('Enter picture format',@s15),%QueueFormat,PROP(PROP:Tip,'Picture format')
  #PROMPT('Max queue rows:',SPIN(@n5,0,99999)),%QueueMaxRows,DEFAULT(0),PROP(PROP:Tip,'Maximum number of queue records to dump or 0 for all')
  #PROMPT('Force?',CHECK),%QueueForce,DEFAULT(%False),PROP(PROP:Tip,'If checked then the DumpQ will be issued regardless of debug setting')
#ENDBOXED
DumpStr" = %DBInstance.DumpQue(%QueueMsgHdr,%QueueLabel,%QueueDumpFile,%QueueFormat,%QueueMaxRows,%QueueForce)
#!-Group code here---------------------------------------------------------
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#GROUP(%GlobalEmbedVirtuals, %TreeText, %DataText, %CodeText)
#EMBED(%DebugerClassDataSection,'DebugerClass Method Data Section'), %ApplicationTemplateInstance,%pClassMethod,%pClassMethodPrototype,TREE(%TreeText & %DataText)
  #?CODE
  #EMBED(%DebugerClassCodeSection,'DebugerClass Method Code Section'), %ApplicationTemplateInstance,%pClassMethod,%pClassMethodPrototype,TREE(%TreeText & %CodeText)
#!-------------------------------------------------------------------------
#GROUP(%ParentCallValid),AUTO
#DECLARE(%RVal)
#CALL(%ParentCallValid(ABC)),%RVal
#RETURN(%RVal)
#!-------------------------------------------------------------------------
#!#GROUP(%ODS,*%SymbolToPass)                                                      #!Example debug group, salt to taste
#!#RUNDLL('ODS.DLL','ODS','Debugview message: ' & %SymbolToPass),WIN32,RELEASE     #!ODS.DLL should be in bin - not shipped, search Clarion Mag for template debugger
#!-------------------------------------------------------------------------
#GROUP(%RADFusionLogo)
  #BOXED   (''),AT(5,0,184,93),PROP(PROP:Bevel,-1)
    #IMAGE('RADFusionLight800.jpg'), AT(5, 1, 183, 92)
  #ENDBOXED
#!-------------------------------------------------------------------------
#GROUP(%MemDrivers) 
#IF (%UseMemory = %True)
  %#pragma define(_MemDrivers_=>on)
#ELSE
  %#pragma define(_MemDrivers_=>off)
#ENDIF
#!-------------------------------------------------------------------------
#GROUP(%IPDrivers)
#IF (%UseIP = %True)
  %#pragma define(_IPDrivers_=>on)
#ELSE
  %#pragma define(_IPDrivers_=>off)
#ENDIF
#!-------------------------------------------------------------------------
