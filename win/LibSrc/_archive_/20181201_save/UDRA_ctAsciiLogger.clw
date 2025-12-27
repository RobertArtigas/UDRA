
  MEMBER()

!Region Notices 
! ================================================================================
! Notice : Copyright (C) 2014, Mark Goldberg
!          Distributed under LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
!
!    This file is part of CwUnit (https://github.com/MarkGoldberg/CwUnit)
!
!    CwUnit is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    CwUnit is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with CwUnit.  If not, see <http://www.gnu.org/licenses/>.
! ================================================================================
!EndRegion Notices 

  MAP
  END
  INCLUDE('UDRA_ctAsciiLogger.inc'),ONCE
  INCLUDE('Errors.clw'),ONCE
 
  PRAGMA ('link (C%V%ASC%X%%L%.LIB)') !<-- so don't have to add Ascii Driver to .CWProj

LogFileMG	FILE,DRIVER('ASCII'),CREATE													! RA.2018.03.03 - Avoid duplication
Record    RECORD
Line         STRING(4096) !HACK: arbitrary limit of LogFileMG
		  END
		END
 
ctAsciiLogger.FormatLine            PROCEDURE(STRING Msg)!,STRING,VIRTUAL 
	CODE
	RETURN Msg

!Region iLog Mirror Methods	
 
ctAsciiLogger.Add                   PROCEDURE(STRING Msg)!,VIRTUAL
 	CODE
 	LogFileMG.Line = SELF.FormatLine(Msg)
 	ADD(LogFileMG)                                                                      ! RA.2018.03.03 - Avoid duplication
	
ctAsciiLogger.Finish	PROCEDURE(STRING Info)!,LONG,PROC	
	CODE
	CLOSE(LogFileMG)																	! RA.2018.03.03 - Avoid duplication
	RETURN ERRORCODE()
 
ctAsciiLogger.Start	PROCEDURE(STRING Info)!,LONG,PROC
	CODE
	LogFileMG{PROP:Name} = Info															! RA.2018.03.03 - Avoid duplication
	SELF.Started = TRUE
	LOOP
		OPEN(LogFileMG)																	! RA.2018.03.03 - Avoid duplication
		CASE ERRORCODE()
		  OF NoError     ; BREAK
		  OF IsOpenErr   ; BREAK 
		  OF NoFileErr   ; CREATE(LogFileMG)											! RA.2018.03.03 - Avoid duplication
		                   IF ERRORCODE() THEN BREAK END
		END
	END
	RETURN ERRORCODE()

ctAsciiLogger.ClearLog   PROCEDURE()
	CODE
	IF SELF.Started 
	   EMPTY(LogFileMG)																	! RA.2018.03.03 - Avoid duplication
	END

!EndRegion iLog Mirror Methods	
!Region ILog 
	
ctAsciiLogger.ILog.Start	PROCEDURE(STRING Info)!,LONG,PROC
	CODE
	RETURN SELF.Start(Info)
	
ctAsciiLogger.ILog.Finish	PROCEDURE(STRING Info)!,LONG,PROC
	CODE
	RETURN SELF.Finish(Info)
	
ctAsciiLogger.ILog.Add     PROCEDURE(STRING ToAdd)
	CODE
	SELF.Add(ToAdd)


ctAsciiLogger.ILog.ClearLog   PROCEDURE()
	CODE
	SELF.ClearLog()
	
!EndRegion ILog