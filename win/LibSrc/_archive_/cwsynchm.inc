  OMIT('__EndOfInclude__',__CWSYNCHM_INC)

__CWSYNCHM_INC  EQUATE(1)

    INCLUDE('CWSYNCH.INT'),ONCE

ERROR_ALREADY_EXISTS EQUATE(183)

    MAP
      MODULE('')
        NewMutex            (),*IMutex,C,NAME('NewMutex')
        NewMutex            (STRING name, BYTE Own = FALSE, <*LONG Err>),*IMutex,C,NAME('NewProcessMutexCla')
        GetMutex            (STRING name, <*LONG Err>),*IMutex,C,NAME('GetProcessMutexCla')
        NewSemaphore        (LONG initial=0,LONG max=1),*ISemaphore,C,NAME('NewSemaphore')
        NewNamedSemaphore   (STRING name, LONG initial=0,LONG max=1, <*LONG Err>),*ISemaphore,C,NAME('NewProcessSemaphoreCla')
        GetSemaphore        (STRING name, <*LONG Err>),*ISemaphore,C,NAME('GetProcessSemaphoreCla')
        NewCriticalSection  (),*ICriticalSection,C,NAME('NewCriticalSection')
        NewReaderWriterLock (BYTE WritersHavePriority = TRUE),*IReaderWriterLock,C,NAME('NewReaderWriterLock')
      END
    END

! __EndOfInclude__
