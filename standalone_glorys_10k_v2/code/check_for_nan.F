#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CHECK_FOR_NAN

C     !INTERFACE:
      SUBROUTINE CHECK_FOR_NAN( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Local (non-standard MITgcm) routine: scan the main prognostic
C     fields (etaN, uVel, vVel, wVel, theta, salt) for NaN values at
C     every timestep and, if one is found, print its location and
C     STOP the run immediately -- rather than letting it silently run
C     to completion (or crash much later) once a NaN has appeared.
C     Only wet (hFac>0) points are checked.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO k=1,1
         DO j=1,sNy
          DO i=1,sNx
           IF ( hFacC(i,j,1,bi,bj).GT.0. _d 0 .AND.
     &          etaN(i,j,bi,bj).NE.etaN(i,j,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'etaN', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
          ENDDO
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           IF ( hFacC(i,j,k,bi,bj).GT.0. _d 0 .AND.
     &          theta(i,j,k,bi,bj).NE.theta(i,j,k,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'theta', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
           IF ( hFacC(i,j,k,bi,bj).GT.0. _d 0 .AND.
     &          salt(i,j,k,bi,bj).NE.salt(i,j,k,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'salt', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
           IF ( hFacW(i,j,k,bi,bj).GT.0. _d 0 .AND.
     &          uVel(i,j,k,bi,bj).NE.uVel(i,j,k,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'uVel', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
           IF ( hFacS(i,j,k,bi,bj).GT.0. _d 0 .AND.
     &          vVel(i,j,k,bi,bj).NE.vVel(i,j,k,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'vVel', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
           IF ( hFacC(i,j,k,bi,bj).GT.0. _d 0 .AND.
     &          wVel(i,j,k,bi,bj).NE.wVel(i,j,k,bi,bj) ) THEN
            CALL CHECK_FOR_NAN_STOP( 'wVel', i,j,k,bi,bj,
     &                                myTime, myIter, myThid )
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CHECK_FOR_NAN_STOP

C     !INTERFACE:
      SUBROUTINE CHECK_FOR_NAN_STOP( fldName, i,j,k,bi,bj,
     &                                myTime, myIter, myThid )

C     !DESCRIPTION:
C     Print an informative error message (field name, grid location,
C     iteration/time, process id) and halt the run, following the
C     standard MITgcm "ABNORMAL END" STOP convention.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fldName
      INTEGER i,j,k,bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(3A)') 'CHECK_FOR_NAN: NaN detected in field ''',
     &     fldName, ''''
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A,5(A,I6))') 'CHECK_FOR_NAN: ',
     &     'at i=', i, ' , j=', j, ' , k=', k,
     &     ' , bi=', bi, ' , bj=', bj
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A,I10,A,1PE16.8,A,I6)') 'CHECK_FOR_NAN: ',
     &     'myIter=', myIter, ' , myTime=', myTime,
     &     ' , myProcId=', myProcId
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CHECK_FOR_NAN: NaN detected'

      RETURN
      END
