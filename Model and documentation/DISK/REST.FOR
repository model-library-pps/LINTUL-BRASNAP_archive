**==OBSTRG.FOR
      REAL FUNCTION OBSTRG (VARIN,FILEIN,VARNAM)

*     Returns the estimated (through interpolation) observation from
*     datafile FILEIN for variable VARNAM if forcing was enabled.
*     If no observations could be found or forcing for the current
*     simulation time was disabled, the value VARIN is returned. Can
*     be used to force rate variables.

*     VARIN  - Value to be returned when no triggering is found      I
*     FILEIN - File name in which VARNAM should occur                I
*     VARNAM - Name of variable for which triggering is requested    I

*     OBSTRG - Returned value, = VARIN when no triggering is found,  O
*              if triggered the interpolated observation is returned

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*) FILEIN, VARNAM
      REAL VARIN

*     local parameters
      INTEGER ITASK
      LOGICAL FRCREQ, THERE, FRC, OBS_DAY
      REAL VARVAL
      SAVE

      ITASK  = 2
      FRCREQ = .TRUE.

      CALL OBSSYS (ITASK, FILEIN, VARNAM , FRCREQ,
     &             THERE, FRC   , OBS_DAY, VARVAL)

      IF (THERE.AND.FRC) THEN
         OBSTRG = VARVAL
      ELSE
         OBSTRG = VARIN
      END IF

      RETURN
      END

**==INQOBS.FOR
      LOGICAL FUNCTION INQOBS (FILEIN,VARNAM)

*     Inquires the observation system about the presence of
*     observation data on the simulation time.

*     Returns .true. or .false.

*     FILEIN - File name in which VARNAM should occur                I
*     VARNAM - Name of variable for which observations are requested I

*     INQOBS - Returns .true. when observation is found, else .false.

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*) FILEIN, VARNAM

*     local parameters
      INTEGER ITASK
      LOGICAL FRCREQ, THERE, FRC, OBS_DAY
      REAL VARVAL
      SAVE

      ITASK  = 2
      FRCREQ = .FALSE.

      CALL OBSSYS (ITASK, FILEIN, VARNAM , FRCREQ,
     &             THERE, FRC   , OBS_DAY, VARVAL)

      INQOBS = OBS_DAY

      RETURN
      END

**==GETOBS.FOR
      REAL FUNCTION GETOBS (FILEIN,VARNAM)

*     Returns the estimated (through interpolation) observation from
*     datafile FILEIN for variable VARNAM.
*     Can be used to get observations to the output file.

*     FILEIN - File name in which VARNAM should occur                I
*     VARNAM - Name of variable for which observations are requested I

*     GETOBS - Returned interpolated observation is returned

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*) FILEIN, VARNAM

*     local parameters
      INTEGER ITASK
      LOGICAL FRCREQ, THERE, FRC, OBS_DAY
      REAL VARVAL
      SAVE

      ITASK  = 2
      FRCREQ = .FALSE.

      CALL OBSSYS (ITASK, FILEIN, VARNAM , FRCREQ,
     &             THERE, FRC   , OBS_DAY, VARVAL)

      IF (.NOT.THERE) CALL FATALERR ('GETOBS','data not available')
      GETOBS = VARVAL

      RETURN
      END

**==INTGR2.FOR
      REAL FUNCTION INTGR2 (STATE,RATE,DELT,FILEIN,STATNM)

*     Integrates similar to the INTGRL function when no observations
*     and/or forcing was given. When forcing was enabled, the
*     estimated observation is returned

*     STATE  - Old value of state                                    I
*     RATE   - Rate as calculated by model                           I
*     DELT   - Time step to be applied when no forcing takes place   I
*     FILEIN - Name of datafile in which STATNM is possibly forced   I
*     STATNM - Name of state variable                                I

*     INTGR2 - Function result, integrated or forced                 I

      IMPLICIT NONE
*     formal parameters
      REAL STATE, RATE, DELT
      CHARACTER*(*) STATNM, FILEIN

*     local variables
      LOGICAL FRCREQ, FRC, THERE, OBS_DAY
      REAL FRCSTA
      INTEGER ITASK
      SAVE

      ITASK  = 2
      FRCREQ = .TRUE.

      CALL OBSSYS (ITASK, FILEIN, STATNM , FRCREQ,
     &             THERE, FRC   , OBS_DAY, FRCSTA)

      IF (.NOT.FRC) THEN
         INTGR2 = STATE+RATE*DELT
      ELSE
         INTGR2 = FRCSTA
      END IF

      RETURN
      END

**==OBSINI.FOR
      SUBROUTINE OBSINI

*     initialises the observation/forcing system

      IMPLICIT NONE
      INTEGER ITASK
      CHARACTER CDUM1*1, CDUM2*1
      LOGICAL LDUM1,LDUM2,LDUM3,LDUM4
      REAL RDUM1
      SAVE

      ITASK = 1
      CDUM1 = ' '
      CDUM2 = ' '
      LDUM1 = .FALSE.
      LDUM2 = .FALSE.
      LDUM3 = .FALSE.
      LDUM4 = .FALSE.
      RDUM1 = -99.
      CALL OBSSYS (ITASK,CDUM1,CDUM2,
     &             LDUM1,LDUM2,LDUM3,LDUM4,RDUM1)

      RETURN
      END

**==DS1900.FOR
      REAL FUNCTION DS1900 (YEAR,DAY)

*     calculate number of days since Jan 1, 1900

      REAL YEAR, DAY
      SAVE

      DS1900 = (YEAR-1900.)*365.+DAY+REAL (INT(YEAR+3.-1900.)/4)

      RETURN
      END

**==OBSSYS.FOR
      SUBROUTINE OBSSYS (ITASK,FILEIN,VARNAM,FRCREQ,
     &                   THERE,FRC,OBS_DAY,VARVAL)
      IMPLICIT NONE

* remarks:
*            - extrapolatie buiten _obs range vindt niet plaats,
*            - _trg array wordt met een dummy dag verlengd zodat de
*              laatste _trg value nog een dag aktief is
*            - _trg array is alleen werkzaam binnen de gedefinieerde
*              datum range. Als de tijd daarbuiten komt wordt de _frc
*              waarde van nul aangenomen (geen forcering dus).
*            - belangrijk: _TRG = 1 en _TRG = 2 zijn w.b. betekenis
*              omgedraaid
*            - als forcing aanstaan en er zijn op die tijd geen
*              observaties wordt there=.false. teruggegeven
*              (dit heeft in intgr2 tot gevolg dat er gesimuleerd wordt)
*            - _FRC = 0 do not force
*              _FRC = 1 use triggering table
*              _FRC = 2 force during whole simulation
*              _TRG = 0 do not force
*              _TRG = 1 do point reset
*              _TRG = 2 force when applicable

*     Author : Daniel van Kraalingen, J.J.M. Riethoven
*              AB-DLO, PO Box 14, 6700 AA  Wageningen
*     Date   : May 1996
*     Version: 1.42

*     ITASK   - Task to be executed,                                  I
*               = 1, reset everything
*               = 2, get observed value, if available, and determine
*                    forcing status
*     FILEIN  - Filename to which variable name corresponds           I
*               (ITASK = 2 only)
*     VARNAM  - Variable name                                         I
*     FRCREQ  - Request forcing status                                I
*     THERE   - Flags presence of VARNAM_obs in FILEIN                O
*     FRC     - Flag indicates forcing of VARNAM                      O
*     OBS_DAY - Flag indicates simulation equals date of observ.      O
*     VARVAL  - Variable value                                        O

*     Formal parameters
      INTEGER ITASK
      CHARACTER FILEIN*(*), VARNAM*(*)
      REAL VARVAL
      LOGICAL FRCREQ,THERE,FRC,OBS_DAY

*     Common area with MODELS routine of FSE
      REAL YEAR, DOY
      LOGICAL TERMNL
      INTEGER IUNITD, IUNITL
      COMMON /FSECM1/ YEAR, DOY, IUNITD, IUNITL, TERMNL

*     Declarations for RD routine data
      INTEGER   ILNDAT,INFDAT
      PARAMETER (ILNDAT=400)
      CHARACTER DATLIS(ILNDAT)*31

*     Storage of input files
*     ======================
*     MNINF  - Maximum number of input files
*     NINF   - Actual number of input files
*     AINF   - Character array with names of input files
*     INFBPT - Points to first variable name from input file
*              in array of variable names
*     INFEPT - Points to last variable name from input file
*              in array of variable names

      INTEGER MNINF, NINF
      PARAMETER (MNINF=10)
      CHARACTER AINF(MNINF)*80,INF*80
      INTEGER INFBPT(MNINF),INFEPT(MNINF)

*     Storage of variable names
*     =========================
*     (length 27 because suffix is removed)
*     MNVAR  - Maximum number of relevant variable names
*     NVAR   - Actual number of variable names
*     AVAR   - Array with variable names
*     OBSBPT - Points to first value from variable_obs in
*              array of values
*     OBSEPT - Points to last value from variable_obs in
*              array of values
*     TRGBPT - Points to first value from variable_trg in
*              array of values
*     TRGEPT - Points to last value from variable_trg in
*              array of values
*     FRCVAL - Array with _frc values
*     TERVAL - Array with terminal observation values (_ter)
      INTEGER MNVAR,NVAR
      PARAMETER (MNVAR=100)
      CHARACTER AVAR(MNVAR)*27, TMP27*27
      INTEGER OBSBPT(MNVAR),OBSEPT(MNVAR)
      INTEGER TRGBPT(MNVAR),TRGEPT(MNVAR)
      INTEGER FRCVAL(MNVAR)
      REAL    TERVAL(MNVAR)

*     Storage of values
*     =================
      INTEGER MNVAL, NVAL
      PARAMETER (MNVAL=1000)
      REAL AVAL(MNVAL)

*     Local variables
      INTEGER IN, I1, I2, I3, I4, IL, ILFIL, IS, ILEN, IFND,IFINDC
      INTEGER PVAR
      REAL LINT2, NDAYS, DS1900
      CHARACTER TMP31*31
      LOGICAL FND, FND2, MISS, INIT

      SAVE

      DATA INIT /.FALSE./

      IF (ITASK.EQ.1) THEN
*        reset everything
         NINF = 0
         NVAR = 0
         NVAL = 0

*        RESET FOR TEST PURPOSES ONLY
         DO I1=1,MNINF
            AINF(I1) = 'XXXXXXXXXX'
            INFBPT(I1) = -99
            INFEPT(I1) = -99
         END DO
         DO I1=1,MNVAR
            AVAR(I1) = 'XXXXXXX'
            OBSBPT(I1) = -99
            OBSEPT(I1) = -99
            TRGBPT(I1) = -99
            TRGEPT(I1) = -99
            FRCVAL(I1) = -99
            TERVAL(I1) = -99.
         END DO
         DO I1=1,MNVAL
            AVAL(I1) = -99.
         END DO

         INIT = .TRUE.

      ELSE IF (ITASK.EQ.2) THEN

         IF (.NOT.INIT) CALL FATALERR
     &      ('OBSSYS','system never initialized')

         ILFIL = ILEN (FILEIN)

*        see if file is already processed
         INF = FILEIN
         IN  = IFINDC (AINF,MNINF,1,NINF,INF)

         IF (IN.EQ.0) THEN
*           filename not yet processed, store filename in buffer
            NINF = NINF+1
            IF (NINF.GT.MNINF) CALL FATALERR
     &         ('OBSSYS','too many input files')
            AINF(NINF)   = INF
            INFBPT(NINF) = NVAR+1
            INFEPT(NINF) = 0

*           read the input file
            CALL RDINIT (IUNITD,IUNITL,INF)

*           start a loop to test the name of every variable from the
*           input file
            CALL RDINLV (.FALSE.,DATLIS,ILNDAT,INFDAT)

            DO I1=1,INFDAT
               TMP31 = DATLIS(I1)
               CALL UPPERC (TMP31)
               IL = ILEN (TMP31)
*              calculate the position of the character from where it is
*              four characters to the right until the end of the string
               IS = MAX (IL-4+1,1)

               IF (TMP31(IS:IL).EQ.'_OBS'.OR.
     &             TMP31(IS:IL).EQ.'_TER'.OR.
     &             TMP31(IS:IL).EQ.'_TRG'.OR.
     &             TMP31(IS:IL).EQ.'_FRC') THEN

*                 variable name has proper suffix

*                 assign part without suffix to temporary variable
                  TMP27 = ' '
                  TMP27 = TMP31(1:IS-1)

*                 check that this name is not yet read from other input
*                 files, this gives problems with reruns
                  FND = .FALSE.
                  IF (NINF.GT.1) THEN
                     I2  = IFINDC (AVAR,MNVAR,1,INFEPT(NINF-1),TMP27)
                     FND = I2.GT.0
                  END IF

*                 display warning if found
                  IF (FND) THEN
                     I2 = ILEN (TMP27)
                     WRITE (*,'(4A,/,2A)')
     &               ' WARNING from OBSSYS: Definitions for variable: ',
     &                 TMP27(1:I2),' from datafile: ',FILEIN(1:ILFIL),
     &               ' already occurred in a previous datafile,',
     &               ' this will give problems with reruns'
                     WRITE (IUNITL,'(4A,/,2A)')
     &               ' WARNING from OBSSYS: Definitions for variable: ',
     &                 TMP27(1:I2),' from datafile: ',FILEIN(1:ILFIL),
     &               ' already occurred in a previous datafile,',
     &               ' this will give problems with reruns'
                  END IF

                  FND   = .FALSE.
                  IF (INFEPT(NINF).GT.0) THEN
*                    a variable with a relevant suffix was already found
*                    in this input file, determine if this name with
*                    a different suffix was already parsed
                     I2  = IFINDC (AVAR,MNVAR,INFBPT(NINF),INFEPT(NINF),
     &                             TMP27)
                     FND = I2.GT.0
                  END IF

                  IF (.NOT.FND) THEN
*                    name with this suffix was not yet parsed
                     NVAR = NVAR+1
                     IF (NVAR.GT.MNVAR) CALL FATALERR
     &                  ('OBSSYS','too many variables')

*                    store name and reset pointers
                     INFEPT(NINF) = NVAR
                     AVAR(NVAR)   = TMP27
                     OBSBPT(NVAR) = 0
                     OBSEPT(NVAR) = 0
                     TRGBPT(NVAR) = 0
                     TRGEPT(NVAR) = 0
                     FRCVAL(NVAR) = -1
                     TERVAL(NVAR) = -99.
                     PVAR = NVAR
                  ELSE
                     PVAR = I2
                  END IF

                  MISS = .FALSE.

                  IF (TMP31(IS:IL).EQ.'_OBS'.OR.
     &                TMP31(IS:IL).EQ.'_TRG') THEN

*                    name ends with _OBS or _TRG suffix, we expect an
*                    array in the data file, read it
                     CALL RDAREA (DATLIS(I1),
     &                  AVAL(NVAL+1),MNVAL-NVAL,IFND)

                     IF (IFND.EQ.1) THEN
                        IF (AVAL(NVAL+1).EQ.-99.) THEN
*                          missing value on file, don't store value
*                          leave pointers at zero
                           MISS = .TRUE.
                        ELSE
                           CALL FATALERR ('OBSSYS','illegal value')
                        END IF
                     ELSE IF (IFND.EQ.3) THEN
                        CALL FATALERR ('OBSSYS','only one point')
                     ELSE IF (MOD(IFND,3).NE.0) THEN
                        WRITE (*,*) IFND,DATLIS(I1)
                        CALL FATALERR ('OBSSYS','no multiple of three')
                     END IF
                  END IF

                  IF (.NOT.MISS.AND.
     &               (TMP31(IS:IL).EQ.'_OBS'.OR.
     &                TMP31(IS:IL).EQ.'_TRG')) THEN

*                    shift array to the left because the year,day values
*                    are converted to number of days since 1900, (this
*                    is a single value)
                     I3 = NVAL
                     DO I2=NVAL+1,NVAL+IFND,3
                        I3 = I3+1
                        AVAL(I3) = DS1900 (AVAL(I2),AVAL(I2+1))
                        I3 = I3+1
                        AVAL(I3) = AVAL(I2+2)
                     END DO

                     IF (TMP31(IS:IL).EQ.'_OBS') THEN
*                       update _OBS pointers
                        OBSBPT(PVAR) = NVAL+1
                        OBSEPT(PVAR) = I3
                     ELSE
*                       add a zero _trg value at the end of the table
*                       so that the last _trg value in the file is
*                       'active', however, only for one day

                        IF (I3+2.GT.MNVAL) CALL FATALERR ('OBSSYS',
     &                      'value array full')
                        I3 = I3+2
                        AVAL(I3-1) = AVAL(I3-3)+1.
                        AVAL(I3)   = 0.

*                       update _TRG pointers
                        TRGBPT(PVAR) = NVAL+1
                        TRGEPT(PVAR) = I3

                        DO I2=TRGBPT(PVAR)+1,TRGEPT(PVAR),2
                           IF (AVAL(I2).LT.0..OR.AVAL(I2).GT.2.) THEN
                              CALL FATALERR ('OBSSYS',
     &                        'illegal triggering value')
                           END IF
                        END DO

                     END IF
                     NVAL = I3

                  ELSE IF (TMP31(IS:IL).EQ.'_TER') THEN
*                    variable has been a terminal _OBS value

                     CALL RDSREA (DATLIS(I1),TERVAL(PVAR))

                  ELSE IF (TMP31(IS:IL).EQ.'_FRC') THEN
*                    variable must have been a _FRC variable type,
*                    get and store the single value

                     CALL RDSINT (DATLIS(I1),FRCVAL(PVAR))
                     IF (FRCVAL(PVAR).LT.0.OR.FRCVAL(PVAR).GT.2) THEN
                        CALL FATALERR ('OBSSYS','illegal force value')
                     END IF
                  END IF
               END IF
            END DO
            IN = NINF
            CLOSE (IUNITD)

*           error checks
            DO I1=INFBPT(IN),INFEPT(IN)
*              loop through every variable in the input file
               IF (FRCVAL(I1).GE.1.AND.
     &             OBSBPT(I1).EQ.0.AND.
     &             OBSEPT(I1).EQ.0) THEN
*                 triggering or forcing without observations
                  I2 = ILEN (AVAR(I1))
                  WRITE (*,'(4A,/,A)')
     &            ' ERROR in OBSSYS: Variable: ', AVAR(I1)(1:I2),
     &            ' from datafile: ',FILEIN(1:ILFIL),
     &            ' should be triggered but it has no observations'
                  CALL FATALERR ('OBSSYS',' ')
               END IF

               IF (FRCVAL(I1).EQ.1.AND.
     &             TRGBPT(I1).EQ.0.AND.
     &             TRGEPT(I1).EQ.0) THEN
*                 triggering without triggering table
                  I2 = ILEN (AVAR(I1))
                  WRITE (*,'(4A,/,A)')
     &            ' ERROR in OBSSYS: Variable: ', AVAR(I1)(1:I2),
     &            ' from datafile: ',FILEIN(1:ILFIL),
     &            ' should be triggered but it has no triggering table'
                  CALL FATALERR ('OBSSYS',' ')
               END IF

               IF (OBSBPT(I1).GT.0.AND.OBSEPT(I1).GT.0.AND.
     &             OBSEPT(I1)-OBSBPT(I1).GE.3) THEN
*                 valid set of observations was found, test data
                  I2 = ILEN (AVAR(I1))
                  TMP31 = AVAR(I1)(1:I2)//'_OBS'
                  VARVAL = LINT2 (TMP31,AVAL(OBSBPT(I1)),
     &                     OBSEPT(I1)-OBSBPT(I1)+1,AVAL(OBSBPT(I1)))

               END IF
            END DO
         END IF

*        filename is processed when execution arrives here
*        try to get observed values, forcing and triggering information

         THERE   = .FALSE.
         FRC     = .FALSE.
         VARVAL  = -99.
         NDAYS   = DS1900 (YEAR,DOY)
         OBS_DAY = .FALSE.

         IF (INFBPT(IN).GT.0.AND.INFEPT(IN).GT.0) THEN
*           variable names with relevant suffixes were found in this
*           input file, see if the current variable name has been
*           defined in the input file

            TMP27 = VARNAM
            I2    = IFINDC (AVAR,MNVAR,INFBPT(IN),INFEPT(IN),TMP27)

            IF (I2.GT.0) THEN
*              requested variable found in input file

               IF (OBSBPT(I2).GT.0.AND.OBSEPT(I2).GT.0) THEN
*                 observations were supplied with this variable,
*                 get the observed value, only if within the date range
                  IF (AVAL(OBSBPT(I2)).LE.NDAYS.AND.
     &                AVAL(OBSEPT(I2)-1).GE.NDAYS) THEN

*                    current day is within defined dates in _obs table,

*                    see if the current date exactly matches an
*                    observation date

                     DO I1=OBSBPT(I2),OBSEPT(I2)-1,2
                        IF (AVAL(I1).EQ.NDAYS) OBS_DAY = .TRUE.
                     END DO
*                    carry out interpolation

                     VARVAL = LINT2 (VARNAM,AVAL(OBSBPT(I2)),
     &                        OBSEPT(I2)-OBSBPT(I2)+1,NDAYS)

                     THERE = .TRUE.
                     IF (TERMNL) WRITE (*,'(1X,3A)')
     &                 'WARNING from OBSSYS: ',
     &                 'time not yet outside observations of ',VARNAM
                  END IF

               ELSE IF (TERMNL.AND.TERVAL(I2).NE.-99.) THEN

*                 observations not defined, final value requested
                  THERE   = .TRUE.
                  VARVAL  = TERVAL(I2)
                  OBS_DAY = .TRUE.
               END IF

               IF (FRCVAL(I2).EQ.2.AND.FRCREQ.AND.THERE) THEN
*                 forcing was enabled
                  FRC = .TRUE.
               ELSE IF (FRCVAL(I2).EQ.1.AND.FRCREQ.AND.THERE) THEN
*                 triggering was enabled, search for the data
*                 that apply to the triggering date

                  FND = .FALSE.
                  IF (AVAL(TRGBPT(I2)).LE.NDAYS.AND.
     &                AVAL(TRGEPT(I2)-1).GE.NDAYS) THEN
*                    current day is within defined dates in _frc table,
*                    search for appropriate date in _frc table

                     I3  = TRGEPT(I2)-1

40                   IF (I3.GE.TRGBPT(I2).AND..NOT.FND) THEN
                        IF (AVAL(I3).LE.NDAYS) THEN
                           FND = .TRUE.
                        ELSE
                           I3 = I3-2
                        END IF
                     GOTO 40
                     END IF
                  END IF

                  IF (FND) THEN
*                    a triggering date was found

                     IF (I3.EQ.TRGEPT(I2)-1) THEN
*                       final date of triggering table is taken
                        WRITE (*,'(4A,/,A)')
     &                     ' WARNING from OBSSYS: Variable: ',
     &                       AVAR(I2)(1:I2),
     &                     ' from datafile: ',FILEIN(1:ILFIL),
     &                     ' Final date from triggering table is taken'
                        WRITE (IUNITL,'(4A,/,A)')
     &                     ' WARNING from OBSSYS: Variable: ',
     &                       AVAR(I2)(1:I2),
     &                     ' from datafile: ',FILEIN(1:ILFIL),
     &                     ' Final date from triggering table is taken'
                     END IF

                     IF (AVAL(I3+1).EQ.0.) THEN
*                       nothing
                        CONTINUE
                     ELSE IF (AVAL(I3+1).EQ.1.) THEN
*                       point reset
                        FRC = .TRUE.
                        AVAL(I3+1) = 0.

                        FND2 = .FALSE.
                        I4   = OBSBPT(I2)
50                      IF (I4.LE.OBSEPT(I2).AND..NOT.FND2) THEN
                           FND2 = AVAL(I3).EQ.AVAL(I4)
                           I4 = I4+1
                        GOTO 50
                        END IF
                        IF (.NOT.FND2) THEN
                           WRITE (*,'(4A,/,A)')
     &                     ' WARNING from OBSSYS: Variable: ',
     &                       AVAR(I2)(1:I2),
     &                     ' from datafile: ',FILEIN(1:ILFIL),
     &                     ' Point reset value is interpolated'
                           WRITE (IUNITL,'(4A,/,A)')
     &                     ' WARNING from OBSSYS: Variable: ',
     &                       AVAR(I2)(1:I2),
     &                     ' from datafile: ',FILEIN(1:ILFIL),
     &                     ' Point reset value is interpolated'
                        END IF
                     ELSE IF (AVAL(I3+1).EQ.2.) THEN
*                       forcing
                        FRC = .TRUE.
                     ELSE
                        CALL FATALERR ('OBSSYS','illegal value')
                     END IF
                  END IF
               END IF
            END IF
         END IF
      ELSE
         CALL FATALERR ('OBSSYS','internal error, illegal task')
      END IF

      RETURN
      END
      SUBROUTINE OPSTOR (VARNAM,VARVAL)
      IMPLICIT NONE

*     formal parameters

      CHARACTER*(*) VARNAM
      REAL VARVAL
*     local parameters
      INTEGER ITASK, IUNIT, INCL
      REAL VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER*1 VAR2NM
      SAVE

      ITASK  = 2
      IUNIT  = -99
      VARMIN = -99.
      VARMAX = -99.
      XLAB   = -99.
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.
      INCL   = -99

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB,
     &            VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      RETURN
      END
      SUBROUTINE OPINIT
      IMPLICIT NONE

*     formal parameters

*     local parameters
      INTEGER ITASK, IUNIT, INCL
      REAL VARVAL, VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER*1 VARNAM, VAR2NM
      SAVE

      ITASK = 1
      IUNIT  = -99
      VARNAM = ' '
      VARVAL = -99.
      VARMIN = -99.
      VARMAX = -99.
      XLAB   = -99.
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.
      INCL   = -99

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB,
     &            VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      RETURN
      END

      SUBROUTINE OPWRIT (IUNIT)
      IMPLICIT NONE

*     formal parameters

      INTEGER IUNIT

*     local parameters
      INTEGER ITASK, INCL
      REAL VARVAL, VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER*1 VARNAM,VAR2NM
      SAVE

      ITASK = 3
      VARNAM = ' '
      VARVAL = -99.
      VARMIN = -99.
      VARMAX = -99.
      XLAB   = -99.
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.
      INCL   = -99

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB,
     &            VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      RETURN
      END

      SUBROUTINE OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB,
     &                  VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      IMPLICIT NONE

*     formal parameters
      INTEGER ITASK,IUNIT, INCL
      CHARACTER*(*) VARNAM,VAR2NM
      REAL VARVAL,VARMIN,VARMAX,XLAB,VAR2MN,VAR2MX,VAR2LB

*     local parameters

*     IMNV - Maximum number of variables
*     IMNP - Maximum number of points per variable
*     INCL - Number of classes to use for frequency distribution

      INTEGER IMNV, IMNP, IMNCL
      PARAMETER (IMNV=15,IMNP=1000, IMNCL=30)
      REAL VALA(IMNP,IMNV),VALMX(IMNV),VALMN(IMNV),HULP(IMNP)
      CHARACTER*11 LNAM, LNAM2, NAMA(IMNV)
      INTEGER IWBEG(IMNV),IWEND(IMNV)

      CHARACTER LINE*1000
      CHARACTER FILENM*80
      INTEGER INP(IMNV),INDX(IMNP), ICL(IMNCL)
      INTEGER INV,I1,I2,IFINDC,ITOLD,ISUM,IV,IV2,ILEN,IM
      REAL TMP1, TMP2, XTMP,YTMP,SUM
      REAL RDIFF
      LOGICAL EOF, NODATA, LTMP
      SAVE

      DATA ITOLD /0/, NODATA /.TRUE./

      IF (ITASK.EQ.1) THEN

*        initialize

*        reset sum, maximum and minimum
         DO 10 I1=1,IMNV
            VALMX(I1) = 0.
            VALMN(I1) = 0.
            INP(I1)   = 0
            DO 12 I2=1,IMNP
               VALA(I2,I1) = -99.
12          CONTINUE
10       CONTINUE
         INV = 0

         NODATA = .TRUE.

      ELSE IF (ITASK.EQ.2) THEN

*        store data

         IF (ITOLD.NE.1.AND.ITOLD.NE.2)
     &      CALL ERROR ('OPSYS','wrong ITASK')

         LNAM = VARNAM

         IV = IFINDC (NAMA,IMNV,1,INV,LNAM)

         IF (IV.EQ.0) THEN
*           name not in array, add
            INV = INV+1
            IF (INV.GT.IMNV) CALL ERROR ('OPSYS','too many variables')
            NAMA(INV) = LNAM
            IV = INV
         END IF

         IF (INP(IV).LT.IMNP) THEN
*           add data to specific array
            INP(IV) = INP(IV)+1
            VALA(INP(IV),IV) = VARVAL
         ELSE
            CALL ERROR ('OPSYS','too many data, increase IMNP')
         END IF

      ELSE IF (ITASK.EQ.3) THEN

*        write table to output file
         IF (INV.EQ.0) CALL ERROR ('OPSYS','no data for output file')
         CALL FOPENG (IUNIT,'OP.OUT','NEW','FS',0,'DEL')

         WRITE (IUNIT,'(A,T12,255A12)') 'RUNNUM',(NAMA(I1),I1=1,INV)

*        determine overall maximum number data
         IM = 0
         DO 20 I1=1,INV
            IF (INP(I1).GT.IM) IM = INP(I1)
20       CONTINUE

         DO 30 I1=1,IM
            WRITE (IUNIT,'(I6,T12,255G12.5)') I1,(VALA(I1,I2),I2=1,INV)
30       CONTINUE

         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.4) THEN

*        read table from output file
         CALL FOPENG (IUNIT,'OP.OUT','OLD','FS',0,' ')

         DO 31 I1=1,IMNV
            INP(I1) = 0
            DO 32 I2=1,IMNP
               VALA(I2,I1) = -99.
32          CONTINUE
31       CONTINUE

         CALL GETREC (IUNIT,LINE,EOF)
         CALL WORDS (LINE,IMNV,' ,',IWBEG,IWEND,INV)

*4-jan-94
*jj      changed inv-1 to inv I1+1 to I1 so runnum can be chosen
*        as a plotting variable AND we can use general tabular
*        output files now.

*         INV = INV - 1
         DO 33 I1=1,INV
            NAMA(I1) = LINE(IWBEG(I1):IWEND(I1))
33       CONTINUE

35       IF (.NOT.EOF) THEN
            CALL GETREC (IUNIT,LINE,EOF)
            IF (.NOT.EOF) THEN
*(JJ)               CALL DECREC (LINE,INV+1,HULP)
               CALL DECREC (LINE,INV,HULP)
               DO 36 I1=1,INV
*                  IF (HULP(I1+1).NE.-99.) THEN
                  IF (HULP(I1).NE.-99.) THEN
                     INP(I1) = INP(I1)+1
*                     VALA(INP(I1),I1) = HULP(I1+1)
                     VALA(INP(I1),I1) = HULP(I1)
                 END IF
36             CONTINUE
            END IF
         GOTO 35
         END IF

         CLOSE (IUNIT)
         NODATA = .FALSE.

      ELSE IF (ITASK.EQ.5) THEN

*        create cumulative frequency distribution

*        lookup name in array of names
         LNAM = VARNAM
         IV   = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IF (IV.EQ.0) CALL ERROR ('OPSYS','name not found')
         IF (INP(IV).EQ.0) CALL ERROR ('OPSYS','no data for name')

*        create sorted index array based on values for variable
         CALL INDEXX (INP(IV),VALA(1,IV),INDX)

         I1 = ILEN (VARNAM)
         FILENM = 'CF'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Cumulative frequency distribution',
     &                VARMIN,VARMAX,XLAB,XLAB,VARNAM,
     &                0.,1.,0.2,0.2,' ',
     &                0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 1 1'

         SUM = 0.
         DO 37 I1=1,INP(IV)
            SUM = SUM+VALA(I1,IV)
37       CONTINUE

         TMP1 = 0.
         DO 40 I1=1,INP(IV)
            IF (VALA(INDX(I1),IV).GE.VARMIN.AND.
     &          VALA(INDX(I1),IV).LE.VARMAX) THEN
               TMP1 = TMP1+VALA(INDX(I1),IV)
               WRITE (IUNIT,*) VALA(INDX(I1),IV),REAL(I1)/REAL(INP(IV))
            ELSE
               WRITE (*,'(A,G12.5,A)')
     &          ' Warning: the value ',VALA(INDX(I1),IV),
     &          ' is outside the required bounds.'
            END IF
40       CONTINUE
         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.6) THEN

*        create normal frequency distribution

         LNAM = VARNAM
         IV   = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IF (IV.EQ.0) CALL ERROR ('OPSYS','name not found')
         IF (INP(IV).EQ.0) CALL ERROR ('OPSYS','no data for name')
         IF (INCL.GT.IMNCL) CALL ERROR ('OPSYS','too many classes')

         RDIFF  = (VARMAX-VARMIN)/INCL

         DO 50 I1=1,INCL
            ICL(I1) = 0
50       CONTINUE

         ISUM = 0
         DO 60 I1=1,INP(IV)
            LTMP = .FALSE.
            DO 70 I2=1,INCL
               TMP1 = VARMIN+REAL(I2-1)*RDIFF
               TMP2 = VARMIN+REAL(I2)*RDIFF
               IF (VALA(I1,IV).GE.TMP1.AND.VALA(I1,IV).LT.TMP2) THEN
                  ICL(I2) = ICL(I2)+1
                  ISUM = ISUM+1
                  LTMP = .TRUE.
               END IF
70          CONTINUE
            IF (.NOT.LTMP) THEN
               WRITE (*,'(A,G12.5,A)')
     &          ' Warning: the value ',VALA(I1,IV),
     &          ' is outside the required categories.'
            END IF
60       CONTINUE

         I1 = ILEN (VARNAM)
         FILENM = 'NF'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Frequency distribution',
     &                VARMIN,VARMIN+RDIFF*INCL,RDIFF,RDIFF,VARNAM,
     &                0.,1.,0.2,0.2,' ',
     &                0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 1 0'

         DO 80 I1=1,INCL
            XTMP = VARMIN+(I1-1)*RDIFF
            YTMP = REAL(ICL(I1))/REAL(INP(IV))
            WRITE (IUNIT,*) XTMP,0.
            WRITE (IUNIT,*) XTMP,YTMP
            WRITE (IUNIT,*) XTMP+RDIFF,YTMP
            WRITE (IUNIT,*) XTMP+RDIFF,0.
80       CONTINUE
         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.7) THEN

*        create scatter plot

         LNAM  = VARNAM
         LNAM2 = VAR2NM
         IV  = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IV2 = IFINDC (NAMA,IMNV,1,INV,LNAM2)
         IF (IV.EQ.0.OR.IV2.EQ.0)
     &      CALL ERROR ('OPSYS','name not found')
         IF (INP(IV).EQ.0.OR.INP(IV).EQ.0)
     &      CALL ERROR ('OPSYS','no data for name')

         I1 = ILEN (VARNAM)
         FILENM = 'SC'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Scatter plot',
     &                VARMIN,VARMAX,XLAB,XLAB,VARNAM,
     &                VAR2MN,VAR2MX,VAR2LB,VAR2LB,VAR2NM,
     &                0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 0 1'
         I2 = MIN (INP(IV),INP(IV2))

         DO 90 I1=1,I2
            WRITE (IUNIT,*) VALA(I1,IV),VALA(I1,IV2)
90       CONTINUE

         CLOSE (IUNIT)

      END IF

      ITOLD = ITASK

      RETURN
      END

      SUBROUTINE TTHEAD (IUNIT,FILE,TITLE,
     &                   XMIN,XMAX,XLAB,XTICK,XTXT,
     &                   YMIN,YMAX,YLAB,YTICK,YTXT,
     &                   XLEG,YLEG)
*     formal parameters
      IMPLICIT NONE
      INTEGER IUNIT
      CHARACTER*(*) FILE,TITLE,XTXT,YTXT
      REAL XMIN,XMAX,XLAB,XTICK
      REAL YMIN,YMAX,YLAB,YTICK
      REAL XLEG,YLEG

*     local parameters
      INTEGER IL, ILEN
      SAVE

      CALL FOPENG (IUNIT,FILE,'NEW','FS',0,'DEL')
      IL = MAX (1, ILEN (TITLE))
      WRITE (IUNIT,'(A)') TITLE(1:IL)
      WRITE (IUNIT,'(A,4G12.5,A)') 'X 1 0',XMIN,XMAX,XLAB,XTICK,' 0 0'
      IL = MAX (1, ILEN (XTXT))
      WRITE (IUNIT,'(A)') XTXT(1:IL)
      WRITE (IUNIT,'(A,4G12.5,A)') 'Y 1 0',YMIN,YMAX,YLAB,YTICK,' 0 0'
      IL = MAX (1, ILEN (YTXT))
      WRITE (IUNIT,'(A)') YTXT(1:IL)
      WRITE (IUNIT,'(2G12.5)') XLEG,YLEG

      RETURN
      END

      SUBROUTINE INDEXX (N,ARRIN,INDX)
      IMPLICIT REAL(A-Z)

      INTEGER I,J,N,IR,L
      DIMENSION ARRIN(N)
      INTEGER INDXT,INDX(N)
      SAVE

      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF (L.GT.1) THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF (IR.EQ.1) THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF (J.LE.IR) THEN
          IF (J.LT.IR) THEN
            IF (ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF (Q.LT.ARRIN(INDX(J))) THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

