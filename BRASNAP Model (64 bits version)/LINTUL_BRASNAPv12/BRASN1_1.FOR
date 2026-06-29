*************************************************************************
* LINTUL-BRASNAP 1.1                                                    *
*                                                                       *
* Version with some changes in the subroutine SUBNAP                    *
* after the first test version for Cropsense, UK                        *
* Authors  : B. Habekotte' (AB-DLO)                                     *
* Date     : April 1995                                                 *
* Controle : Daniel van Kraalingen (november, 1996)                     *
*-----------------------------------------------------------------------*
*                                                                       *
* LINTUL-BRASNAP uses the FORTRAN Simulation Environment 2.1a           *
* With observation system                                               *
*                                                                       *
* FSE version 2.1 is described in:                                      *
* Kraalingen, D.W.G., van, 1995. The FSE system for crop simulation,    *
* version 2.1. Quantitative Approaches in Systems Analysis No. 1,       *
* AB-DLO/PE, Wageningen, 70 pp.                                         *
*                                                                       *
* In case of you have trouble with the model, contact:                  *
* DLO-Research Institute for Agrobiology and Soil Fertility (AB-DLO)    *
* Software Product Support                                              *
* P.O. Box 14,                                                          *
* 6700 AA Wageningen, The Netherlands                                   *
* e-mail: sps@ab.dlo.nl                                                *
*************************************************************************

      PROGRAM MAIN
      CALL FSE
      END

*----------------------------------------------------------------------*
* SUBROUTINE MODELS                                                    *
* Authors: Daniel van Kraalingen                                       *
* Date   : 5-Jul-1993                                                  *
* Purpose: This subroutine is the interface routine between the FSE-   *
*          driver and the simulation models. This routine is called    *
*          by the FSE-driver at each new task at each time step. It    *
*          can be used by the user to specify calls to the different   *
*          models that have to be simulated                            *
*                                                                      *
* FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
* name   type meaning                                     units  class *
* ----   ---- -------                                     -----  ----- *
* ITASK   I4  Task that subroutine should perform            -      I  *
* IUNITD  I4  Unit that can be used for input files          -      I  *
* IUNITO  I4  Unit used for output file                      -      I  *
* IUNITL  I4  Unit used for log file                         -      I  *
* FILEIT  C*  Name of input file with timer data             -      I  *
* FILEI1  C*  Name of input file no. 1                       -      I  *
* FILEI2  C*  Name of input file no. 2                       -      I  *
* FILEI3  C*  Name of input file no. 3                       -      I  *
* FILEI4  C*  Name of input file no. 4                       -      I  *
* FILEI5  C*  Name of input file no. 5                       -      I  *
* OUTPUT  L4  Flag to indicate if output should be done      -      I  *
* TERMNL  L4  Flag to indicate if simulation is to stop      -     I/O *
* DOY     R4  Day number within year of simulation (REAL)    d      I  *
* IDOY    I4  Day number within year of simulation (INTEGER) d      I  *
* YEAR    R4  Year of simulation (REAL)                      y      I  *
* IYEAR   I4  Year of simulation (INTEGER)                   y      I  *
* TIME    R4  Time of simulation                             d      I  *
* STTIME  R4  Start time of simulation                       d      I  *
* FINTIM  R4  Finish time of simulation                      d      I  *
* DELT    R4  Time step of integration                       d      I  *
* LAT     R4  Latitude of site                            dec.degr. I  *
* WSTAT   C6  Status code from weather system                -      I  *
* WTRTER  L4  Flag whether weather can be used by model      -      O  *
* RDD     R4  Daily shortwave radiation                   J/m2/d    I  *
* TMMN    R4  Daily minimum temperature                  degrees C  I  *
* TMMX    R4  Daily maximum temperature                  degrees C  I  *
* VP      R4  Early morning vapour pressure                 kPa     I  *
* WN      R4  Average wind speed                            m/s     I  *
* RAIN    R4  Daily amount of rainfall                     mm/d     I  *
*                                                                      *
* Fatal error checks: none                                             *
* Warnings          : none                                             *
* Subprograms called: models as specified by the user                  *
* File usage        : none                                             *
*----------------------------------------------------------------------*

      SUBROUTINE MODELS (ITASK , IUNITD, IUNITO, IUNITL,
     &                   FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5,
     &                   OUTPUT, TERMNL,
     &                   DOY   , IDOY  , YEAR  , IYEAR ,
     &                   TIME  , STTIME, FINTIM, DELT  ,
     &                   LAT   , LONG  , ELEV  , WSTAT , WTRTER,
     &                   RDD   , TMMN  , TMMX  , VP    , WN, RAIN)
      IMPLICIT REAL (A-Z)

*     Formal parameters
      INTEGER   ITASK, IUNITD, IUNITO, IUNITL, IDOY, IYEAR
      CHARACTER FILEIT*(*), FILEI1*(*), FILEI2*(*), FILEI3*(*)
      CHARACTER FILEI4*(*), FILEI5*(*)
      LOGICAL   OUTPUT, TERMNL, WTRTER
      CHARACTER WSTAT*6

*     Local variables
*     <none>
      SAVE

*     Only one model used here
      CALL SUBNAP (ITASK,IUNITD,IUNITO,IUNITL,
     &             FILEI1,OUTPUT,TERMNL,
     &             DOY,YEAR,
     &             STTIME,TIME,DELT,
     &             LAT,WSTAT,WTRTER,
     &             TMMN,TMMX,RDD)

      RETURN
      END

*-----------------------------------------------------------------------*
* Subroutine SUBNAP 1.1                                                 *
*                                                                       *
* Second version with some changes in the subroutine SUBNAP             *
* after the first test version for Cropsense, UK (BRASNAP1.for)         *
* Authors  : B. Habekotte' (AB-DLO)                                     *
* Date     : April 1995                                                 *
*                                                                       *
* SUBNAP calculates crop growth (since early spring) and                *
* phenological development (since sowing) of                            *
* Winter oilseed rape (Brassica napus L.)                               *
*                                                                       *
* SUBNAP 1.1 is described in:                                           *
* Habekotte, 1996. Description, parameterization and user guide of      *
* LINTUL-BRASNAP 1.1, Quantitative Approaches in Systems Analysis No. 9 *
* AB-DLO/PE, Wageningen.                                                *
*-----------------------------------------------------------------------*
*                                                                       *
* changes in subroutine SUBNAP:                                         *
* > 21-07-1995: 'twntm= 0.95*twnm' instead of '=0.9*twnm'               *
*                     = 'aboveground normal dry matter at final harvest'*
*                       (according to data of Habekotte and Smid (1992))*
* > 01-12-1995: initial seed weight has a maximum of 25 g m-2           *
*               this is only necessary for explorations with changes    *
*               in seed density (numbers per m-2)                       *
*-----------------------------------------------------------------------*

       SUBROUTINE SUBNAP (ITASK,IUNITD,IUNITO,IUNITL,
     &   FILEI1,OUTPUT,TERMNL,
     &   DOY,YEAR,
     &   STTIME,TIME,DELT,
     &   LAT,WSTAT,WTRTER,
     &   TMMN,TMMX,RDD)

      IMPLICIT REAL (A-Z)

*-----Formal parameters
      INTEGER   ITASK, IUNITD, IUNITO, IUNITL
      LOGICAL  OUTPUT, TERMNL, WTRTER
      CHARACTER*(*) FILEI1,WSTAT

*-----Standard local declarations
      LOGICAL   INQOBS
      INTEGER   L2, L6
      PARAMETER (L2=25, L6=60)
      INTEGER   IT1,IT3,IT4,IT11,IT12
      INTEGER   IWVAR
      REAL      ROILTB(L2)
      REAL      FLDVS(L2), FSDVS(L2),LUEDVS(L2),PDMSWT(L6)
      PARAMETER (TINY=1.E-4, TINY2=0.1)
      CHARACTER WUSED*6

      SAVE
*     code for the use of RDD, TMMN, TMMX, VP, WN, RAIN  (in that order)
*     a letter 'U' indicates that the variable is Used in calculations
      DATA WUSED/'UUU---'/

*     Check weather data availability
      IF (ITASK.EQ.1.OR.ITASK.EQ.2.OR.ITASK.EQ.4) THEN
         DO 10 IWVAR=1,6
*           is there an error in the IWVAR-th weather variable ?
            IF (WUSED(IWVAR:IWVAR).EQ.'U' .AND.
     &          WSTAT(IWVAR:IWVAR).EQ.'4') THEN
               WTRTER = .TRUE.
               TERMNL = .TRUE.
               RETURN
            END IF
10       CONTINUE
      END IF


      IF (ITASK.EQ.1) THEN

*----------------------------------------------------------------------*
*                    Initialisation of states section                  *
*                    and reading parameters from input file            *
*----------------------------------------------------------------------*

*-----Initialization section
      IF (DELT.LT.1.0) CALL ERROR
     &      ('SUBNAP','DELT too small for SUBNAP')

*-----open parameter and initial conditions input data file

      CALL RDINIT(IUNITD,IUNITL,FILEI1)

*-----reading parameter-values from input file:

      CALL RDSREA ('SWD',SWD )
      CALL RDSREA ('STIM',STIM )
      CALL RDSREA ('TDW',TDW )
      CALL RDSREA ('SLAI',SLAI )

*-----other parameters
      CALL RDSREA ('VF' ,VF )
      CALL RDSREA ('TB1',TB1 )
      CALL RDSREA ('TB2',TB2 )
      CALL RDSREA ('TB3',TB3 )
      CALL RDSREA ('TB4',TB4 )
      CALL RDSREA ('AT1',AT1 )
      CALL RDSREA ('AT2',AT2 )
      CALL RDSREA ('AT3',AT3 )
      CALL RDSREA ('AT4',AT4 )
      CALL RDSREA ('TMIN',TMIN )
      CALL RDSREA ('TOP1',TOP1 )
      CALL RDSREA ('TOP2',TOP2 )
      CALL RDSREA ('TMAX',TMAX )
      CALL RDSREA ('MVR',MVR )
      CALL RDSREA ('DLB',DLB )
      CALL RDSREA ('DLOPT',DLOPT )

*-----reading parameters for growth and seed formation
      CALL RDSREA ('CONVG',CONVG)
      CALL RDSREA ('FPROT',FPROT)
      CALL RDSREA ('FOILM',FOILM)
      CALL RDAREA ('ROILTB',ROILTB,L2, IT1)
      CALL RDSREA ('GO',GO)
      CALL RDSREA ('GP',GP)
      CALL RDSREA ('GC',GC)
      CALL RDAREA ('FLDVS',FLDVS,L2,IT3)
      CALL RDAREA ('FSDVS',FSDVS,L2,IT11)
      CALL RDAREA ('PDMSWT',PDMSWT,L6,IT12)
      CALL RDAREA ('LUEDVS',LUEDVS,L2,IT4)
      CALL RDSREA ('FPDVS',FPDVS)
      CALL RDSREA ('KLAI',KLAI)
      CALL RDSREA ('REFGBF',REFGBF)
      CALL RDSREA ('REFGAF',REFGAF)
      CALL RDSREA ('SPFL',SPFL)
      CALL RDSREA ('DPFL',DPFL)
      CALL RDSREA ('FPER',FPER)
      CALL RDSREA ('PF1',PF1)
      CALL RDSREA ('PF2',PF2)
      CALL RDSREA ('RLAI',RLAI)
      CALL RDSREA ('SLA',SLA)
      CALL RDSREA ('PSPP',PSPP)
      CALL RDSREA ('NP1',NP1)
      CALL RDSREA ('NP2',NP2)
      CALL RDSREA ('NS1',NS1)
      CALL RDSREA ('NS2',NS2)
      CALL RDSREA ('IMSW',IMSW)
      CALL RDSREA ('FRECAR',FRECAR)
      CALL RDSREA ('MDREC',MDREC)

      CLOSE (IUNITD)

*-----Initialization of states
*-----for normal dry weight (g m-2):
      HI2     = 0.
      MSWN    = 0.
      SWNOR   = 0.
      SWNM    = 0.
      TWNOR   = 0.
      TWNM    = 0.

*-----for calculations in green crop units (GCU, g m-2)
*     and other calculations:
      DVSLUE  = 0.
      DOF     = 0.
      DFF     = 0.
      D350    = 0.
      DEF     = 0.
      DOYM    = 0.
      DVS     = 0.
      DVSM    = 0.
      DGRAF   = 1.
      FOIL    = 0.
      GRQST   = 0.
      HI1     = 0.
      LAI     = 0.
      LAIM    = 0.
      LUE     = 0.
      MSWM    = 0.
      NSEED   = 0.
      NPOD    = 0.
      NOFL    = 0.
      NSNP    = 0.
      RECAR   = 0.
      RCALL   = 0.
      RCALL1  = 0.
      RCALL2  = 0.
      RCALL4  = 0.
      RCALL7  = 0.
      RCALL8  = 0.
      RCALL9  = 0.
      RCAL10  = 0.
      ROIL    = 0.
      SEWM    = 0.
      SEEDW   = 0.
      SMSW    = 0.
      TDWM    = 0.
      TDWOF   = 0.
      TDWFF   = 0.
      TDW350  = 0.
      TDWEF   = 0.
      TDWTOP  = 0.
      TSLAI   = 0.
      TSOF    = 0.
      TSSEED  = 0.
      TLAI    = 0.
      TOF     = 0.
      TSEED   = 0.
      WNTM    = 0.


      ELSE IF (ITASK.EQ.2) THEN

*----------------------------------------------------------------------*
*                    Calculation of rates section                      *
*----------------------------------------------------------------------*

*-----subroutine astro computes day-length from latitude and day-no:
      CALL ASTRO (LAT,DOY,DAYL,DAYLP)

*-----daily Temperature (C): mean daily temprature :
      TMG = (TMMX + TMMN)*0.5

*-----daily effective temperature for calculation of temperature
*     sum with Tb=0.and with Tb=0.6870:
      TMM = LIMIT (0.,35.,TMG) - 0.
      TMS = LIMIT (0.6870,35.,TMG) - 0.6870

*-----temperatures with maximum phenological development rate:
      OPT1 = TB1+30.
      OPT2 = TB2+30.
      OPT3 = TB3+30.
      OPT4 = TB4+30.

*-----phenological development rate calculations:
      IF(TIME.GE.SWD)THEN
        DLF = 0.
        DLE = 0.
        IF (DVS.GE.0..AND.DVS.LT.1.) THEN
          TME = LIMIT(TB1,OPT1,TMG) - TB1
          DVR = AMAX1(0.,AT1*TME)
        ELSE IF (DVS.GE.1..AND.DVS.LT.2.) THEN
          TME = LIMIT (TB2,OPT2,TMG) - TB2
          DLE = (LIMIT (DLB,DLOPT,DAYLP)) - DLB
          DLF = DLE*(1./(DLOPT-DLB))
          DVR = MAX (0.,DLF*VF*AT2*TME)
        ELSE IF (DVS.GE.2..AND.DVS.LT.3.) THEN
          TME = LIMIT (TB3,OPT3,TMG) - TB3
          DVR = MAX (0.,AT3*TME)
        ELSE IF (DVS.GE.3.) THEN
          TME = LIMIT (TB4,OPT4,TMG) - TB4
          DVR = MAX (0.,AT4*TME)
        ENDIF
      ELSE
        DVR = 0.
        TME = 0.
        DLE = 0.
        DLF = 0.
      ENDIF

*-----vernalization rates:
      IF (DVS.GE.(1.0-TINY2).AND.DVS.LT.2..AND.VF.LT.1.)THEN
        IF((TMG.LE.TOP2).AND.(TMG.GE.TOP1))THEN
          VR=MVR
        ELSE IF((TMG.GT.TMIN).AND.(TMG.LT.TOP1))THEN
          VR=(TMG-TMIN)*(MVR/(TOP1-TMIN))
        ELSE IF((TMG.GT.TOP2).AND.(TMG.LT.TMAX))THEN
          VR = MVR - (TMG-TOP2)*(MVR/(TMAX-TOP2))
        ELSE IF((TMG.GE.TMAX).OR.(TMG.LE.TMIN))THEN
          VR=0.
        ENDIF
      ELSE
        VR = 0.
      ENDIF

*-----calculation of light absorbtion of the crop:
*-----a: calculation of incomming PAR:
*     RDD in J/m2/d en PAR in MJ/m2/d
      PAR  = RDD*0.5*1.E-6

*-----b: calculation of availability of PAR for the growth of the crop
*     the transmission of PAR is lower than 1.0 during flowering because
*     of reflection and absorption of PAR by the yellow flowers.
*     (note: transfp is only calculated for output)
      IF(TIME.GE.STIM)THEN
        FAI    = 0.
        GRA    = 0.
        GRB    = 0.
        TRANSA = 0.
        TRANSB = 0.
        IF(DVS.LT.2.)THEN
          TRANS  = 1.
          REFLC  = REFGBF
        ELSE IF(DVS.GE.2..AND.DVS.LE.3.)THEN
          FAI    = (DPFL/FPER)*NOFL*SPFL
          GRA    = 0.0183*(1.-EXP(-0.771*FAI))
          GRB    = 0.225*(1.-EXP(-0.506*FAI))
          TRANSA = 0.4*(1.-GRA)*EXP(-0.771*FAI)
          TRANSB = (1.-0.4)*(1.-GRB)*EXP(-0.506*FAI)
          TRANS  = TRANSA + TRANSB
          REFLC  = REFGBF
          IF(DVS.GE.2.5.AND.DVS.LT.2.6)THEN
            TRANFP = TRANS
          ENDIF
        ELSE IF(DVS.GE.3.)THEN
          TRANS  = 1.
          REFLC  = REFGAF
        ENDIF

        IF(DVS.LT.3.5)THEN
          FPAR35 = 0.
        ELSE IF(DVS.GE.3.5.AND.RCALL.EQ.0.)THEN
          FPAR35 = FPAR
          RCALL = 1.
        ENDIF

*-----Near maturity PAR transmitted through the canopy increases
*     with development stage of the crop (fpdvs):
        IF(DVS.LT.3.5)THEN
          FPAR = TRANS*(1.-REFLC)*EXP(-KLAI*LAI)
        ELSE IF(DVS.GE.3.5.AND.RCALL.EQ.1.)THEN
          FPAR = FPAR35 + FPDVS*(DVS-3.5)
        ENDIF

*-----Transmitted PAR to the bottum of the canopy is assumed to be
*     minimal 5 % of incoming PAR:
        IF(FPAR.LT.0.05)THEN
          FPAR = 0.05
        ENDIF

*-----(PAR at the bottom of the canopy):
        PARR = FPAR*PAR

*-----d: absorbed PAR:
        PARA = PAR*TRANS*(1.-REFLC) - PARR

      ELSE
        TRANS  = 0.
        REFLC  = 0.
        FAI    = 0.
        GRA    = 0.
        GRB    = 0.
        TRANSA = 0.
        TRANSB = 0.
        FPAR   = 0.
        FPAR35 = 0.
        PARR   = 0.
        PARA   = 0.
      ENDIF

*-----calculation of daily growth of the crop in green crop units
*     via the lue (GCU; g/m2,day; before timst(iryz) para=0. ):
*     (note: The global approach of crop growth related to absorbed light
*     by the green canopy in the model does not include influence
*     of temperature on crop growth).

*-----a: the lue related to the development stage of the crop:
      IF(TIME.GE.STIM.AND.DOY.LT.STTIME)THEN
        LUEM = LINT (LUEDVS,IT4,DVS)
      ELSE
        LUEM = 0.
      ENDIF

*-----b: calculation of the lue from stim till the value at dvs=2.0:
      IF(TIME.LT.STIM.AND.DOY.GE.STTIME)THEN
        DVSLUE = 0.
      ELSE IF(TIME.GE.STIM.AND.DOY.LT.STTIME.AND.RCALL9.EQ.0.)THEN
        DVSLUE = DVS
        RCALL9 = 1.
      ENDIF

      IF(RCALL9.EQ.0..AND.DVS.LT.2.0)THEN
        ALFA = 0.
        LUE  = 0.
      ELSE IF(RCALL9.EQ.1..AND.DVS.LT.2.0)THEN
        ALFA = (2.6818-1.145)/(2.0 - DVSLUE)
        LUE  = 1.145 + ALFA*(DVS-DVSLUE)
*-----c: calculation of the lue after dvs=2.0:
      ELSE IF(DVS.GE.2.0)THEN
        ALFA = 0.
        LUE  = LUEM
      ENDIF

*-----d: calculation of the daily growth of the crop in GCU (g m-2):
      IF(TIME.GE.STIM.AND.DOY.LT.STTIME)THEN
        DTDW = LUE*PARA
      ELSE
        DTDW = 0.
      ENDIF

*-----calculation of daily increase of leaf area index (LAI)
*     a: calculation of daily increase in temperature sum (tslai)
      IF(TIME.GE.STIM.AND.DOY.LT.STTIME)THEN
        TLAI = TMM
      ELSE
        TLAI = 0.
      ENDIF
*     b: when LAI <= 0.75:
      IF(TIME.GE.STIM.AND.LAI.LE.0.75.AND.DOY.LT.STTIME)THEN
        DLAI = SLAI*RLAI*(EXP(RLAI*TSLAI))*TMM
        FL   = 0.
*     c: when LAI>0.75:
      ELSE IF(TIME.GE.STIM.AND.LAI.GT.0.75.AND.DOY.LT.STTIME)THEN
        FL=LINT(FLDVS,IT3,DVS)
        DLAI=DTDW*FL*SLA
      ELSE
        DLAI = 0.
        FL   = 0.
      ENDIF

*-----calculation of daily increase in temperature sum since
*     onset of flowering to determine the critical period of
*     seed set (350 Cd):
      IF(DVS.GE.2.0)THEN
        TOF = TMM
      ELSE
        TOF = 0.
      ENDIF

*-----calculation of daily potential and actual seed growth
*     in green crop units (g/m2) with
*     the availability of reserves after the end of flowering.
*     the demand for reserves (in green crop units) is positive
*     or negative:

*     a) calculation of daily increase in temperature sum
*     since dvs = 2.333:
      IF(DVS.GE.2.333)THEN
        TSEED = TMM
      ELSE
        TSEED = 0.
      ENDIF

*-----b) potential distribution of total dry matter to the seeds:
      FS=LINT(FSDVS,IT11,DVS)

*-----c) potential total seed growth:
      IF(DVS.GE.3..AND.NSEED.GT.0.)THEN
        PDMSW=LINT(PDMSWT,IT12,DVS)
        PSEEDW = (PDMSW/1000.)*NSEED*TMS
        DEMAND = PSEEDW - DTDW*FS
      ELSE
        PDMSW  = 0.
        PSEEDW = 0.
        DEMAND = 0.
      ENDIF

*-----d) total seed growth after dvs = 3.0 and since the number of seeds
*     is defined after the critical period of seedset;
*     with the use of reserve carbohydrates after DVS = 3.3:
      IF(TIME.GE.STIM.AND.DVS.GE.3..AND.NSEED.GT.0.)THEN
        IF(DVS.LT.3.3.AND.DEMAND.GT.0.)THEN
          DSEEDW = DTDW*FS
        ELSE IF(DVS.LT.3.3.AND.DEMAND.LE.0.)THEN
          DSEEDW = PSEEDW
        ENDIF
        IF(DVS.LT.3.3)THEN
          DRECAR = 0.
        ENDIF
        IF(DVS.GE.3.3)THEN
          IF(DEMAND.LT.0.)THEN
            DRECAR = -DEMAND
            DSEEDW =  PSEEDW
          ELSE IF(DEMAND.GE.0..AND.DEMAND.LE.(MDREC*RECAR))THEN
            DSEEDW = PSEEDW
            DRECAR = -DEMAND
          ELSE IF(DEMAND.GE.0..AND.DEMAND.GT.(MDREC*RECAR))THEN
            DSEEDW = DTDW*FS + MDREC*RECAR
            DRECAR = -MDREC*RECAR
          ELSE IF(DEMAND.GE.0..AND.RECAR.LE.0.)THEN
            DSEEDW = DTDW*FS
            DRECAR = 0.
          ENDIF
        ENDIF
      ELSE
        DSEEDW = 0.
        DRECAR = 0.
      ENDIF

*-----calculation of variable for making graphs with crop variables
*     versus number of days (dgr) since 1-1-year
      IF(DOY.LT.STTIME)THEN
        DGR   = 1.
      ELSE
        DGR   = 0.
      ENDIF

*-----green crop units balance check:
*     All crop parts are given in green crop units. Crop-parts that are
*     integrated are: total crop dry weigth (tdw), seed weight (seedw)
*     and reserve-carbohydrates (recar);
*     after dvs = 3.3, total crop growth is distributed to the seeds with
*     a maximum of the potential growth rate of the seeds. When current
*     crop growth (dtdw) cannot cover the daily growth capacity of the
*     seeds, additional assimilates are drawn from the reserves up to a
*     maximum depletion rate. When current crop growth exeeds the growth
*     capacity of the seeds, the surplus is added to the reserve pool
*     The check-variable (gcudif) has to be equal to 0.0

      IF(DVS.GE.3.3)THEN
        GCUIN  = DSEEDW + DRECAR
        GCUFL  = DTDW
        GCUDIF = (GCUIN - GCUFL)/NOTNUL(GCUIN)
      ELSE
        GCUIN  = 0.
        GCUFL  = 0.
        GCUDIF = 0.
      ENDIF

      IF (OUTPUT) THEN

*-----output of states and rates (weights in g m-2)
      CALL OUTDAT (2,0, 'YEAR '  ,  YEAR)
      CALL OUTDAT (2,0, 'DOY'    ,  DOY)
      CALL OUTDAT (2,0, 'DVS'    ,  DVS )
      CALL OUTDAT (2,0, 'TDW'    ,  TDW)
      CALL OUTDAT (2,0, 'SEEDW'  ,  SEEDW)

        IF (INQOBS (FILEI1,'TDW')) CALL OUTDAT
     &         (2,0,'TDW_OBS',GETOBS (FILEI1,'TDW'))
        IF (INQOBS (FILEI1,'SEEDW')) CALL OUTDAT
     &         (2,0,'SEEDW_OBS',GETOBS (FILEI1,'SEEDW'))
        IF (INQOBS (FILEI1,'LAI')) CALL OUTDAT
     &         (2,0,'LAI_OBS',GETOBS (FILEI1,'LAI'))

      ENDIF

*-----Determine the finish conditions of the simulation

      IF (DVS.GE.4.1) TERMNL = .TRUE.


      ELSE IF (ITASK.EQ.3) THEN

*----------------------------------------------------------------------*
*                    Calculation of states section                     *
*----------------------------------------------------------------------*

*-----calculation of variable for making graphs with crop variables
*     versus number of days since 1-1-year
      DGRAF = INTGRL(DGRAF,DGR,DELT)

*-----integration of the phenological development rate. Initial value
*     of DVS in planap.dat and in rernap.dat
      DVS = INTGRL(DVS,DVR,DELT)
      IF(DVS.GE.(1.0-TINY).AND.RCAL10.EQ.0.)THEN
        DVS    = 1.00
        RCAL10 = 1.
      ENDIF

*-----integration of vernalisation rate between dvs=1 and dvs=2 with
*     a maximum of the vernalisation state of 1.
      VF = LIMIT(0.,1., INTGRL(VF,VR,DELT))

*-----calculation of the temperature sum since the
*     onset of flowering (OF):
      TSOF = INTGRL(TSOF,TOF,DELT)

*-----calculation of the LAI (exponential growth till lai = 0.75)
*     (a: calculation of the temperature sum (tb=0.) since initial
*     growth day in spring (itim); b: calculation of the lai).
*     It is assumed that the fraction of PAR absorbed by the green canopy
*     after transmission through the flower layer, remains constant
*     until near maturity. Therefore, the LAI is kept constant after
*     the maximum value have been reached.

*-----calculation of the LAI:
*     a: calculation of temperature sum since start of regrowth, lai
*     development and integration of crop growth in spring at stim
      TSLAI = INTGRL(TS,TMM,DELT)

*     b: initial condition of lai at stim in spring:
      IF(TIME.LE.STIM.AND.RCALL7.EQ.0.)THEN
        LAI = SLAI
        RCALL7 = 1.
      ENDIF

*     c: calculation of the state of lai after stim in spring:
      LAI=INTGRL(LAI,DLAI,DELT)

*-----integration of daily growth of the crop in green crop units
*     (GCU; g/m2) since start of regrowth in spring at stim
      TDW  = INTGRL(TDW,DTDW,DELT)

*-----calculation of total dry weight of the crop at the
*     onset of flowering (tdwof) and at the end of
*     flowering (tdwef) and the number of flowers
*     (related to tdwof), the number of pods (related to tdwef)
*     and the number of seeds (related to tdw350-tdwof)

      IF(DVS.GE.(2.-TINY).AND.RCALL1.EQ.0.)THEN
        DOF    = DOY
        TDWOF  = TDW
        RCALL1 = 1.
        NOFL   = PF1 + PF2*TDWOF
      ENDIF
      IF(DOY.GE.(DOF+7.).AND.RCALL1.EQ.1..AND.DOY.LT.STTIME)THEN
        TDWFF  = TDW
        RCALL1 = 2.
      ENDIF
      IF(DVS.GE.(3.-TINY).AND.RCALL1.EQ.2.)THEN
        DEF = DOY
        TDWEF = TDW
        RCALL1=3.
        NPOD  = NP1 + NP2*TDWEF
      ENDIF
*-----final characteristics and weights at maturity (GCU, g m-2):
      IF(DVS.GE.(4.-TINY).AND.RCALL1.EQ.3.)THEN
        DOYM  = DOY
        DVSM  = DVS
        LAIM  = LAI
        TDWM  = TDW
        MSWM  = (SEEDW/NSEED)*1000.
        SEWM  = SEEDW
        TDWTOP = 0.95*TDWM
        HI1    = SEWM/TDWTOP
        RCALL1 = 4.
      ENDIF

*-----calculation of seed density (m-2):
      IF(TSOF.GE.350..AND.RCALL8.EQ.0.)THEN
        D350   = DOY
        TDW350 = TDW
        NSEED  = NS1 + NS2*(TDW350-TDWOF)
        RCALL8 = 1.
      ENDIF

      IF(RCALL8.EQ.1..AND.NPOD.GT.0.)THEN
        NSNP = NSEED/NPOD
        IF(NSNP.GT.PSPP)THEN
          NSEED = NPOD*PSPP
          NSNP = NSEED/NPOD
          RCALL8 = 2.0
        ENDIF
      ENDIF

*-----calculation of reserve-carbohydrates in green crop units :
*     a: initial condition just after flowering at dvs = 3.3:
      IF(DVS.GE.3.3.AND.RCALL4.EQ.0.)THEN
        RECAR = FRECAR*TDW
        RCALL4 = 1.
      ENDIF
*     b: the state of reserve-carbohydrates since dvs = 3.3:
      RECAR = INTGRL(RECAR,DRECAR,DELT)

*-----calculation of total seed weight in green crop units (GCU g/m2)
*-----a: calculation of initial total seed weight at dvs >= 3.0:
*        initial seed weight has a maximum of 25 g m-2::
      IF(DVS.GE.3..AND.RCALL2.EQ.0..AND.NSEED.GT.0.)THEN
        SEEDW  = (IMSW/1000.)*NSEED
        IF(SEEDW.GT.25.)THEN
          SEEDW = 25.
        ENDIF
        RCALL2 = 1.
      ENDIF

*-----b: total seed weight and individual seed weight (per 1000)
*     calculated since the number of seeds are defined and since
*     the end of flowering (dvs=3.0):
      SEEDW = INTGRL(SEEDW,DSEEDW,DELT)
      IF(SEEDW.GT.0.)THEN
        SMSW  = (SEEDW/NSEED)*1000.
      ELSE
        SMSW  = 0.
      ENDIF

*-----calculation of normal total dry weight and seed weight (g m-2):
*     (weights of reserve carbohydrates remained in GCU)
*-----a: calculation of the temperature sum (tb=0.) since 1/3 flow. period
      TSSEED = INTGRL(TSSEED,TSEED,DELT)

*-----b: calculation of normal seed weight in relation to oil,
*     protein and carbohydrate content:
      IF(SEEDW.GT.0.0)THEN
        ROIL   = LINT(ROILTB,IT1,TSSEED)
        FOIL   = ROIL*FOILM
        GRQS   = FOIL*GO + FPROT*GP + (1.-FOIL-FPROT)*GC
        SWNOR  = (SEEDW/CONVG)*(1./GRQS)
      ENDIF

*-----c: calculation of normal total dry weight:
      IF(SEEDW.GT.0.0)THEN
        TWNOR  = TDW - SEEDW + SWNOR
      ELSE
        TWNOR  = TDW
      ENDIF

*-----d: calculation of final normal dry weights at maturity (g m-2):
      IF(DVS.GE.(4.-TINY).AND.RCALL1.EQ.4.)THEN
        TWNM   = TWNOR
        SWNM   = SWNOR
        MSWN   = (SWNOR/NSEED)*1000.
        TWNTM  = 0.95*TWNM
        HI2    = SWNM/TWNTM
        RCALL1 = 5.
      ENDIF

      ELSE IF(ITASK.EQ.4)THEN
*----------------------------------------------------------------------*
*                          Terminal section                            *
*----------------------------------------------------------------------*

*-----Define graph for output
*     use individual scale, small plot width for output

*       CALL OUTPLT (1, 'TDW')
*       CALL OUTPLT (1, 'LAI')
*       CALL OUTPLT (1, 'SEEDW')
*       CALL OUTPLT (6, 'Printplot of SUCROS')

*-----Store values of variables at maturity

*-----Calculate day of the year of full flowering (defined
*     as the day of onset of flowering plus seven days.
      DFF   = DOF + 7.

      CALL OPSTOR ('YOM',  YEAR)
      CALL OPSTOR ('DOF',  DOF)
      CALL OPSTOR ('DEF',  DEF)
      CALL OPSTOR ('DOYM', DOYM)
      CALL OPSTOR ('SEWM', SEWM)
      CALL OPSTOR ('TDWM', TDWM)
      CALL OPSTOR ('LAIM', LAIM)
      CALL OPSTOR ('NSEED',NSEED)
      CALL OPSTOR ('MSWM', MSWM)

      ENDIF

      RETURN
      END

*----------------------------------------------------------------------*
*  SUBROUTINE ASTRO                                                    *
*  Simplified version (B.Habekotte)                                    *
*  Authors: Daniel van Kraalingen                                      *
*  Date   : 9-Aug-1987                                                 *
*  Modified by Jan Goudriaan 4 Febr 1988                               *
*  Modified by Jan Goudriaan and Kees Spitters 7 December 1989         *
*  Purpose: This subroutine calculates astronomic daylength            *
*           and photoperiodic daylength                                *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
*  name    meaning                                     units  class    *
*  ----    -------                                     -----  -----    *
*  DOY     Day number (Jan 1st = 1)                      -      I      *
*  LAT     Latitude of the site                       degrees   I      *
*  DAYL    Astronomical daylength (base = 0 degrees)     h      O      *
*  DAYLP   Photoperiodic daylength (base = 0 degrees)    h      O      *
*                                                                      *
*  FATAL ERROR CHECKS (execution terminated, message)                  *
*  condition: LAT > 67, LAT < -67                                      *
*                                                                      *
*  SUBROUTINES and FUNCTIONS called : ERROR                            *
*  FILE usage : none                                                   *
*----------------------------------------------------------------------*

      SUBROUTINE ASTRO (LAT,DOY,DAYL,DAYLP)
      IMPLICIT REAL (A-Z)
      SAVE

*-----PI and conversion factor from degrees to radians
      PARAMETER (PI=3.141592654, RAD=0.017453292)

*-----check on input range of parameters
      IF (LAT.GT.67.)  CALL ERROR ('ASTRO','LAT > 67')
      IF (LAT.LT.-67.) CALL ERROR ('ASTRO','LAT < -67')

*-----declination of the sun as function of daynumber (DAY)
      DEC = -ASIN (SIN (23.45*RAD)*COS (2.*PI*(DOY+10.)/365.))

*-----SINLD, COSLD and AOB are intermediate variables

      SINLD = SIN (RAD*LAT)*SIN (DEC)
      COSLD = COS (RAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD

*-----daylength (DAYL)
      DAYL   = 12.0*(1.+2.*ASIN (AOB)/PI)
      DAYLP  = 12.*(1.+2.*ASIN((-SIN(-4.*RAD)+SINLD)/COSLD)/PI)

      RETURN
      END
