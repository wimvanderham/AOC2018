&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoc2018.p
    Purpose     : Solve Advent of Code puzzles 2018

    Syntax      : run aoc2018.p persistent

    Description : 

    Author(s)   : Wim van der Ham
    Created     : 01-12-2018
    Notes       : Debutto Anthea allo Schiaccianoci @ Tallinn
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFrequency
   FIELD iFrequency   AS INTEGER
   FIELD iCalculation AS INTEGER
INDEX indF IS UNIQUE iFrequency.

DEFINE TEMP-TABLE ttSquare
   FIELD iID         AS INTEGER
   FIELD iX_From     AS INTEGER
   FIELD iY_From     AS INTEGER
   FIELD iWidth      AS INTEGER
   FIELD iHeight     AS INTEGER
   FIELD iX_To       AS INTEGER
   FIELD iY_To       AS INTEGER
   FIELD iMaxOccupy  AS INTEGER
   FIELD iTotalPower AS INTEGER /* Day 11: Fuel Square */
INDEX indID IS UNIQUE iID
INDEX indXY IS PRIMARY iX_From iX_To iY_From iY_To
INDEX indPower iTotalPower DESCENDING.

DEFINE TEMP-TABLE ttOccupy
   FIELD iX        AS INTEGER
   FIELD iY        AS INTEGER
   FIELD iNrOccupy AS INTEGER
INDEX indXY IS PRIMARY UNIQUE iX iY.

DEFINE TEMP-TABLE ttSquareOccupy
   FIELD iX        AS INTEGER
   FIELD iY        AS INTEGER
   FIELD iID       AS INTEGER
INDEX indXYID IS PRIMARY UNIQUE iX iY iID.

/* Day 4 */
DEFINE TEMP-TABLE ttEvent
   FIELD EventDatetime AS DATETIME
   FIELD EventYear     AS INTEGER FORMAT "9999"
   FIELD EventMonth    AS INTEGER FORMAT "99"
   FIELD EventDay      AS INTEGER FORMAT "99"
   FIELD EventHour     AS INTEGER FORMAT "99"
   FIELD EventMinute   AS INTEGER FORMAT "99"
   FIELD Guard         AS INTEGER
   FIELD Asleep        AS LOGICAL
   FIELD Awake         AS LOGICAL
INDEX indEventDatetime IS PRIMARY UNIQUE EventDatetime.

DEFINE TEMP-TABLE ttGuardAsleep
   FIELD SleepGuard    AS INTEGER
   FIELD SleepDatetime AS DATETIME
   FIELD SleepYear   AS INTEGER FORMAT "9999"
   FIELD SleepMonth  AS INTEGER FORMAT "99"
   FIELD SleepDay    AS INTEGER FORMAT "99"
   FIELD SleepHour   AS INTEGER FORMAT "99"
   FIELD SleepMinute AS INTEGER FORMAT "99"
INDEX indSleep IS PRIMARY UNIQUE SleepGuard SleepDatetime.

/* Day 5 */
DEFINE TEMP-TABLE ttChar
   FIELD iPosition AS INTEGER
   FIELD cChar     AS CHARACTER
   FIELD lConsider AS LOGICAL
INDEX indPosition IS UNIQUE PRIMARY iPosition
INDEX indChar cChar.

/* Day 6 */
DEFINE TEMP-TABLE ttPoint
   FIELD iX     AS INTEGER
   FIELD iY     AS INTEGER
   FIELD iNr    AS INTEGER
   FIELD iArea  AS INTEGER
   FIELD iPower AS INTEGER /* Day 11: Power */
INDEX indNr IS UNIQUE iNr
INDEX indXY IS PRIMARY iX iY
INDEX indArea iArea DESCENDING.

DEFINE TEMP-TABLE ttArea
   FIELD iX        AS INTEGER
   FIELD iY        AS INTEGER
   FIELD iNr       AS INTEGER
   FIELD iDistance AS INTEGER
INDEX indXY IS UNIQUE PRIMARY iX iY.

/* Day 7 */
DEFINE TEMP-TABLE ttStep
   FIELD cStep     AS CHARACTER
   FIELD cRequires AS CHARACTER
INDEX indStep     cStep cRequires
INDEX indRequires cRequires cStep.

DEFINE TEMP-TABLE ttConstruction
   FIELD cStep     AS CHARACTER
   FIELD cRequires AS CHARACTER
INDEX indStep     cStep cRequires
INDEX indRequires cRequires cStep.

DEFINE TEMP-TABLE ttWorker
   FIELD iWorker  AS INTEGER
   FIELD lIdle    AS LOGICAL
   FIELD cStep    AS CHARACTER
   FIELD iFinish  AS INTEGER
   FIELD iCurrent AS INTEGER
INDEX indWorker IS UNIQUE iWorker.

DEFINE TEMP-TABLE ttSecond
   FIELD iSecond AS INTEGER
   FIELD iWorker AS INTEGER
   FIELD cStep   AS CHARACTER
INDEX indUnique IS UNIQUE iSecond iWorker.

/* Day 8 */
DEFINE TEMP-TABLE ttNode
   FIELD iID AS INTEGER
   FIELD iNrChildNodes   AS INTEGER
   FIELD iNrChildCreated AS INTEGER
   FIELD iNrMetaData     AS INTEGER
   FIELD cMetaData       AS CHARACTER
   FIELD iSumMetaData    AS INTEGER
   FIELD iID_Parent      AS INTEGER
   FIELD iChildNr        AS INTEGER
   FIELD iValue          AS INTEGER
INDEX indID IS UNIQUE iID
INDEX indIDParent IS UNIQUE iID_Parent iID.

/* Day 9 */
DEFINE TEMP-TABLE ttMarble
   FIELD iID AS INTEGER
   FIELD iID_Prev AS INTEGER
   FIELD iID_Next AS INTEGER
   FIELD iValue   AS INTEGER
INDEX indID   IS UNIQUE iID
INDEX indPrev iID_Prev
INDEX indNext iID_Next.

DEFINE TEMP-TABLE ttPlayer
   FIELD iPlayerID AS INTEGER
   FIELD iScore    AS INT64
INDEX indID IS UNIQUE iPlayerID
INDEX indScore iScore.

/* Day 10 */
DEFINE TEMP-TABLE ttPoint9
   FIELD iID     AS INTEGER
   FIELD iStartX AS INTEGER
   FIELD iStartY AS INTEGER
   FIELD iVx     AS INTEGER
   FIELD iVy     AS INTEGER
   FIELD iSecond AS INTEGER
   FIELD iX      AS INTEGER
   FIELD iY      AS INTEGER
INDEX indID iID
INDEX indXY iX iY.

/* Day 12 */
DEFINE TEMP-TABLE ttPot 
   FIELD iPotNr      AS INTEGER
   FIELD iGeneration AS INT64
   FIELD cPlant      AS CHARACTER
INDEX indPotNr IS UNIQUE iPotNr iGeneration
INDEX indGen   iGeneration iPotNr.

DEFINE TEMP-TABLE ttPattern
   FIELD cPatternIn AS CHARACTER
   FIELD cPlant     AS CHARACTER
INDEX indPattern IS UNIQUE cPatternIn.

/* Day 13 */
DEFINE TEMP-TABLE ttTrack
   FIELD iX      AS INTEGER
   FIELD iY      AS INTEGER
   FIELD cTrack  AS CHARACTER
INDEX indXY IS UNIQUE iX iY.

DEFINE TEMP-TABLE ttCart
   FIELD iID        AS INTEGER
   FIELD iTick      AS INTEGER
   FIELD iX         AS INTEGER
   FIELD iY         AS INTEGER
   FIELD cDirection AS CHARACTER
   FIELD cLastCross AS CHARACTER
INDEX indID   IS UNIQUE iID
INDEX indXY   iX iY
INDEX indTick IS PRIMARY iTick iY iX.

/* Day 14 */
DEFINE TEMP-TABLE ttRecipe
   FIELD iIndex  AS INTEGER
   FIELD iRecipe AS INTEGER
INDEX indIndex IS UNIQUE iIndex.

/* Day 15 */
DEFINE TEMP-TABLE ttBoard
   FIELD iRound AS INTEGER
   FIELD iX     AS INTEGER
   FIELD iY     AS INTEGER
   FIELD cType  AS CHARACTER /* #=Wall, .=Empty, E=Elf, G=Goblin */
INDEX indXY IS UNIQUE PRIMARY iY iX.

DEFINE TEMP-TABLE ttUnit
   FIELD iID        AS INTEGER
   FIELD iRound     AS INTEGER
   FIELD iX         AS INTEGER
   FIELD iY         AS INTEGER
   FIELD cType      AS CHARACTER
   FIELD iPower     AS INTEGER
   FIELD iHitPoints AS INTEGER
INDEX indID IS UNIQUE iID
INDEX indXY IS PRIMARY iY iX.

DEFINE TEMP-TABLE ttPath
   FIELD iID_From  AS INTEGER
   FIELD iID_To    AS INTEGER
   FIELD iX_Start  AS INTEGER
   FIELD iY_Start  AS INTEGER
   FIELD iX_From   AS INTEGER
   FIELD iY_From   AS INTEGER
   FIELD iX_To     AS INTEGER
   FIELD iY_To     AS INTEGER
   FIELD iDistance AS INTEGER
INDEX indIDs      IS UNIQUE iID_From iID_To
INDEX indNearest  IS PRIMARY iID_From iDistance iY_To iX_To
INDEX indXY       iX_To iY_To iDistance
INDEX indDistance iDistance iY_To iX_To.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getNodeValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeValue Procedure 
FUNCTION getNodeValue RETURNS INTEGER
(  /* parameter-definitions */ 
   INPUT ipiID AS INTEGER
)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPatternIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPatternIn Procedure 
FUNCTION getPatternIn RETURNS CHARACTER
(  /* parameter-definitions */ 
   INPUT ipiGeneration AS INTEGER,
   INPUT ipiPotNr      AS INTEGER
)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 22.86
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calcFrequency) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcFrequency Procedure 
PROCEDURE calcFrequency :
/*------------------------------------------------------------------------------
  Purpose:     Calculate Frequency
  Parameters:  string of signs and numbers
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE cTerm      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTerm      AS INTEGER     NO-UNDO.
DEFINE VARIABLE lPositive  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iFrequency AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumber    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLoop      AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttFrequency.

   ASSIGN
      iLoop      = 1
      iFrequency = 0
   .

   MainBlock:
   REPEAT:

      LoopBlock:
      DO iTerm = 1 TO NUM-ENTRIES(ipcInput, "~n"):
         cTerm = ENTRY(iTerm, ipcInput, "~n").
         lPositive = SUBSTRING(cTerm, 1, 1) EQ "+".
         ASSIGN
            iNumber = INTEGER(SUBSTRING(cTerm, 2))
         NO-ERROR.
         IF lPositive EQ FALSE THEN DO:
            iNumber = -1 * iNumber.
         END.
         iFrequency = iFrequency + iNumber.
   
         FIND  ttFrequency
         WHERE ttFrequency.iFrequency = iFrequency NO-ERROR.
   
/*          PUBLISH "debug" (SUBSTITUTE("&1.&2. &3 &4 &5", iLoop, iTerm, cTerm, iFrequency, AVAILABLE ttFrequency)). */
   
         IF NOT AVAILABLE ttFrequency THEN DO:
            CREATE ttFrequency.
            ASSIGN
               ttFrequency.iFrequency   = iFrequency
               ttFrequency.iCalculation = iTerm
            .
         END.
         ELSE DO:
            IF opiOutput2 = 0 THEN DO:
               opiOutput2 = iFrequency.
               IF iLoop GT 1 THEN DO:
                  LEAVE LoopBlock.
               END.
            END.
         END.
      END.

      IF opiOutput2 NE 0 THEN DO:
         LEAVE MainBlock.
      END.

      IF iLoop = 1 THEN DO:
         ASSIGN
            opiOutput = iFrequency
         .
      END.
      iLoop = iLoop + 1.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createImage Procedure 
PROCEDURE createImage :
/*------------------------------------------------------------------------------
  Purpose:     Creates an image from an ASCII text file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInputFile AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiWidth     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiHeight    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiFontSize  AS INTEGER     NO-UNDO.

DEFINE VARIABLE cCommand   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImageFile AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cSize   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRotate AS CHARACTER   NO-UNDO.

   ASSIGN
      cImageFile = SUBSTITUTE("&1.png", 
                              SUBSTRING(ipcInputFile, 1, R-INDEX(ipcInputFile, ".") - 1))
   .

   IF ipiWidth LT ipiHeight THEN DO:
      cRotate = "90x90".
      cSize   = SUBSTITUTE("&1x&2",
                           ipiHeight,
                           ipiWidth).
   END.
   ELSE DO:
      cSize = SUBSTITUTE("&1x&2",
                         ipiWidth,
                         ipiHeight).
   END.

   cCommand = SUBSTITUTE('convert -size &1 xc:white -gravity center -font "Courier-New" -pointsize &2 -fill black -annotate &5+0+0 "@&3" "&4"',
                         cSize,
                         ipiFontSize,
                         ipcInputFile,
                         cImageFile,
                         cRotate).

   PUBLISH "nodebug" (SUBSTITUTE("&1", cCommand)).

   OS-COMMAND SILENT VALUE(cCommand).

   PUBLISH "debug" (SUBSTITUTE("IMAGE:&1", cImageFile)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMovie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createMovie Procedure 
PROCEDURE createMovie :
/*------------------------------------------------------------------------------
  Purpose:     Creates a movie of images
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInputFile AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCommand AS CHARACTER   NO-UNDO.

   cCommand = SUBSTITUTE("C:\Users\Wim\Downloads>ffmpeg\bin\ffmpeg.exe -framerate 24 -loop 1 -t 10 -i inputAOC2018_13_Example_%06d.png output.mp4").

   OS-COMMAND SILENT VALUE(cCommand).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getArea Procedure 
PROCEDURE getArea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxDistance AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput      AS INTEGER     NO-UNDO.

DEFINE VARIABLE lcInput      AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE iCoordinate  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cCoordinates AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNr          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinX        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxX        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinY        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxY        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iArea        AS INTEGER     NO-UNDO.
DEFINE VARIABLE lDebug       AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttPrevPoint FOR ttPoint.

   EMPTY TEMP-TABLE ttPoint.
   EMPTY TEMP-TABLE ttArea.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
      lDebug = FALSE.
   END.
   ELSE DO:
      lcInput = ipcInput.
      lDebug = TRUE.
   END.

   DO iCoordinate = 1 TO NUM-ENTRIES(lcInput, "~n"):
      ASSIGN
         cCoordinates = ENTRY(iCoordinate, lcInput, "~n")
      .

      IF TRIM (cCoordinates) NE "" THEN DO:
         iNr = iNr + 1.
         CREATE ttPoint.
         ASSIGN
            ttPoint.iNr = iNr
         .
         ASSIGN
            ttPoint.iX = INTEGER (TRIM(ENTRY(1, cCoordinates)))
            ttPoint.iY = INTEGER (TRIM(ENTRY(2, cCoordinates)))
         NO-ERROR.

         /* Min/Max X */
         IF iMinX EQ 0
         OR ttPoint.iX LT iMinX THEN DO:
            iMinX = ttPoint.iX.
         END.
         IF iMaxX EQ 0
         OR ttPoint.iX GT iMaxX THEN DO:
            iMaxX = ttPoint.iX.
         END.

         /* Min/Max Y */
         IF iMinY EQ 0
         OR ttPoint.iY LT iMinY THEN DO:
            iMinY = ttPoint.iY.
         END.
         IF iMaxY EQ 0
         OR ttPoint.iY GT iMaxY THEN DO:
            iMaxY = ttPoint.iY.
         END.
      END.
   END.
   
   XBlock:
   DO iX = iMinX TO iMaxX:
      YBlock:
      DO iY = iMinY TO iMaxY:
         CREATE ttArea.
         ASSIGN
            ttArea.iX        = iX
            ttArea.iY        = iY
         .

         IF ipiPart EQ 1 THEN DO:
            CoordinateBlock:
            FOR EACH ttPoint
            BY ABS(ttPoint.iX - ttArea.iX) + ABS(ttPoint.iY - ttArea.iY):
               ACCUM "" (COUNT).
               IF (ACCUM COUNT "") EQ 1 THEN DO:
                  ASSIGN
                     ttArea.iNr       = ttPoint.iNr
                     ttArea.iDistance = ABS(ttPoint.iX - ttArea.iX) + ABS(ttPoint.iY - ttArea.iY)
                  .
                  ASSIGN
                     ttPoint.iArea = ttPoint.iArea + 1
                  .
               END.
               ELSE DO:
                  IF ttArea.iDistance EQ ABS(ttPoint.iX - ttArea.iX) + ABS(ttPoint.iY - ttArea.iY) THEN DO:
                     /* More then one Point at same distance */
                     FIND  ttPrevPoint 
                     WHERE ttPrevPoint.iNr = ttArea.iNr.
                     IF AVAILABLE ttPrevPoint THEN DO:
                        ASSIGN
                           ttPrevPoint.iArea = ttPrevPoint.iArea - 1
                        .
                     END.
                     ASSIGN
                        ttArea.iNr       = 0
                        ttArea.iDistance = 0
                     .
                  END.
                  LEAVE CoordinateBlock.
               END.
            END. /* CoordinateBlock: */

            FIND  ttPoint 
            WHERE ttPoint.iNr EQ ttArea.iNr NO-ERROR.
            IF AVAILABLE ttPoint THEN DO:
               iArea = ttPoint.iArea.
            END.
            ELSE DO:
               iArea = 0.
            END.

            IF lDebug THEN DO:
               PUBLISH "debug" (SUBSTITUTE("(&1, &2): Point: &3 (&4) Distance: &5 Area: &6", iX, iY, ttArea.iNr, CHR(ASC("A") - 1 + ttArea.iNr), ttArea.iDistance, iArea)).
            END.
            
         END. /* Part 1 */
         ELSE DO:
            /* Part 2 */
            FOR EACH ttPoint:
               IF lDebug THEN DO:
                  PUBLISH "debug" (SUBSTITUTE("Distance from (&1,&2) to Point (&3, &4): &5", ttArea.iX, ttArea.iY, ttPoint.iX, ttPoint.iY, ABS(ttArea.iX - ttPoint.iX) + ABS(ttArea.iY - ttPoint.iY))).
               END.
               ASSIGN
                  ttArea.iDistance = ttArea.iDistance + ABS(ttArea.iX - ttPoint.iX) + ABS(ttArea.iY - ttPoint.iY)
               .
               IF ttArea.iDistance GE ipiMaxDistance THEN DO:
                  IF lDebug THEN DO:
                     PUBLISH "debug" (SUBSTITUTE("Within? &1 LT &2: &3 ", ttArea.iDistance, ipiMaxDistance, ttArea.iDistance LT ipiMaxDistance)).
                  END.

                  NEXT YBlock.
               END.
            END.
            
            IF lDebug THEN DO:
               PUBLISH "debug" (SUBSTITUTE("Within? &1 LT &2: &3 ", ttArea.iDistance, ipiMaxDistance, ttArea.iDistance LT ipiMaxDistance)).
            END.

         END. /* Part 2 */
      END. /* DO iY = iMinY TO iMaxY: */
   END. /* DO iX = iMinX TO iMaxX: */

   IF ipiPart EQ 1 THEN DO:
      /* Part 1 */
      FOR EACH ttArea
      WHERE (ttArea.iX EQ iMinX OR ttArea.iX EQ iMaxX OR ttArea.iY EQ iMinY OR ttArea.iY EQ iMaxY)
      AND   ttArea.iNr GT 0:
         /* Exclude Points linked to Borders */
         FIND  ttPoint
         WHERE ttPoint.iNr   EQ ttArea.iNr
         AND   ttPoint.iArea GT 0 NO-ERROR.
         IF AVAILABLE ttPoint THEN DO:
            ASSIGN
               ttPoint.iArea = -1 * ttPoint.iArea
            .
            IF lDebug THEN DO:
               PUBLISH "debug" (SUBSTITUTE("(&1, &2): Border Exclude point: (&3, &4) &5 (&6) Area: &7", ttArea.iX, ttArea.iY, ttPoint.iX, ttPoint.iY, ttPoint.iNr, CHR(ASC("A") - 1 + ttPoint.iNr), ttPoint.iArea)).
            END.

         END.
      END. /* Borders */

      FOR EACH ttPoint
      BY ttPoint.iArea DESCENDING:
         IF lDebug THEN DO:
            PUBLISH "debug" (SUBSTITUTE("Point (&1,&2) &3 (&4), Area: &5", ttPoint.iX, ttPoint.iY, ttPoint.iNr, CHR(ASC("A") - 1 + ttPoint.iNr), ttPoint.iArea)).
         END.
         IF opiOutput EQ 0 THEN DO:
            ASSIGN
               opiOutput = ttPoint.iArea
            .
         END.
      END.
   END. /* Part 1 */
   ELSE DO:
      /* Part 2 */
      FOR EACH ttArea
      WHERE ttArea.iDistance LT ipiMaxDistance:
         ACCUM "" (COUNT).
         IF lDebug THEN DO:
            PUBLISH "debug" (SUBSTITUTE("#&1 (&2, &3) Distance: &4", (ACCUM COUNT ""), ttArea.iX, ttArea.iY, ttArea.iDistance)).
         END.
      END.
      ASSIGN
         opiOutput = (ACCUM COUNT "")
      .
   END. /* Part 2 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCheckSum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCheckSum Procedure 
PROCEDURE getCheckSum :
/*------------------------------------------------------------------------------
  Purpose:     Calculate check sum as multiplication of two's and three's
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiCheckSum AS INTEGER     NO-UNDO.

DEFINE VARIABLE iTwos   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iThrees AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBox    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cBox    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCharList  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCountList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEntry     AS INTEGER     NO-UNDO.
DEFINE VARIABLE lTwos      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lThrees    AS LOGICAL     NO-UNDO.

   DO iBox = 1 TO NUM-ENTRIES(ipcInput, "~n"):
      cBox = ENTRY(iBox, ipcInput, "~n").

      RUN getCounts
         (INPUT  cBox,
          OUTPUT cCharList,
          OUTPUT cCountList).

      ASSIGN
         lTwos   = FALSE
         lThrees = FALSE
      .

      CountBlock:
      DO iEntry = 1 TO NUM-ENTRIES(cCountList):
         IF lTwos EQ FALSE AND ENTRY(iEntry, cCountList) EQ "2" THEN DO:
            iTwos = iTwos + 1.
            lTwos = TRUE.
         END.
         IF lThrees EQ FALSE AND ENTRY(iEntry, cCountList) EQ "3" THEN DO:
            iThrees = iThrees + 1.
            lThrees = TRUE.
         END.
      END.

/*       PUBLISH "debug" (SUBSTITUTE("&1 &2 &3 &4 &5", cBox, cCharList, cCountList, iTwos, iThrees)). */

   END.

   ASSIGN
      opiCheckSum = iTwos * iThrees
   .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCounts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCounts Procedure 
PROCEDURE getCounts :
/*------------------------------------------------------------------------------
  Purpose:     Returns counts per character
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcChars  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcCounts AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

   DO iChar = 1 TO LENGTH(ipcInput):
      cChar = SUBSTRING(ipcInput, iChar, 1).

      IF LOOKUP(cChar, opcChars) EQ 0 THEN DO:
         opcChars = opcChars +
            (IF opcChars = "" THEN "" ELSE ",") +
            cChar.
         opcCounts = opcCounts +
            (IF opcCounts = "" THEN "" ELSE ",") +
            "1".
      END.
      ELSE DO:
         ENTRY(LOOKUP(cChar, opcChars), opcCounts) = 
            STRING (INTEGER(ENTRY(LOOKUP(cChar, opcChars), opcCounts)) + 1).
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDay11) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDay11 Procedure 
PROCEDURE getDay11 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput  AS INT64       NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput2 AS INT64       NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput3 AS INT64       NO-UNDO.

DEFINE VARIABLE lcInput       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iSerialNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxX         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxY         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNewID        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNewNr        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPower        AS INTEGER     NO-UNDO.

DEFINE VARIABLE iWidth        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCalcX        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCalcY        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxPower     AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttSquare.
   EMPTY TEMP-TABLE ttPoint.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   EMPTY TEMP-TABLE ttSquare.

   ASSIGN
      iSerialNumber = INTEGER(lcInput)
   .

   ASSIGN
      iMaxX = 300
      iMaxY = iMaxX
   .

   IF ipiPart EQ 1 THEN DO:
      iWidth = 3.
      
   END.
   ELSE DO:
      iWidth = 1.
   END.

   CreateSquares:
   DO iX = 1 TO iMaxX - iWidth + 1:
      PUBLISH "debug" (SUBSTITUTE("Part &1: X=&2", ipiPart, iX)).

      DO iY = 1 TO iMaxY - iWidth + 1:
         iNewID = iNewID + 1.
         CREATE ttSquare.
         ASSIGN
            ttSquare.iID         = iNewID
            ttSquare.iX_From     = iX
            ttSquare.iY_From     = iY
            ttSquare.iWidth      = iWidth
            ttSquare.iHeight     = iWidth
            ttSquare.iX_To       = ttSquare.iX_From + ttSquare.iWidth  - 1
            ttSquare.iY_To       = ttSquare.iY_From + ttSquare.iHeight - 1
         .

         DO iCalcX = ttSquare.iX_From TO ttSquare.iX_To:
            DO iCalcY = ttSquare.iY_From TO ttSquare.iY_To:
               FIND  ttPoint
               WHERE ttPoint.iX     = iCalcX
               AND   ttPoint.iY     = iCalcY NO-ERROR.
               IF NOT AVAILABLE ttPoint THEN DO:
                  iNewNr = iNewNr + 1.
                  CREATE ttPoint.
                  ASSIGN
                     ttPoint.iNr    = iNewNr
                     ttPoint.iX     = iCalcX
                     ttPoint.iY     = iCalcY
                  .
                  ASSIGN
                     iPower = (ttPoint.iX + 10) * ttPoint.iY
                     iPower = iPower + iSerialnumber
                     iPower = iPower * (ttPoint.iX + 10)
                     iPower = INTEGER (TRUNCATE(iPower / 100, 0)) MOD 10
                     iPower = iPower - 5
                  .

                  ASSIGN
                     ttPoint.iPower = iPower
                  .
               END.
               ASSIGN
                  ttSquare.iTotalPower = ttSquare.iTotalPower + ttPoint.iPower
               .
            END.
         END.
      END.
   END.

   IF ipiPart EQ 1 THEN DO:
      FOR EACH ttSquare 
      BY ttSquare.iTotalPower DESCENDING:
         ASSIGN
            opiOutput  = ttSquare.iX_From
            opiOutput2 = ttSquare.iY_From
            opiOutput3 = 3
         .
         LEAVE.
      END.
   END.
   ELSE DO:
      /* Search highest Total Power */
      DO iCalcX = 1 TO iMaxX:
         PUBLISH "debug" (SUBSTITUTE("CalcX: &1", iCalcX)).
         DO iCalcY = 1 TO iMaxY:
            DO iWidth = 1 TO iMaxX - iCalcX:
               FOR EACH ttPoint
               WHERE ttPoint.iX GE iCalcX
               AND   ttPoint.iX LE iCalcX + iWidth - 1
               AND   ttPoint.iY GE iCalcY
               AND   ttPoint.iY LE iCalcY + iWidth - 1:
                  ACCUM ttPoint.iPower (TOTAL).
               END.
               IF (ACCUM TOTAL ttPoint.iPower) GT iMaxPower THEN DO:
                  ASSIGN
                     opiOutput  = iCalcX
                     opiOutput2 = iCalcY
                     opiOutput3 = iWidth
                     iMaxPower  = (ACCUM TOTAL ttPoint.iPower)
                  .
                  PUBLISH "debug" (SUBSTITUTE("New Maximum: (&1,&2) x &3: &4", iCalcX, iCalcY, iWidth, iMaxPower)).
                  iNewID = iNewID + 1.
                  CREATE ttSquare.
                  ASSIGN
                     ttSquare.iID         = iNewID
                     ttSquare.iX_From     = iCalcX
                     ttSquare.iY_From     = iCalcY
                     ttSquare.iWidth      = iWidth
                     ttSquare.iHeight     = iWidth
                     ttSquare.iX_To       = ttSquare.iX_From + ttSquare.iWidth - 1
                     ttSquare.iY_To       = ttSquare.iY_From + ttSquare.iHeight - 1
                     ttSquare.iTotalPower = iMaxPower
                  .
               END.
            END.
         END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDay12) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDay12 Procedure 
PROCEDURE getDay12 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput AS INT64       NO-UNDO.

DEFINE VARIABLE lcInput          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInitial         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iChar            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPotNr           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDebug           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPatternIn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iGeneration      AS INT64       NO-UNDO.
DEFINE VARIABLE iMaxGeneration   AS INT64       NO-UNDO.
DEFINE VARIABLE iLastGeneration  AS INT64       NO-UNDO.
DEFINE VARIABLE dtLastDatetime   AS DATETIME    NO-UNDO.
DEFINE VARIABLE iMinutes         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGenerations     AS INT64       NO-UNDO.
DEFINE VARIABLE dtFinishDatetime AS DATETIME    NO-UNDO.
DEFINE VARIABLE cString          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNextString      AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttPot     FOR ttPot.
DEFINE BUFFER ttNextPot FOR ttPot.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   EMPTY TEMP-TABLE ttPot.
   EMPTY TEMP-TABLE ttPattern.

   DO iLine = 1 TO NUM-ENTRIES(lcInput, "~n"):
      cLine = ENTRY(iLine, lcInput, "~n").

      IF cLine BEGINS "initial state:" THEN DO:
         ASSIGN
            cSection = "Init"
         .
         DO iPotNr = -2 TO 20:
            CREATE ttPot.
            ASSIGN
               ttPot.iPotNr      = iPotNr
               ttPot.iGeneration = 0
            .
         END.
      END.
      ELSE DO:
         IF  TRIM (cLine) EQ ""
         AND cSection EQ "Init" THEN DO:
            cSection = "InitEnd".
         END.
         ELSE DO:
            IF  cLine NE ""
            AND cSection = "InitEnd" THEN DO:
               cSection = "Pattern".
            END.
         END.
      END.

      CASE cSection:
         WHEN "Init" THEN DO:
            cInitial = TRIM(ENTRY(2, cLine, ":")).
            DO iChar = 1 TO LENGTH(cInitial):
               cChar = SUBSTRING(cInitial, iChar, 1).
               FIND  ttPot
               WHERE ttPot.iGeneration EQ 0
               AND   ttPot.iPotNr      EQ iChar - 1 NO-ERROR.
               IF NOT AVAILABLE ttPot THEN DO:
                  CREATE ttPot.
                  ASSIGN
                     ttPot.iPotNr      = iChar - 1
                     ttPot.iGeneration = 0
                  .
               END.
               ASSIGN
                  ttPot.cPlant = cChar
               .
            END.
         END.
         WHEN "Pattern" THEN DO:
            CREATE ttPattern.
            ASSIGN
               ttPattern.cPatternIn = TRIM (ENTRY(1, cLine, " "))
               ttPattern.cPlant     = TRIM (ENTRY(2, cLine, ">"))
            .
         END.
      END CASE.
   END.

   IF ipiPart EQ 1 THEN DO:
      iMaxGeneration = 20.
   END.
   ELSE DO:
      iMaxGeneration = 50000000000.      
   END.

   GenerationBlock:
   DO iGeneration = 0 TO iMaxGeneration - 1:
      cDebug = SUBSTITUTE("&1: ", iGeneration).

      FOR EACH ttPot
      WHERE ttPot.iGeneration EQ iGeneration:
         ACCUM ttPot.iPotNr (MAXIMUM).
      END.

      /* Extend two Pots to the right */
      CREATE ttPot.
      ASSIGN
         ttPot.iGeneration = iGeneration
         ttPot.iPotNr      = (ACCUM MAXIMUM ttPot.iPotNr) + 1
      .
      CREATE ttPot.
      ASSIGN
         ttPot.iGeneration = iGeneration
         ttPot.iPotNr      = (ACCUM MAXIMUM ttPot.iPotNr) + 2
      .

      ASSIGN
         cString     = ""
         cNextString = ""
      .
      FOR EACH ttPot
      WHERE ttPot.iGeneration EQ iGeneration
      AND   ttPot.iPotNr      LE (ACCUM MAXIMUM ttPot.iPotNr) + 2:
         cString = SUBSTITUTE("&1&2", cString, ttPot.cPlant).
         cDebug  = SUBSTITUTE("&1&2", cDebug, ttPot.cPlant).
         
         CREATE ttNextPot.
         ASSIGN
            ttNextPot.iGeneration = ttPot.iGeneration + 1
            ttNextPot.iPotNr      = ttPot.iPotNr
         .
         cPatternIn = getPatternIn(ttPot.iGeneration, ttPot.iPotNr).
         FIND  ttPattern
         WHERE ttPattern.cPatternIn EQ cPatternIn NO-ERROR.
         IF AVAILABLE ttPattern THEN DO:
            ASSIGN
               ttNextPot.cPlant = ttPattern.cPlant
            .
         END.
         ELSE DO:
            ASSIGN
               ttNextPot.cPlant = "."
            .
         END.
         cNextString = SUBSTITUTE("&1&2", cNextString, ttNextPot.cPlant).
      END.
      IF iGeneration EQ 0
      OR iGeneration LE 20
      OR iGeneration MOD 100 EQ 0 THEN DO:
         PUBLISH "debug" (cString).

         IF iLastGeneration NE 0 THEN DO:
            ASSIGN
               iMinutes        = INTERVAL(NOW, dtLastDatetime, "minutes")
               iGenerations    = iGeneration - iLastGeneration
            .
            cDebug = SUBSTITUTE("NOW: &1 Velocity: &2 generations in &3 minutes.", NOW, iGenerations, iMinutes).
            iMinutes = (iMaxGeneration - 1) / (iGenerations / iMinutes).
            dtFinishDatetime = ADD-INTERVAL (NOW, iMinutes, "minutes").
            cDebug = SUBSTITUTE("&1~nExpected Finish Datetime: &2.",
                                cDebug,
                                dtFinishDatetime).

         END.
         /*
         ASSIGN
            iLastGeneration = iGeneration
            dtLastDatetime  = NOW
         .
         PUBLISH "debug" (cDebug).
         */
      END.
      
      IF TRIM(cString, ".") EQ TRIM(cNextString, ".") THEN DO:
         /* Data starts to repeat */
         PUBLISH "debug" (SUBSTITUTE("&1: &2", iGeneration, cString)).
         PUBLISH "debug" (SUBSTITUTE("&1: &2", iGeneration + 1, cNextString)).
         PUBLISH "debug" (SUBSTITUTE("Repeat started: &1", iGeneration)).
         FOR EACH ttPot 
         WHERE ttPot.iGeneration EQ iGeneration
         AND   ttPot.cPlant      EQ "#":
            ACCUM "Plant" (COUNT).
            ACCUM ttPot.iPotNr (TOTAL).
         END.
         PUBLISH "debug" (SUBSTITUTE("(ACCUM COUNT Plant): &1 Total: &2",
                                     (ACCUM COUNT "Plant"),
                                     (ACCUM TOTAL ttPot.iPotNr))).
         ASSIGN
            opiOutput = 
               /* Total so far */
               (ACCUM TOTAL ttPot.iPotNr) +
               /* Total until the Max Generation */
               (iMaxGeneration - iGeneration) * (ACCUM COUNT "Plant")
         .
         PUBLISH "debug" (SUBSTITUTE("&1", opiOutput)).

         LEAVE GenerationBlock.
      END. /* Data starts to repeat */
/*       PUBLISH "debug" (cDebug). */
   END. /* GenerationBlock: */

   IF ipiPart EQ 1 THEN DO:
      FOR EACH ttPot 
      WHERE ttPot.iGeneration EQ 20
      AND   ttPot.cPlant      EQ "#":
         ACCUM "Plant" (COUNT).
         ACCUM ttPot.iPotNr (TOTAL).
      END.

      ASSIGN
         opiOutput = (ACCUM COUNT "Plant")
         opiOutput = (ACCUM TOTAL ttPot.iPotNr)
      .
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDay13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDay13 Procedure 
PROCEDURE getDay13 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutputX AS INT64       NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutputY AS INT64       NO-UNDO.

/* Variables for handling of the puzzle input */
DEFINE VARIABLE cFileName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcInput          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.

/* Variables for character read */
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

/* Variable for creating of ttCart */
DEFINE VARIABLE iNewID AS INTEGER     NO-UNDO.

/* Conversione Cart --> Track */
DEFINE VARIABLE cCartList  AS CHARACTER   NO-UNDO INIT ">,<,v,^".
DEFINE VARIABLE cTrackList AS CHARACTER   NO-UNDO INIT "-,-,|,|".

/* Variables for Cart "playground" */
DEFINE VARIABLE iMinX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinY       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxY       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOutputFile AS CHARACTER   NO-UNDO.

/* Variables for moving forward */
DEFINE VARIABLE iTick       AS INTEGER     NO-UNDO.

/* Variables for checking Crash */
DEFINE VARIABLE iCount      AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCrash      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lImages     AS LOGICAL     NO-UNDO.

/* For part two */
DEFINE BUFFER ttCheckCart   FOR ttCart.
DEFINE BUFFER ttCrashedCart FOR ttCart.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      cFileName = FILE-INFO:FULL-PATHNAME.
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   EMPTY TEMP-TABLE ttTrack.
   EMPTY TEMP-TABLE ttCart.

   DO iLine = 1 TO NUM-ENTRIES(lcInput, "~n"):
      cLine = ENTRY(iLine, lcInput, "~n").

      DO iChar = 1 TO LENGTH(cLine):
         cChar = SUBSTRING(cLine, iChar, 1).
         CASE cChar:
            WHEN "-" OR
            WHEN "|" OR
            WHEN "+" OR
            WHEN "/" OR
            WHEN "\" THEN DO:
               CREATE ttTrack.
               ASSIGN
                  ttTrack.iX     = iChar - 1
                  ttTrack.iY     = iLine - 1
                  ttTrack.cTrack = cChar
               .
            END.
            WHEN ">" OR
            WHEN "<" OR
            WHEN "v" OR
            WHEN "^" THEN DO:
               CREATE ttTrack.
               ASSIGN
                  ttTrack.iX = iChar - 1
                  ttTrack.iY = iLine - 1
                  ttTrack.cTrack = ENTRY (LOOKUP(cChar, cCartList), cTrackList)
               .
               iNewID = iNewID + 1.
               CREATE ttCart.
               ASSIGN
                  ttCart.iID        = iNewID
                  ttCart.iTick      = 0
                  ttCart.iX         = ttTrack.iX
                  ttCart.iY         = ttTrack.iY
                  ttCart.cDirection = cChar
               .
            END.
         END CASE.
      END.
   END.

   FOR EACH ttCart:
      ACCUM "Cart" (COUNT).
      PUBLISH "debug" (SUBSTITUTE("Cart ID&1 at (&2,&3). Direction: &4.",
                                  ttCart.iID,
                                  ttCart.iX,
                                  ttCart.iY,
                                  ttCart.cDirection)).
   END.

   FOR EACH ttTrack:
      ACCUM ttTrack.iX (MINIMUM MAXIMUM).
      ACCUM ttTrack.iY (MINIMUM MAXIMUM).
   END.
   
   ASSIGN
      iMinX = (ACCUM MINIMUM ttTrack.iX)
      iMaxX = (ACCUM MAXIMUM ttTrack.iX)
      iMinY = (ACCUM MINIMUM ttTrack.iY)
      iMaxY = (ACCUM MAXIMUM ttTrack.iY)
   .


   ASSIGN
      iTick   = 0
      lImages = FALSE
   .

   IF (ACCUM COUNT "Cart") LE 10 THEN DO:
      lImages = TRUE.
   END.

   TickBlock:
   REPEAT:

      /* Check for Crash */
      ASSIGN
         lCrash = FALSE
      .
      FOR EACH ttCart
      BREAK
      BY ttCart.iX
      BY ttCart.iY:
         ACCUM "" (COUNT).
         IF FIRST-OF(ttCart.iY) THEN DO:
            iCount = 0.
         END.
         iCount = iCount + 1.
         IF LAST-OF(ttCart.iY) THEN DO:
            IF iCount GT 1 THEN DO:
               /* More then One Cart on one Track --> CRASH */
               lCrash = TRUE.
               PUBLISH "debug" (SUBSTITUTE("Crash at (&1,&2)!", ttCart.iX, ttCart.iY)).
            END.
         END.
         IF ipiPart EQ 2 THEN DO:
            ASSIGN
               opiOutputX = ttCart.iX
               opiOutputY = ttCart.iY
            .
         END.
      END. /* Check for Crash */
      IF (ACCUM COUNT "") EQ 1 THEN DO:
         /* Last Cart left */
         lCrash = TRUE.
      END.

      IF lImages EQ TRUE 
      OR lCrash  EQ TRUE THEN DO:
         cOutputFile = SUBSTITUTE("&1_&2.txt",
                                  SUBSTRING(cFileName, 1, R-INDEX(cFilename, ".") - 1),
                                  STRING(iTick, "999999")).
         OUTPUT TO VALUE(cOutputFile).
         DO iY = iMinY TO iMaxY:
            cLine = "".
            DO iX = iMinX TO iMaxX:
               FIND  ttTrack
               WHERE ttTrack.iX EQ iX
               AND   ttTrack.iY EQ iY NO-ERROR.
               IF AVAILABLE ttTrack THEN DO:
                  FIND FIRST ttCart
                  WHERE ttCart.iTick EQ iTick
                  AND   ttCart.iX    EQ ttTrack.iX
                  AND   ttCart.iY    EQ ttTrack.iY NO-ERROR.
                  IF AVAILABLE ttCart THEN DO:
                     cChar = ttCart.cDirection.
                     FIND NEXT ttCart 
                     WHERE ttCart.iTick EQ iTick
                     AND   ttCart.iX    EQ ttTrack.iX
                     AND   ttCart.iY    EQ ttTrack.iY NO-ERROR.
                     IF AVAILABLE ttCart THEN DO:
                        /* Crash */
                        cChar = "X".
                     END.
                  END.
                  ELSE DO:   
                     cChar = ttTrack.cTrack.
                  END.
               END.
               ELSE DO:
                  cChar = " ".
               END.
               cLine = SUBSTITUTE("&1&2", cLine, cChar).
            END.
            PUBLISH "debug" (OUTPUT cLine).
            PUT UNFORMATTED
               cLine SKIP.
         END.
         OUTPUT CLOSE.

         RUN createImage
            (INPUT cOutputFile,
             INPUT MAX ((iMaxX - iMinX), 320),
             INPUT MAX ((iMaxY - iMinY), 160),
             INPUT 24).
         
      END.
      
      /* Check for Crash */
      FOR EACH ttCart
      BREAK
      BY ttCart.iX
      BY ttCart.iY:
         IF FIRST-OF(ttCart.iY) THEN DO:
            iCount = 0.
         END.
         iCount = iCount + 1.
         IF LAST-OF(ttCart.iY) THEN DO:
            IF iCount GT 1 THEN DO:
               /* More then One Cart on one Track --> CRASH */
               ASSIGN
                  opiOutputX = ttCart.iX
                  opiOutputY = ttCart.iY
               .
               IF ipiPart EQ 1 THEN DO:
                  LEAVE TickBlock.
               END.
            END.
         END.
      END. /* Check for Crash */
      
      FOR EACH ttCrashedCart:
         ACCUM "" (COUNT).
         ASSIGN
            opiOutputX = ttCrashedCart.iX
            opiOutputY = ttCrashedCart.iY
         .
      END.
      IF (ACCUM COUNT "") EQ 1 THEN DO:
         /* Only Cart left */
         LEAVE TickBlock.
      END.
      ELSE DO:
         PUBLISH "debug" (SUBSTITUTE("There are &1 Carts left.", (ACCUM COUNT ""))).
      END.

      /* Move forward */
      FOR EACH ttCart
      WHERE ttCart.iTick EQ iTick:
         RUN getNextTrack
            (INPUT  ttCart.iX,
             INPUT  ttCart.iY,
             INPUT  ttCart.cDirection,
             INPUT  ttCart.cLastCross,
             OUTPUT ttCart.iX,
             OUTPUT ttCart.iY,
             OUTPUT ttCart.cDirection,
             OUTPUT ttCart.cLastCross).

         ASSIGN
            ttCart.iTick = ttCart.iTick + 1
         .
         IF ipiPart EQ 2 THEN DO:
            /* Part Two, immediately check for Crash and remove Carts */
            ASSIGN
               lCrash = FALSE
            .
            FOR EACH ttCheckCart
            BREAK
            BY ttCheckCart.iX
            BY ttCheckCart.iY:
               IF FIRST-OF(ttCheckCart.iY) THEN DO:
                  iCount = 0.
               END.
               iCount = iCount + 1.
               IF LAST-OF(ttCheckCart.iY) THEN DO:
                  IF iCount GT 1 THEN DO:
                     /* More then One Cart on one Track --> CRASH */
                     lCrash = TRUE.
                     PUBLISH "debug" (SUBSTITUTE("Crash at (&1,&2) Tick &3", ttCheckCart.iX, ttCheckCart.iY, ttCart.iTick)).
                     FOR EACH ttCrashedCart
                     WHERE ttCrashedCart.iX EQ ttCheckCart.iX
                     AND   ttCrashedCart.iY EQ ttCheckCart.iY:
                        DELETE ttCrashedCart.
                     END.
                  END.
               END.
            END. /* Check for Crash */
         END. /* Part Two, immediately check for Crash and remove */
      END.
      ASSIGN
         iTick = iTick + 1
      .

/*       IF iTick GE 100 THEN DO: */
/*          LEAVE TickBlock.      */
/*       END.                     */
   END. /* TickBlock */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDay14) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDay14 Procedure 
PROCEDURE getDay14 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput1 AS INT64       NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput2 AS INT64       NO-UNDO.

/* Variables for handling of the puzzle input */
DEFINE VARIABLE cFileName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcInput          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.

/* Variables for character read */
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

/* Variable for creating of ttRecords */
DEFINE VARIABLE iNewID AS INTEGER     NO-UNDO.

DEFINE VARIABLE iElf           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartRecipe   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxRecipe     AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcRecipeList   AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE iCurrentElf    AS INTEGER     NO-UNDO EXTENT 2.
DEFINE VARIABLE iCurrentRecipe AS INTEGER     NO-UNDO EXTENT 2.
DEFINE VARIABLE iNewRecipe     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNewRecipe     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iStep          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxStep       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lLast10        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iEntry         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cSum           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lDebug         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cSearch        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLastString    AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttRecipe1 FOR ttRecipe.
DEFINE BUFFER ttRecipe2 FOR ttRecipe.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      cFileName = FILE-INFO:FULL-PATHNAME.
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   ASSIGN
      iMaxRecipe   = INT64(lcInput)
      iStartRecipe = iMaxRecipe
   NO-ERROR.

   IF iMaxRecipe LE 2018 THEN DO:
      lDebug = TRUE.
   END.
   ELSE DO:
      lDebug = FALSE.
   END.

   ASSIGN
      iCurrentElf[1] = 1
      iCurrentElf[2] = 2
   .

   IF ipiPart EQ 1 THEN DO:
      /* Initial Situation */
      ASSIGN
         lcRecipeList   = "3,7"
         lLast10        = FALSE
      .

      ASSIGN
         iCurrentRecipe[1] = INTEGER (ENTRY(iCurrentElf[1], lcRecipeList))
         iCurrentRecipe[2] = INTEGER (ENTRY(iCurrentElf[2], lcRecipeList))
      .

      RecipeBlock:
      REPEAT:
         IF iMaxRecipe LE 0 THEN DO:
            IF lLast10 EQ TRUE THEN DO:
               /* Done */
               DO iEntry = 1 TO 10:
                  cSum = cSum + ENTRY(iEntry + iStartRecipe, lcRecipeList).
               END.
               ASSIGN
                  opiOutput1 = INT64(cSum)
               .
               LEAVE RecipeBlock.
            END.
            ELSE DO:
               lLast10 = TRUE.
               iMaxRecipe = 9.
            END.
         END.

         /* Calculate New Recipe */
         IF lDebug THEN DO:
            PUBLISH "debug" (SUBSTITUTE("&1: &2 &3", STRING(lcRecipeList), iCurrentElf[1], iCurrentElf[2])).
         END.
         ASSIGN
            iNewRecipe = INTEGER (ENTRY (iCurrentElf[1], lcRecipeList)) + 
               INTEGER (ENTRY (iCurrentElf[2], lcRecipeList))
            cNewRecipe = STRING(iNewRecipe)
         .
         DO iChar = 1 TO LENGTH(STRING(iNewRecipe)):
            cChar = SUBSTRING(cNewRecipe, iChar, 1).
            ASSIGN
               lcRecipeList = lcRecipeList + "," + cChar
            .
            iMaxRecipe = iMaxRecipe - 1.
         END.

         /* Move Current Recipe of Elves */
         DO iElf = 1 TO 2:
            iMaxStep = 1 + INTEGER (ENTRY(iCurrentElf[iElf], lcRecipeList)).
            DO iStep = 1 TO iMaxStep:
               IF iCurrentElf[iElf] + 1 LE NUM-ENTRIES(lcRecipeList) THEN DO:
                  iCurrentElf[iElf] = iCurrentElf[iElf] + 1.
               END.
               ELSE DO:
                  iCurrentElf[iElf] = 1.
               END.
            END.
         END.

      END. /* RecipeBlock */
      
   END.
   ELSE DO:
      /* Part Two */
      /* Temp-table Solution */
      iNewID = iNewID + 1.
      CREATE ttRecipe.
      ASSIGN
         ttRecipe.iIndex  = iNewID
         ttRecipe.iRecipe = 3
      .
      IF LENGTH(cLastString) EQ LENGTH(lcInput) THEN DO:
         cLastString = SUBSTRING(cLastString, 2).
      END.
      cLastString = cLastString + STRING(ttRecipe.iRecipe).

      iNewID = iNewID + 1.
      CREATE ttRecipe.
      ASSIGN
         ttRecipe.iIndex  = iNewID
         ttRecipe.iRecipe = 7
      .
      IF LENGTH(cLastString) EQ LENGTH (lcInput) THEN DO:
         cLastString = SUBSTRING(cLastString, 2).
      END.
      cLastString = cLastString + STRING(ttRecipe.iRecipe).

      RecipeBlock2:
      REPEAT:
         FIND ttRecipe1 WHERE ttRecipe1.iIndex = iCurrentElf[1].
         FIND ttRecipe2 WHERE ttRecipe2.iIndex = iCurrentElf[2].

         ASSIGN
            iCurrentRecipe[1] = ttRecipe1.iRecipe
            iCurrentRecipe[2] = ttRecipe2.iRecipe
         .

         /* Calculate New Recipe */
         ASSIGN
            iNewRecipe = ttRecipe1.iRecipe + 
               ttRecipe2.iRecipe.
            cNewRecipe = STRING(iNewRecipe)
         .
         DO iChar = 1 TO LENGTH(STRING(iNewRecipe)):
            cChar = SUBSTRING(cNewRecipe, iChar, 1).
            iNewID = iNewID + 1.
            CREATE ttRecipe.
            ASSIGN
               ttRecipe.iIndex = iNewID
               ttRecipe.iRecipe = INTEGER(cChar)
            .
            IF LENGTH(cLastString) EQ LENGTH(lcInput) THEN DO:
               cLastString = SUBSTRING(cLastString, 2).
            END.
            cLastString = cLastString + STRING(ttRecipe.iRecipe).
            IF cLastString EQ lcInput THEN DO:
               /* Found matching input */
               ASSIGN
                  opiOutput2 = iNewID - LENGTH(lcInput)
               .
               FOR EACH ttRecipe WHERE
               ttRecipe.iIndex GE iNewID:
                  PUBLISH "debug" (SUBSTITUTE("&1: &2", ttRecipe.iIndex, ttRecipe.iRecipe)).
               END.
               PUBLISH "debug" (SUBSTITUTE("Found solution at &1.", opiOutput2)).

               LEAVE RecipeBlock2.
            END.
            IF iNewID MOD 20000 EQ 0 THEN DO:
               PUBLISH "debug" (SUBSTITUTE("Current Recipe: &1", iNewID)).
            END.
         END.

         /* Move Current Recipe of Elves */
         DO iElf = 1 TO 2:
            iMaxStep = 1 + iCurrentRecipe[iElf].
            iMaxStep = iCurrentElf[iElf] + iMaxStep.
            IF iMaxStep GT iNewID THEN DO:
               iMaxStep = iMaxStep MOD iNewID.
            END.
            iCurrentElf[iElf] = iMaxStep.
         END.
      END. /* RecipeBlock */
   END. /* Part Two */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDay15) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDay15 Procedure 
PROCEDURE getDay15 :
/*------------------------------------------------------------------------------
  Purpose:     Solve AOC2018 --- Day 15: Beverage Bandits ---
  Parameters:  
  Notes:       
  

------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPart    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput1 AS INT64       NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput2 AS INT64       NO-UNDO.

/* Variables for handling of the puzzle input */
DEFINE VARIABLE cFileName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcInput          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.

/* Variables for character read */
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

/* Variable for creating of ttRecords */
DEFINE VARIABLE iNewID AS INTEGER     NO-UNDO.

/* Variable for Debugging */
DEFINE VARIABLE lDebug AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cDebug AS CHARACTER   NO-UNDO.

/* Variables for "playground" */
DEFINE VARIABLE iMinX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinY       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxY       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOutput     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cOutputFile AS CHARACTER   NO-UNDO.

/* Variables for Rounds */
DEFINE VARIABLE iRound   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEnemy   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEnemies AS INTEGER     NO-UNDO.

/* Variable for Battle */
DEFINE VARIABLE iTargets AS INTEGER     NO-UNDO.

/* Variables for Moving */
DEFINE VARIABLE lMoveOk AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iNewX   AS INTEGER     NO-UNDO.
DEFINE VARIABLE inewY   AS INTEGER     NO-UNDO.

/* Buffers for Temp-table */
DEFINE BUFFER ttBoard FOR ttBoard.
DEFINE BUFFER ttUp    FOR ttBoard.
DEFINE BUFFER ttDown  FOR ttBoard.
DEFINE BUFFER ttRight FOR ttBoard.
DEFINE BUFFER ttLeft  FOR ttBoard.
DEFINE BUFFER ttUnit  FOR ttUnit.
DEFINE BUFFER ttEnemy FOR ttUnit.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      cFileName = FILE-INFO:FULL-PATHNAME.
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   EMPTY TEMP-TABLE ttBoard.
   EMPTY TEMP-TABLE ttUnit.
   EMPTY TEMP-TABLE ttPath.

   ASSIGN
      iRound = 0
   .

   DO iLine = 1 TO NUM-ENTRIES(lcInput, "~n"):
      cLine = ENTRY(iLine, lcInput, "~n").
      DO iChar = 1 TO LENGTH(cLine):
         cChar = SUBSTRING(cLine, iChar, 1).
         CASE cChar:
            WHEN "#" OR 
            WHEN "." THEN DO:
               CREATE ttBoard.
               ASSIGN 
                  ttBoard.iRound = iRound
                  ttBoard.iX     = iChar
                  ttBoard.iY     = iLine
                  ttBoard.cType  = cChar
               .
            END.
            WHEN "E" OR
            WHEN "G" THEN DO:
               CREATE ttBoard.
               ASSIGN
                  ttBoard.iRound = iRound
                  ttBoard.iX     = iChar
                  ttBoard.iY     = iLine
                  ttBoard.cType  = cChar
               .
               iNewID = iNewID + 1.
               CREATE ttUnit.
               ASSIGN
                  ttUnit.iID        = iNewID
                  ttUnit.iX         = ttBoard.iX
                  ttUnit.iY         = ttBoard.iY
                  ttUnit.cType      = ttBoard.cType
                  ttUnit.iPower     = 3
                  ttUnit.iHitPoints = 200
               .
            END.
         END CASE.
      END.
   END.
   
   FOR EACH ttBoard:
      ACCUM ttBoard.iX (MINIMUM MAXIMUM).
      ACCUM ttBoard.iY (MINIMUM MAXIMUM).
   END.

   ASSIGN
      iMinX = (ACCUM MINIMUM ttBoard.iX)
      iMaxX = (ACCUM MAXIMUM ttBoard.iX)
      iMinY = (ACCUM MINIMUM ttBoard.iY)
      iMaxY = (ACCUM MAXIMUM ttBoard.iY)
   .

   ASSIGN
      lOutput = TRUE
   .

   BattleBlock:
   REPEAT:
      IF lOutput EQ TRUE THEN DO:
         RUN outputBoard
            (INPUT  cFileName,
             INPUT  iRound,
             INPUT  0,
             INPUT  iMinX,
             INPUT  iMaxX,
             INPUT  iMinY,
             INPUT  iMaxY,
             OUTPUT cOutputFile).
         RUN createImage
            (INPUT cOutputFile,
             INPUT 160, /* MAX ((iMaxX - iMinX), 320), */
             INPUT 320, /* MAX ((iMaxY - iMinY), 160), */
             INPUT 6).
      END.

      FOR EACH ttUnit
      BREAK
      BY ttUnit.cType:
         IF FIRST-OF(ttUnit.cType) THEN DO:
            ACCUM "Type" (COUNT).
         END.
         ACCUM ttUnit.iHitPoints (TOTAL).
      END.

      IF (ACCUM COUNT "Type") EQ 1 THEN DO:
         ASSIGN
            opiOutput1 = iRound
            opiOutput2 = (ACCUM TOTAL ttUnit.iHitPoints)
         .
         LEAVE BattleBlock.
      END.

      UnitBlock:
      FOR EACH ttUnit
      WHERE ttUnit.iRound EQ iRound
      BREAK
      BY ttUnit.iY
      BY ttUnit.iX:
         /* For each ttUnit */
         IF ttUnit.cType EQ "E" THEN DO:
            cEnemy = "G".
         END.
         ELSE DO:
            cEnemy = "E".
         END.

         ASSIGN
            iEnemies = 0
         .

         PUBLISH "nodebug" (SUBSTITUTE("Round: &1 ttUnit: ID&2 &3 (&4,&5)", iRound, ttUnit.iID, ttUnit.cType, ttUnit.iX, ttUnit.iY)).

         FIND  ttBoard
         WHERE ttBoard.iX EQ ttUnit.iX
         AND   ttBoard.iY EQ ttUnit.iY.
         FIND  ttUp
         WHERE ttUp.iX EQ ttBoard.iX
         AND   ttUp.iY EQ ttBoard.iY - 1 NO-ERROR.
         FIND  ttDown
         WHERE ttDown.iX EQ ttBoard.iX
         AND   ttDown.iY EQ ttBoard.iY + 1 NO-ERROR.
         FIND  ttLeft
         WHERE ttLeft.iX EQ ttBoard.iX - 1
         AND   ttLeft.iY EQ ttBoard.iY NO-ERROR.
         FIND  ttRight
         WHERE ttRight.iX EQ ttBoard.iX + 1
         AND   ttRight.iY EQ ttBoard.iY NO-ERROR.

         PUBLISH "nodebug" (SUBSTITUTE("Board (&1,&2): &3", ttBoard.iX, ttBoard.iY, ttBoard.cType)).

         /* Decide what to do */
         IF AVAILABLE ttUp AND ttUp.cType EQ cEnemy THEN DO:
            iEnemies = iEnemies + 1.
         END.
         IF AVAILABLE ttDown AND ttDown.cType EQ cEnemy THEN DO:
            iEnemies = iEnemies + 1.
         END.
         IF AVAILABLE ttLeft AND ttLeft.cType EQ cEnemy THEN DO:
            iEnemies = iEnemies + 1.
         END.
         IF AVAILABLE ttRight AND ttRight.cType EQ cEnemy THEN DO:
            iEnemies = iEnemies + 1.
         END.
         
         PUBLISH "nodebug" (SUBSTITUTE("Unit ID&1 (&2,&3) &4 &5 &6: Enemies: &7", ttUnit.iID, ttUnit.iX, ttUnit.iY, ttUnit.cType, ttUnit.iHitPoints, ttUnit.iPower, iEnemies)). 

         IF iEnemies GT 0 THEN DO:
            /* Enemies in Range --> Attack */
            AttackBlock:
            FOR EACH ttEnemy
            WHERE ttEnemy.cType EQ cEnemy
            AND   (ABS (ttEnemy.iX - ttUnit.iX) + ABS(ttEnemy.iY - ttUnit.iY)) EQ 1
            BREAK
            BY ttEnemy.iHitPoints
            BY ttEnemy.iY
            BY ttEnemy.iX:
               /* AttackBlock */
               PUBLISH "debug" (SUBSTITUTE("Round: &1 Unit ID&2 &3 (&4,&5) Attacks Enemy ID&6 &7 (&8,&9)", ttUnit.iRound, ttUnit.iID, ttUnit.cType, ttUnit.iX, ttUnit.iY, ttEnemy.iID, ttEnemy.cType, ttEnemy.iX, ttEnemy.iY)).

               PUBLISH "nodebug" (SUBSTITUTE("Unit ID&1 &8 (&2,&3) Attacks Enemy ID&4 &9 (&5,&6) with Power &7", ttUnit.iID, ttUnit.iX, ttUnit.iY, ttEnemy.iID, ttEnemy.iX, ttEnemy.iY, ttUnit.iPower, ttUnit.cType, ttEnemy.cType)).

               ASSIGN
                  ttEnemy.iHitPoints = ttEnemy.iHitPoints - ttUnit.iPower
               .
               IF ttEnemy.iHitPoints LE 0 THEN DO:
                  /* Enemy dies */
                  PUBLISH "nodebug" (SUBSTITUTE("&1 (ID&2) killed &3 (ID&4) at location (&5,&6).",
                                              ttUnit.cType,
                                              ttUnit.iID,
                                              ttEnemy.cType,
                                              ttEnemy.iID,
                                              ttUnit.iX,
                                              ttUnit.iY)).
                  FIND  ttBoard
                  WHERE ttBoard.iX EQ ttEnemy.iX
                  AND   ttBoard.iY EQ ttEnemy.iY.
                  ttBoard.cType = ".".
                  
               END.

               IF LAST-OF(ttEnemy.iHitPoints) THEN DO:
                  LEAVE AttackBlock.
               END.
            END. /* AttackBlock */
            FOR EACH ttEnemy
            WHERE ttEnemy.cType EQ cEnemy
            AND   ttEnemy.iHitPoints LE 0:
               DELETE ttEnemy.
            END.
            ASSIGN
               ttUnit.iRound = ttUnit.iRound + 1
            .
         END. /* Enemies in Range --> Attack */
         ELSE DO:
            /* No enemies in Range --> Move */
            RUN getMove
               (INPUT  ttUnit.iID,
                INPUT  cEnemy,
                OUTPUT lMoveOk,
                OUTPUT iNewX,
                OUTPUT iNewY).
            
            PUBLISH "nodebug" (SUBSTITUTE("getMove(IN &1, IN &2, OUT &3, OUT &4, OUT &5)", ttUnit.iID, cEnemy, lMoveOk, iNewX, iNewY)).
            IF lMoveOk EQ TRUE THEN DO:
               PUBLISH "debug" (SUBSTITUTE("Round: &1 Unit ID&2 &3 (&4,&5) Moves to (&6,&7)", ttUnit.iRound, ttUnit.iID, ttUnit.cType, ttUnit.iX, ttUnit.iY, iNewX, iNewY)).

               IF  iNewX EQ 0
               AND iNewY EQ 0 THEN DO:
                  PUBLISH "nodebug" (SUBSTITUTE("Error!")).
                  ttUnit.iRound = ttUnit.iRound + 1.
                  NEXT UnitBlock.
               END.
               FIND  ttBoard
               WHERE ttBoard.iX EQ ttUnit.iX
               AND   ttBoard.iY EQ ttUnit.iY.
               ASSIGN
                  ttBoard.cType = "."
               .
               ASSIGN
                  ttUnit.iRound = ttUnit.iRound + 1
                  ttUnit.iX     = iNewX
                  ttUnit.iY     = iNewY
               .
               FIND  ttBoard
               WHERE ttBoard.iX EQ ttUnit.iX
               AND   ttBoard.iY EQ ttUnit.iY.
               IF ttBoard.cType NE "." THEN DO:
                  PUBLISH "nodebug" (SUBSTITUTE("Error in getMove. Move to occupied coordinate: (&1, &2)", iNewX, iNewY)).
               END.
               ASSIGN
                  ttBoard.cType = ttUnit.cType
               .
               IF lOutput EQ TRUE THEN DO:
                  RUN outputBoard
                     (INPUT  cFileName,
                      INPUT  iRound,
                      INPUT  ttUnit.iID,
                      INPUT  iMinX,
                      INPUT  iMaxX,
                      INPUT  iMinY,
                      INPUT  iMaxY,
                      OUTPUT cOutputFile).
                  RUN createImage
                     (INPUT cOutputFile,
                      INPUT 160, /* MAX ((iMaxX - iMinX), 320), */
                      INPUT 320, /* MAX ((iMaxY - iMinY), 160), */
                      INPUT 6).
               END.
            END.
            ELSE DO:
               PUBLISH "debug" (SUBSTITUTE("Round: &1 Unit ID&2 &3 (&4,&5) No Move found.", ttUnit.iRound, ttUnit.iID, ttUnit.cType, ttUnit.iX, ttUnit.iY)).

               PUBLISH "nodebug" (SUBSTITUTE("ttUnit: ID&1 (&2,&3) No Move", ttUnit.iID, ttUnit.iX, ttUnit.iY)).
               ttUnit.iRound = ttUnit.iRound + 1.
            END.
         END. /* No enemies in Range --> Move */
         
         PUBLISH "nodebug" (SUBSTITUTE("Round: &1 ID&2 (&3,&4) &5 Points: &6", ttUnit.iRound, ttUnit.iID, ttUnit.iX, ttUnit.iY, ttUnit.cType, ttUnit.iHitPoints)).

      END. /* For each ttUnit */
      
      ASSIGN
         iRound = iRound + 1
      .

      IF iRound GE 100 THEN DO:
         LEAVE BattleBlock.
      END.
   END. /* BattleBlock: */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDifference) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDifference Procedure 
PROCEDURE getDifference :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcString1     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcString2     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiDifferences AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opcCommon      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

   IF LENGTH(ipcString1) NE LENGTH(ipcString2) THEN DO:
      ASSIGN
         opiDifferences = ABS(LENGTH(ipcString1) - LENGTH(ipcString2))
      .
   END.
   ELSE DO:
      DO iChar = 1 TO LENGTH(ipcString1):
         IF SUBSTRING(ipcString1, iChar, 1) NE SUBSTRING(ipcString2, iChar, 1) THEN DO:
            opiDifferences = opiDifferences + 1.
         END.
         ELSE DO:
            opcCommon = opcCommon + SUBSTRING(ipcString1, iChar, 1).
         END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDifferences) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDifferences Procedure 
PROCEDURE getDifferences :
/*------------------------------------------------------------------------------
  Purpose:     Calculate differences between strings
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcCommon   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iBox        AS INTEGER     NO-UNDO.
DEFINE VARIABLE cBox        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iBox2       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cBox2       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iDifference AS INTEGER     NO-UNDO.
DEFINE VARIABLE cCommon     AS CHARACTER   NO-UNDO.

   DO iBox = 1 TO NUM-ENTRIES(ipcInput, "~n"):
      cBox = ENTRY(iBox, ipcInput, "~n").
      DO iBox2 = iBox TO NUM-ENTRIES(ipcInput, "~n"):
         cBox2 = ENTRY(iBox2, ipcInput, "~n").

         RUN getDifference
            (INPUT  cBox,
             INPUT  cBox2,
             OUTPUT iDifference,
             OUTPUT cCommon).

         IF iDifference EQ 1 THEN DO:
            ASSIGN
               opcCommon = cCommon
            .
            RETURN.
         END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEvents) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEvents Procedure 
PROCEDURE getEvents :
/*------------------------------------------------------------------------------
  Purpose:     Extracts events from input
  Parameters: 
  Notes:       Example input:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
  
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiGuard   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiMinute  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiGuard2  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiMinute2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE iLine          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iYear          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMonth         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDay           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iHour          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinute        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGuard         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEvent         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lAwake         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAsleep        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dtStart        AS DATETIME    NO-UNDO.
DEFINE VARIABLE dtPrev         AS DATETIME    NO-UNDO.
DEFINE VARIABLE iMinutesAsleep AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinutesMax    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSleepGuard    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSleepMinute   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxCount      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSleepGuard2   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSleepMinute2  AS INTEGER     NO-UNDO.

DEFINE BUFFER bPrevEvent FOR ttEvent.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
      REPEAT:
         IMPORT UNFORMATTED
            cLine.
      
/*    END.                                         */
/*    DO iLine = 1 TO NUM-ENTRIES(ipcInput, "~n"): */
/*       /* Raw load data into temp-table */       */
/*       cLine = ENTRY(iLine, ipcInput, "~n").     */

         ASSIGN
            iYear   = INTEGER (SUBSTRING(cLine, 2, 4))
            iMonth  = INTEGER (SUBSTRING(cLine, 7, 2))
            iDay    = INTEGER (SUBSTRING(cLine, 10, 2))
            iHour   = INTEGER (SUBSTRING(cLine, 13, 2))
            iMinute = INTEGER (SUBSTRING(cLine, 16, 2))
         NO-ERROR.
         
         ASSIGN
            cEvent  = TRIM (SUBSTRING(cLine, 20))
            iGuard  = 0
            lAwake  = FALSE
            lAsleep = FALSE
         .
   
         CASE ENTRY(1, cEvent, " "):
            WHEN "Guard" THEN DO:
               ASSIGN
                  iGuard = INTEGER(TRIM(ENTRY(2, cEvent, " "), "#"))
                  lAwake = TRUE
               NO-ERROR.
            END.
            WHEN "wakes" THEN DO:
               lAwake = TRUE.
            END.
            WHEN "falls" THEN DO:
               lAsleep = TRUE.
            END.
         END CASE.
   
         CREATE ttEvent.
         ASSIGN
            ttEvent.EventYear     = iYear
            ttEvent.EventMonth    = iMonth
            ttEvent.EventDay      = iDay
            ttEvent.EventHour     = iHour
            ttEvent.EventMinute   = iMinute
            ttEvent.EventDatetime = DATETIME (iMonth, iDay, iYear, iHour, iMinute)
            ttEvent.Guard         = iGuard
            ttEvent.Awake         = lAwake
            ttEvent.Asleep        = lAsleep
         .
   
      END. /* Raw load data into temp-table */

   END.

   FOR EACH ttEvent:
      ACCUM "" (COUNT).

      IF (ACCUM COUNT "") GT 1 THEN DO:
         FIND bPrevEvent
         WHERE bPrevEvent.EventDatetime EQ dtPrev.
         IF ttEvent.Guard = 0 THEN DO:
            ttEvent.Guard = bPrevEvent.Guard.
         END.
         dtPrev = ttEvent.EventDatetime.
      END.
      ELSE DO:
         dtPrev = ttEvent.EventDatetime.
      END.
   END.

   FOR EACH ttEvent:
      IF ttEvent.Asleep EQ TRUE THEN DO:
         dtStart = ttEvent.EventDatetime.
         iGuard  = ttEvent.Guard.
      END.
      ELSE DO:
         IF dtStart NE ? THEN 
         REPEAT:
            CREATE ttGuardAsleep.
            ASSIGN
               ttGuardAsleep.SleepGuard    = iGuard
               ttGuardAsleep.SleepDatetime = dtStart
               ttGuardAsleep.SleepYear     = YEAR (dtStart)
               ttGuardAsleep.SleepMonth    = MONTH (dtStart)
               ttGuardAsleep.SleepDay      = DAY (dtStart)
               ttGuardAsleep.SleepHour     = INTEGER(ENTRY(1, ENTRY(2, ISO-DATE(dtStart), "T"), ":"))
               ttGuardAsleep.SleepMinute   = INTEGER(ENTRY(2, ENTRY(2, ISO-DATE(dtStart), "T"), ":"))
            .
            dtStart = ADD-INTERVAL(dtStart, 1, "minute").
            IF dtStart GE ttEvent.EventDatetime THEN DO:
               LEAVE.
            END.
         END.
         dtStart = ?.
      END.
   END.

   /* Strategy 1: Find Guard that's most Asleep ... */
   FOR EACH ttGuardAsleep
   BREAK
   BY ttGuardAsleep.SleepGuard:
      IF FIRST-OF(ttGuardAsleep.SleepGuard) THEN DO:
         iMinutesAsleep = 0.
      END.
      iMinutesAsleep = iMinutesAsleep + 1.
      IF LAST-OF(ttGuardAsleep.SleepGuard) THEN DO:
         IF iMinutesAsleep GT iMinutesMax THEN DO:
            /* New Leader (sleeper) */
            iGuard = ttGuardAsleep.SleepGuard.
            iMinutesMax = iMinutesAsleep.
         END.
      END.
   END.
   /* and select the minute he's most Asleep */
   FOR EACH ttGuardAsleep
   WHERE ttGuardAsleep.SleepGuard EQ iGuard
   BREAK
   BY ttGuardAsleep.SleepMinute:
      IF FIRST-OF(ttGuardAsleep.SleepMinute) THEN DO:
         iCount = 0.   
      END.
      iCount = iCount + 1.
      IF LAST-OF(ttGuardAsleep.SleepMinute) THEN DO:
         IF iCount GT iMaxCount THEN DO:
            iSleepMinute = ttGuardAsleep.SleepMinute.
            iMaxCount = iCount.
         END.
      END.
   END.

   ASSIGN
      opiGuard  = iGuard
      opiMinute = iSleepMinute
   .

   /* Strategy 2: Find Guard with highest number of occurences on specific minute */
   FOR EACH ttGuardAsleep
   BREAK
   BY ttGuardAsleep.SleepMinute
   BY ttGuardAsleep.SleepGuard:
      IF FIRST-OF(ttGuardAsleep.SleepGuard) THEN DO:
         iCount = 0.
      END.
      iCount = iCount + 1.
      IF LAST-OF(ttGuardAsleep.SleepGuard) THEN DO:
         IF iCount GT iMaxCount THEN DO:
            ASSIGN
               iSleepMinute2 = ttGuardAsleep.SleepMinute
               iSleepGuard2  = ttGuardAsleep.SleepGuard
            .
            iMaxCount = iCount.
         END.
      END.
   END.

   ASSIGN
      opiGuard2  = iSleepGuard2
      opiMinute2 = iSleepMinute2
   .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMessage Procedure 
PROCEDURE getMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput AS INTEGER     NO-UNDO.

DEFINE VARIABLE iLine   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iX      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iVx     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iVy     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSecond AS INTEGER     NO-UNDO.

DEFINE VARIABLE iNewID AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinX  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxX  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinY  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxY  AS INTEGER     NO-UNDO.

DEFINE VARIABLE iStartHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrevWidth   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrevHeight  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrevSquare  AS INT64       NO-UNDO.
DEFINE VARIABLE iDeltaHeight AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttPoint9.

   PUBLISH "debug" (SUBSTITUTE("Read Input")).

   DO iLine = 1 TO NUM-ENTRIES(ipcInput, "~n"):
      /* Read Input */
      cLine = ENTRY(iLine, ipcInput, "~n").

      ASSIGN
         iX  = INTEGER(ENTRY(2, ENTRY(1, cLine), "<"))
         iY  = INTEGER(ENTRY(1, ENTRY(2, cLine), ">"))
         iVx = INTEGER(ENTRY(1, ENTRY(3, cLine, "<")))
         iVy = INTEGER(ENTRY(1, ENTRY(3, cLine), ">"))
      NO-ERROR.

      iNewID = iNewID + 1.
      CREATE ttPoint9.
      ASSIGN
         ttPoint9.iID     = iNewID
         ttPoint9.iStartX = iX
         ttPoint9.iStartY = iY
         ttPoint9.iVx     = iVx
         ttPoint9.iVy     = iVy
         ttPoint9.iSecond = 0
         ttPoint9.iX      = ttPoint9.iStartX + 
            (ttPoint9.iSecond * ttPoint9.iVx)
         ttPoint9.iY      = ttPoint9.iStartY +
            (ttPoint9.iSecond * ttPoint9.iVy)
      .
      IF iMinX EQ 0 
      OR ttPoint9.iX LT iMinX THEN DO:
         iMinX = ttPoint9.iX.
      END.
      IF iMaxX EQ 0
      OR ttPoint9.iX GT iMaxX THEN DO:
         iMaxX = ttPoint9.iX.
      END.
      IF iMinY EQ 0
      OR ttPoint9.iY LT iMinY THEN DO:
         iMinY = ttPoint9.iY.
      END.
      IF iMaxY EQ 0
      OR ttPoint9.iY GT iMaxY THEN DO:
         iMaxY = ttPoint9.iY.
      END.
      iSecond = ttPoint9.iSecond.
   END. /* Read Input */

   ASSIGN
      iPrevWidth   = (iMaxX - iMinX)
      iPrevHeight  = (iMaxY - iMinY)
      iPrevSquare  = (iPrevWidth * iPrevHeight)
      iStartHeight = (iMaxY - iMinY)
   .

   PUBLISH "debug" (SUBSTITUTE("Second #&1: (&2, &3) - (&4, &5). Width: &6 Height: &7", iSecond, iMinX, iMinY, iMaxX, iMaxY, (iMaxX - iMinX), (iMaxY - iMinY))).

   OUTPUT TO "aoc2018_9.run".
   PUT UNFORMATTED
      "Remove this file to stop running." SKIP
      "In file aoc2018_9.txt a new message will be added every 30 seconds." SKIP
   .
   OUTPUT CLOSE.

/*    OUTPUT TO "aoc2018_9.txt" APPEND.                                    */
/*    PUT UNFORMATTED "Second #" iSecond " "                               */
/*       SUBSTITUTE("From (&1,&2) To (&3,&4)", iMinX, iMinY, iMaxX, iMaxY) */
/*       SKIP.                                                             */
/*    DO iY = iMinY TO iMaxY:                                              */
/*       DO iX = iMinX TO iMaxX:                                           */
/*          FIND  ttPoint9                                                 */
/*          WHERE ttPoint9.iX EQ iX                                        */
/*          AND   ttPoint9.iY EQ iY NO-ERROR.                              */
/*          IF AVAILABLE ttPoint9 THEN DO:                                 */
/*             PUT UNFORMATTED "#".                                        */
/*          END.                                                           */
/*          ELSE DO:                                                       */
/*             PUT UNFORMATTED ".".                                        */
/*          END.                                                           */
/*       END.                                                              */
/*       PUT UNFORMATTED SKIP.                                             */
/*    END.                                                                 */
/*    PUT UNFORMATTED SKIP(2).                                             */
/*                                                                         */
/*    OUTPUT CLOSE.                                                        */

   PUBLISH "debug" (SUBSTITUTE("Start looking for the message.")).

   ASSIGN
      iSecond = iSecond + 1
   .

   SearchBlock:
   REPEAT:
      /* REPEAT: */
      ASSIGN
         iMinX = ?
         iMaxX = ?
         iMinY = ?
         iMaxY = ?
      .

      FOR EACH ttPoint9:
         ASSIGN
            ttPoint9.iSecond = iSecond
            ttPoint9.iX      = ttPoint9.iStartX + 
               (ttPoint9.iSecond * ttPoint9.iVx)
            ttPoint9.iY      = ttPoint9.iStartY +
               (ttPoint9.iSecond * ttPoint9.iVy)
         .
         
         ACCUM ttPoint9.iX (MINIMUM MAXIMUM).
         ACCUM ttPoint9.iY (MINIMUM MAXIMUM).

         IF iMinX EQ ?
         OR ttPoint9.iX LT iMinX THEN DO:
            iMinX = ttPoint9.iX.
         END.
         IF iMaxX EQ ?
         OR ttPoint9.iX GT iMaxX THEN DO:
            iMaxX = ttPoint9.iX.
         END.
         IF iMinY EQ ?
         OR ttPoint9.iY LT iMinY THEN DO:
            iMinY = ttPoint9.iY.
         END.
         IF iMaxY EQ ?
         OR ttPoint9.iY GT iMaxY THEN DO:
            iMaxY = ttPoint9.iY.
         END.
      END.

      IF iSecond LT 10 THEN DO:
         PUBLISH "debug" (SUBSTITUTE("Second #&1: (&2, &3) - (&4, &5). Width: &6 Height: &7", iSecond, iMinX, iMinY, iMaxX, iMaxY, (iMaxX - iMinX), (iMaxY - iMinY))).
      END.

      IF (iMaxY - iMinY) EQ 9
      THEN DO:
         PUBLISH "debug" (SUBSTITUTE("Second #&1 Width: &2 Height: &3", iSecond, (iMaxX - iMinX), (iMaxY - iMinY))).
      END.

      IF (iMaxY - iMinY) EQ 9 THEN DO:
         /* Message found  */
         PUBLISH "debug" ("CLEAR").
         
         OUTPUT TO "aoc2018_9.txt" APPEND.
         PUT UNFORMATTED "Second #" iSecond " "
            SUBSTITUTE("From (&1,&2) To (&3,&4)", iMinX, iMinY, iMaxX, iMaxY)
            SKIP.
         DO iY = iMinY TO iMaxY:
            cLine = "".
            DO iX = iMinX TO iMaxX:
               FOR EACH ttPoint9
               WHERE ttPoint9.iX EQ iX
               AND   ttPoint9.iY EQ iY:
                  ACCUM "" (COUNT).
               END.
               IF (ACCUM COUNT "") EQ 0 THEN DO:
                  PUT UNFORMATTED ".".
               END.
               ELSE DO:
                  PUT UNFORMATTED CHR(ASC("A") + (ACCUM COUNT "") - 1).
               END.
               FIND FIRST ttPoint9
               WHERE ttPoint9.iX EQ iX
               AND   ttPoint9.iY EQ iY NO-ERROR.
               IF AVAILABLE ttPoint9 THEN DO:
                  cLine = cLine + "#".
                  opiOutput = ttPoint9.iSecond.
               END.
               ELSE DO:
                  cLine = cLine + " ".
               END.
            END.
            PUT UNFORMATTED SKIP.
            PUBLISH "debug" (cLine).
         END.
         PUT UNFORMATTED SKIP(2).

         OUTPUT CLOSE.

      END. /* Message found  */
      
      FILE-INFO:FILE-NAME = "aoc2018_9.run".
      IF FILE-INFO:FILE-TYPE EQ ? THEN DO:
         LEAVE SearchBlock.
      END.

      IF (iPrevWidth  LT (iMaxX - iMinX))
      OR (iPrevHeight LT (iMaxY - iMaxY)) THEN DO:
         LEAVE SearchBlock.
      END.

      ASSIGN
         iDeltaHeight = iPrevHeight - (iMaxY - iMinY)
      .

      ASSIGN
         iPrevWidth  = (iMaxX - iMinX)
         iPrevHeight = (iMaxY - iMinY)
         iPrevSquare = iPrevWidth * iPrevHeight
      .

      iSecond = INTEGER (TRUNC ((iStartHeight - 9) / iDeltaHeight, 0)).

/*       iSecond = iSecond + 1. */
   END. /* REPEAT: */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMove Procedure 
PROCEDURE getMove :
/*------------------------------------------------------------------------------
  Purpose:     Gets a new move in the playground
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiID     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipcEnemy  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplMoveOk AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opiNewX   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiNewY   AS INTEGER     NO-UNDO.

DEFINE VARIABLE iDistance AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNewX     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNewY     AS INTEGER     NO-UNDO.

/* Define buffers */
DEFINE BUFFER ttUnit     FOR ttUnit.
DEFINE BUFFER ttBoard    FOR ttBoard.
DEFINE BUFFER ttUp       FOR ttBoard.
DEFINE BUFFER ttDown     FOR ttBoard.
DEFINE BUFFER ttLeft     FOR ttBoard.
DEFINE BUFFER ttRight    FOR ttBoard.
DEFINE BUFFER ttEnemy    FOR ttUnit.
DEFINE BUFFER ttPath     FOR ttPath.
DEFINE BUFFER ttNextPath FOR ttPath.

DEFINE VARIABLE iNrNextPath AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttPath.

   FIND  ttUnit
   WHERE ttUnit.iID EQ ipiID.

   ASSIGN
      iDistance = 0
   .

   CREATE ttPath.
   ASSIGN
      ttPath.iID_From  = ipiID
      ttPath.iID_To    = ?
      ttPath.iX_Start  = 0
      ttPath.iY_Start  = 0
      ttPath.iX_From   = ttUnit.iX
      ttPath.iY_From   = ttUnit.iY
      ttPath.iX_To     = ttUnit.iX
      ttPath.iY_To     = ttUnit.iY
      ttPath.iDistance = 0
   .

   NextPathBlock:
   REPEAT:
      iNrNextPath = 0.

      PUBLISH "nodebug" (SUBSTITUTE("iDistance: &1", iDistance)).

      FOR EACH ttPath
      WHERE ttPath.iID_From  EQ ipiID
      AND   ttPath.iDistance EQ iDistance:
         ASSIGN
            iX = ttPath.iX_To
            iY = ttPath.iY_To
         .

         FIND  ttBoard
         WHERE ttBoard.iX EQ iX
         AND   ttBoard.iY EQ iY.
         FIND  ttUp
         WHERE ttUp.iX EQ ttBoard.iX
         AND   ttUp.iY EQ ttBoard.iY - 1 NO-ERROR.
         FIND  ttDown
         WHERE ttDown.iX EQ ttBoard.iX
         AND   ttDown.iY EQ ttBoard.iY + 1 NO-ERROR.
         FIND  ttLeft
         WHERE ttLeft.iX EQ ttBoard.iX - 1
         AND   ttLeft.iY EQ ttBoard.iY NO-ERROR.
         FIND  ttRight
         WHERE ttRight.iX EQ ttBoard.iX + 1
         AND   ttRight.iY EQ ttBoard.iY NO-ERROR.

         PUBLISH "nodebug" (SUBSTITUTE("Board (&1,&2) = &3", ttBoard.iX, ttBoard.iY, ttBoard.cType)).

         IF AVAILABLE ttUp THEN DO:
            IF ttUp.cType EQ "." 
            AND NOT CAN-FIND (ttNextPath WHERE ttNextPath.iX_To EQ ttUp.iX AND ttNextPath.iY_To EQ ttUp.iY) THEN DO:
               PUBLISH "nodebug" (SUBSTITUTE("Up..: (&4,&5) + &6 = (&1,&2) = &3", ttUp.iX, ttUp.iY, ttUp.cType, ttBoard.iX, ttBoard.iY, iDistance)).

               CREATE ttNextPath.
               ASSIGN
                  ttNextPath.iID_From  = ipiID
                  ttNextPath.iID_To    = ?
                  ttNextPath.iX_From   = ttPath.iX_From
                  ttNextPath.iY_From   = ttPath.iY_From
                  ttNextPath.iX_To     = ttUp.iX
                  ttNextPath.iY_To     = ttUp.iY
                  ttNextPath.iDistance = ttPath.iDistance + 1
               .
               IF ttNextPath.iDistance EQ 1 THEN DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttUp.iX
                     ttNextPath.iY_Start = ttUp.iY
                  .
               END.
               ELSE DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttPath.iX_Start
                     ttNextPath.iY_Start = ttPath.iY_Start
                  .
               END.
               iNrNextPath = iNrNextPath + 1.
            END.
            IF ttUp.cType EQ ipcEnemy THEN DO:
               /* Enemy in Reach, select this direction */
               ASSIGN
                  opiNewX   = ttPath.iX_Start
                  opiNewY   = ttPath.iY_Start
                  oplMoveOk = TRUE
               .
               LEAVE NextPathBlock.
            END.
         END. /* AVAILABLE ttUp */
         IF AVAILABLE ttDown THEN DO:

            IF ttDown.cType EQ "." 
            AND NOT CAN-FIND (ttNextPath WHERE ttNextPath.iX_To EQ ttDown.iX AND ttNextPath.iY_To EQ ttDown.iY) THEN DO:
               PUBLISH "nodebug" (SUBSTITUTE("Down.: (&4,&5) + &6 = (&1,&2) = &3", ttDown.iX, ttDown.iY, ttDown.cType, ttBoard.iX, ttBoard.iY, iDistance)).

               CREATE ttNextPath.
               ASSIGN
                  ttNextPath.iID_From  = ipiID
                  ttNextPath.iID_To    = ?
                  ttNextPath.iX_From   = ttPath.iX_From
                  ttNextPath.iY_From   = ttPath.iY_From
                  ttNextPath.iX_To     = ttDown.iX
                  ttNextPath.iY_To     = ttDown.iY
                  ttNextPath.iDistance = ttPath.iDistance + 1
               .
               IF ttNextPath.iDistance EQ 1 THEN DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttDown.iX
                     ttNextPath.iY_Start = ttDown.iY
                  .
               END.
               ELSE DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttPath.iX_Start
                     ttNextPath.iY_Start = ttPath.iY_Start
                  .
               END.
               iNrNextPath = iNrNextPath + 1.
            END.
            IF ttDown.cType EQ ipcEnemy THEN DO:
               /* Enemy in Reach, select this direction */
               ASSIGN
                  opiNewX   = ttPath.iX_Start
                  opiNewY   = ttPath.iY_Start
                  oplMoveOk = TRUE
               .
               LEAVE NextPathBlock.
            END.
         END. /* AVAILABLE ttDown */
         IF AVAILABLE ttLeft THEN DO:

            IF ttLeft.cType EQ "."
            AND NOT CAN-FIND (ttNextPath WHERE ttNextPath.iX_To EQ ttLeft.iX AND ttNextPath.iY_To EQ ttLeft.iY) THEN DO:
               PUBLISH "nodebug" (SUBSTITUTE("Left.: (&4,&5) + &6 = (&1,&2) = &3", ttLeft.iX, ttLeft.iY, ttLeft.cType, ttBoard.iX, ttBoard.iY, iDistance)).
               CREATE ttNextPath.
               ASSIGN
                  ttNextPath.iID_From  = ipiID
                  ttNextPath.iID_To    = ?
                  ttNextPath.iX_From   = ttPath.iX_From
                  ttNextPath.iY_From   = ttPath.iY_From
                  ttNextPath.iX_To     = ttLeft.iX
                  ttNextPath.iY_To     = ttLeft.iY
                  ttNextPath.iDistance = ttPath.iDistance + 1
               .
               IF ttNextPath.iDistance EQ 1 THEN DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttLeft.iX
                     ttNextPath.iY_Start = ttLeft.iY
                  .
               END.
               ELSE DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttPath.iX_Start
                     ttNextPath.iY_Start = ttPath.iY_Start
                  .
               END.
               iNrNextPath = iNrNextPath + 1.
            END.
            IF ttLeft.cType EQ ipcEnemy THEN DO:
               /* Enemy in Reach, select this direction */
               ASSIGN
                  opiNewX   = ttPath.iX_Start
                  opiNewY   = ttPath.iY_Start
                  oplMoveOk = TRUE
               .
               LEAVE NextPathBlock.
            END.
         END. /* AVAILABLE ttLeft */
         IF AVAILABLE ttRight THEN DO:
            PUBLISH "nodebug" (SUBSTITUTE("Right: (&4,&5) + &6 = (&1,&2) = &3", ttRight.iX, ttRight.iY, ttRight.cType, ttBoard.iX, ttBoard.iY, iDistance)).

            IF ttRight.cType EQ "."
            AND NOT CAN-FIND (ttNextPath WHERE ttNextPath.iX_To EQ ttRight.iX AND ttNextPath.iY_To EQ ttRight.iY) THEN DO:
               CREATE ttNextPath.
               ASSIGN
                  ttNextPath.iID_From  = ipiID
                  ttNextPath.iID_To    = ?
                  ttNextPath.iX_From   = ttPath.iX_From
                  ttNextPath.iY_From   = ttPath.iY_From
                  ttNextPath.iX_To     = ttRight.iX
                  ttNextPath.iY_To     = ttRight.iY
                  ttNextPath.iDistance = ttPath.iDistance + 1
               .
               IF ttNextPath.iDistance EQ 1 THEN DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttRight.iX
                     ttNextPath.iY_Start = ttRight.iY
                  .
               END.
               ELSE DO:
                  ASSIGN
                     ttNextPath.iX_Start = ttPath.iX_Start
                     ttNextPath.iY_Start = ttPath.iY_Start
                  .
               END.
               iNrNextPath = iNrNextPath + 1.
            END.
            IF ttRight.cType EQ ipcEnemy THEN DO:
               /* Enemy in Reach, select this direction */
               ASSIGN
                  opiNewX   = ttPath.iX_Start
                  opiNewY   = ttPath.iY_Start
                  oplMoveOk = TRUE
               .
               LEAVE NextPathBlock.
            END.
         END. /* AVAILABLE ttRight */

      END. /*  FOR EACH ttPath WHERE ttPath.iDistance EQ iDistance: */

      IF iNrNextPath EQ 0 THEN DO:
         oplMoveOk = FALSE.
         LEAVE NextPathBlock.
      END.
      ELSE DO:
         iDistance = iDistance + 1.
      END.

   END. /* NextPathBlock */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextTrack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNextTrack Procedure 
PROCEDURE getNextTrack :
/*------------------------------------------------------------------------------
  Purpose:     Find Next Track of Cart
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiX         AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiY         AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipcDirection AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcLastCross AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiX         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiY         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opcDirection AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcLastCross AS CHARACTER   NO-UNDO.

   ASSIGN
      opiX         = ipiX
      opiY         = ipiY
      opcDirection = ipcDirection
      opcLastCross = ipcLastCross
   .

   CASE ipcDirection:
      WHEN "^" THEN DO:
         opiY = opiY - 1.
      END.
      WHEN "v" THEN DO:
         opiY = opiY + 1.
      END.
      WHEN ">" THEN DO:
         opiX = opiX + 1.
      END.
      WHEN "<" THEN DO:
         opiX = opiX - 1.         
      END.
   END CASE.

   FIND  ttTrack
   WHERE ttTrack.iX EQ opiX
   AND   ttTrack.iY EQ opiY NO-ERROR.
   IF AVAILABLE ttTrack THEN DO:
      CASE ttTrack.cTrack:
         WHEN "|" OR
         WHEN "-" THEN DO:
            /* Direction stays the same */
            opcDirection = ipcDirection.
         END.
         WHEN "/" THEN DO:
            /* Right/Down turn */
            IF ipcDirection EQ "^" THEN DO:
               opcDirection = ">".
            END.
            IF ipcDirection EQ "<" THEN DO:
               opcDirection = "v".
            END.
            IF ipcDirection EQ ">" THEN DO:
               opcDirection = "^".
            END.
            IF ipcDirection EQ "v" THEN DO:
               opcDirection = "<".
            END.
         END.
         WHEN "\" THEN DO:
            /* Left/Down turn */
            IF ipcDirection EQ "^" THEN DO:
               opcDirection = "<".
            END.
            IF ipcDirection EQ ">" THEN DO:
               opcDirection = "v".
            END.
            IF ipcDirection EQ "<" THEN DO:
               opcDirection = "^".
            END.
            IF ipcDirection EQ "v" THEN DO:
               opcDirection = ">".
            END.
         END.
         WHEN "+" THEN DO:
            /* Crossing */
            CASE ipcLastCross:
               WHEN "" OR
               WHEN "Right" THEN DO:
                  opcLastCross = "Left".
               END.
               WHEN "Left" THEN DO:
                  opcLastCross = "Straight".
               END.
               WHEN "Straight" THEN DO:
                  opcLastCross = "Right".
               END.
            END CASE.
            CASE opcLastCross:
               WHEN "Left" THEN DO:
                  IF ipcDirection = "v" THEN DO:
                     opcDirection = ">".
                  END.
                  IF ipcDirection = "<" THEN DO:
                     opcDirection = "v".
                  END.
                  IF ipcDirection = "^" THEN DO:
                     opcDirection = "<".
                  END.
                  IF ipcDirection = ">" THEN DO:
                     opcDirection = "^".
                  END.
               END.
               WHEN "Straight" THEN DO:
                  opcDirection = ipcDirection.
               END.
               WHEN "Right" THEN DO:
                  IF ipcDirection = "v" THEN DO:
                     opcDirection = "<".
                  END.
                  IF ipcDirection = "<" THEN DO:
                     opcDirection = "^".
                  END.
                  IF ipcDirection = "^" THEN DO:
                     opcDirection = ">".
                  END.
                  IF ipcDirection = ">" THEN DO:
                     opcDirection = "v".
                  END.
               END.
            END CASE.
         END.
      END CASE.
   END.
   ELSE DO:
      PUBLISH "debug" (SUBSTITUTE("New Coordinates (&1, &2) off track! Input: (&3, &4) Direction: &5.",
                                  opiX,
                                  opiY,
                                  ipiX,
                                  ipiY,
                                  ipcDirection)).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOccupied) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getOccupied Procedure 
PROCEDURE getOccupied :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOccupied AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiID       AS INTEGER     NO-UNDO.

DEFINE VARIABLE iSquare AS INTEGER     NO-UNDO.
DEFINE VARIABLE cSquare AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iID     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX_From AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY_From AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidth  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX_To   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY_To   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY      AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttSquare.
   EMPTY TEMP-TABLE ttOccupy.

   DO iSquare = 1 TO NUM-ENTRIES(ipcInput, "~n"):
      cSquare = ENTRY(iSquare, ipcInput, "~n").

      RUN getSquare
         (INPUT  cSquare,
          OUTPUT iID,
          OUTPUT iX_From,
          OUTPUT iY_From,
          OUTPUT iWidth,
          OUTPUT iHeight,
          OUTPUT iX_To,
          OUTPUT iY_To).

      CREATE ttSquare.
      ASSIGN
         ttSquare.iID     = iID
         ttSquare.iX_From = iX_From
         ttSquare.iY_From = iY_From
         ttSquare.iWidth  = iWidth
         ttSquare.iHeight = iHeight
         ttSquare.iX_To   = iX_To
         ttSquare.iY_To   = iY_To
      .

   END.

   FOR EACH ttSquare:
      DO iX = ttSquare.iX_From TO ttSquare.iX_To:
         DO iY = ttSquare.iY_From TO ttSquare.iY_To:
            FIND  ttOccupy
            WHERE ttOccupy.iX = iX
            AND   ttOccupy.iY = iY NO-ERROR.
            IF NOT AVAILABLE ttOccupy THEN DO:
               CREATE ttOccupy.
               ASSIGN
                  ttOccupy.iX = iX
                  ttOccupy.iY = iY
               .
            END.
            ASSIGN
               ttOccupy.iNrOccupy = ttOccupy.iNrOccupy + 1
            .
/*             PUBLISH "debug" (SUBSTITUTE("(&1;&2): &3 #&4", ttOccupy.iX, ttOccupy.iY, ttOccupy.iNrOccupy, ttSquare.iID)).  */

         END.
      END.
   END.

   FOR EACH ttOccupy
   WHERE ttOccupy.iNrOccupy GE 2:
      ACCUM "" (COUNT).
   END.

   ASSIGN
      opiOccupied = (ACCUM COUNT "")
   .

   /* Part Two - What is the ID of the only claim that doesn't overlap? */
   FindSquare:
   FOR EACH ttSquare:
      DO iX = ttSquare.iX_From TO ttSquare.iX_To:
         DO iY = ttSquare.iY_From TO ttSquare.iY_To:
            FIND  ttOccupy
            WHERE ttOccupy.iX = iX
            AND   ttOccupy.iY = iY.
            IF ttOccupy.iNrOccupy GT 1 THEN DO:
               /* Too many overlaps */
               NEXT FindSquare.
            END.
         END.
      END.

      ASSIGN
         opiID = ttSquare.iID
      .
      LEAVE FindSquare.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getOrder Procedure 
PROCEDURE getOrder :
/*------------------------------------------------------------------------------
  Purpose:     Returns the order in which steps have to be done
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcOutput AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput AS INTEGER     NO-UNDO.

DEFINE VARIABLE lcInput     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCharList   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOrderList  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStartList  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEndList    AS CHARACTER   NO-UNDO.

/* Variables for Part Two: Build the Construction */
DEFINE VARIABLE iWorker AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSecond AS INTEGER     NO-UNDO.

DEFINE BUFFER ttNextStep         FOR ttStep.
DEFINE BUFFER ttNextConstruction FOR ttConstruction.
DEFINE BUFFER ttNextWorker       FOR ttWorker.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
      ASSIGN
         cLine = ENTRY(iLine, lcInput, "~n")
      .

      /* Step C must be finished before step A can begin. */
      /* 1    2 3    4  5        6      7    8 9   10     */
      CREATE ttStep.
      ASSIGN
         ttStep.cStep     = ENTRY(8, cLine, " ")
         ttStep.cRequires = ENTRY(2, cLine, " ")
      .

      IF LOOKUP(ttStep.cStep, cCharList) EQ 0 THEN DO:
         cCharList = SUBSTITUTE("&1&2&3",
                                cCharList,
                                (IF cCharList EQ "" THEN "" ELSE ","),
                                ttStep.cStep).
      END.
      IF LOOKUP(ttStep.cRequires, cCharList) EQ 0 THEN DO:
         cCharList = SUBSTITUTE("&1&2&3",
                                cCharList,
                                (IF cCharList EQ "" THEN "" ELSE ","),
                                ttStep.cRequires).
      END.
/*       PUBLISH "debug" (SUBSTITUTE("&1 requires &2", ttStep.cStep, ttStep.cRequires)). */
   END.

/*    PUBLISH "debug" (SUBSTITUTE("Character List: &1", cCharList)). */

   /* Create independent Steps */
   DO iChar = 1 TO NUM-ENTRIES(cCharList):
      cChar = ENTRY(iChar, cCharList).
      FIND FIRST ttStep 
      WHERE ttStep.cStep EQ cChar NO-ERROR.
      IF NOT AVAILABLE ttStep THEN DO:
         CREATE ttStep.
         ASSIGN
            ttStep.cStep = cChar
         .

/*          PUBLISH "debug" (SUBSTITUTE("&1 requires &2", ttStep.cStep, ttStep.cRequires)). */
      END.
   END.

   /* Save for later */
   FOR EACH ttStep:
      CREATE ttConstruction.
      BUFFER-COPY ttStep TO ttConstruction.
   END.

   ProcessBlock:
   REPEAT:
      cChar = "".
      ProcessStep:
      FOR EACH ttStep 
      WHERE ttStep.cRequires = ""
      AND   NOT CAN-FIND(FIRST ttNextStep WHERE ttNextStep.cStep EQ ttStep.cStep AND ttNextStep.cRequires NE ""):
         cChar = ttStep.cStep.
         IF LOOKUP(cChar, cOrderList) EQ 0 THEN DO:
            cOrderList = SUBSTITUTE("&1&2&3",
                                    cOrderList,
                                    (IF cOrderList EQ "" THEN "" ELSE ","),
                                    cChar).

/*             PUBLISH "debug" (SUBSTITUTE("Step &1 done!", cChar)). */
         END.

         DELETE ttStep.
         LEAVE ProcessStep.
      END.
      IF cChar EQ "" THEN DO:
         /* No more step to do */
         LEAVE ProcessBlock.
      END.
      /* Clear all Requires Steps */
      FOR EACH ttStep 
      WHERE ttStep.cRequires = cChar:

/*          PUBLISH "debug" (SUBSTITUTE("Free up &1", ttStep.cStep)). */
         ttStep.cRequires = "".
      END.
   END.

   ASSIGN
      opcOutput = REPLACE(cOrderList, ",", "")
   .
   
/*    PUBLISH "debug" (SUBSTITUTE("Finished Part One at &1. Solution: &2", NOW, opcOutput)). */
   
   /* Now do the construction */
   /* Create the Workers */
   DO iWorker = 1 TO 5:
      CREATE ttWorker.
      ASSIGN
         ttWorker.iWorker  = iWorker
         ttWorker.lIdle    = TRUE
      .
   END.

   ASSIGN
      iSecond = 0
   .
   
   ConstructionBlock:
   REPEAT:

      FIND FIRST ttWorker WHERE ttWorker.lIdle EQ TRUE NO-ERROR.
      IF AVAILABLE ttWorker THEN DO:
         /* We can construct something */
         cChar = "".
         ConstructionStep:
         FOR EACH ttConstruction
         WHERE ttConstruction.cRequires = ""
         AND   NOT CAN-FIND(FIRST ttNextConstruction WHERE ttNextConstruction.cStep EQ ttConstruction.cStep AND ttNextConstruction.cRequires NE "")
         AND   NOT CAN-FIND(FIRST ttNextWorker       WHERE ttNextWorker.cStep       EQ ttConstruction.cStep):
            cChar = ttConstruction.cStep.
            IF LOOKUP(cChar, cStartList) EQ 0 THEN DO:
               cStartList = SUBSTITUTE("&1&2&3",
                                       cStartList,
                                       (IF cStartList EQ "" THEN "" ELSE ","),
                                       cChar).

/*                PUBLISH "debug" (SUBSTITUTE("Construction &1 to start.", cChar)). */
            END.

            DELETE ttConstruction.
            LEAVE ConstructionStep.
         END.

         IF cChar NE "" THEN DO:
            /* Assign Construction to this Worker */
            ASSIGN
               ttWorker.lIdle   = FALSE
               ttWorker.cStep   = cChar
               ttWorker.iFinish = 60 + ASC(UPPER(cChar)) - ASC("A") + 1
            .
            NEXT ConstructionBlock.
         END.
      END.
      
      /* Move time forward */
      ASSIGN
         iSecond = iSecond + 1
      .

      FOR EACH ttWorker:
         CREATE ttSecond.
         ASSIGN
            ttSecond.iSecond = iSecond
            ttSecond.iWorker = ttWorker.iWorker
            ttSecond.cStep   = (IF ttWorker.lIdle THEN "." ELSE ttWorker.cStep)
         .
      END.

      /* Move contruction forward */
      FOR EACH ttWorker
      WHERE ttWorker.lIdle EQ FALSE:
         /* Working Workers */
         ASSIGN
            ttWorker.iCurrent = ttWorker.iCurrent + 1
         .

         IF ttWorker.iCurrent EQ ttWorker.iFinish THEN DO:
            /* This construction is finished */
            IF LOOKUP(ttWorker.cStep, cEndList) EQ 0 THEN DO:
               ASSIGN
                  cEndList = SUBSTITUTE("&1&2&3", 
                                        cEndList,
                                        (IF cEndList EQ "" THEN "" ELSE ","),
                                        ttWorker.cStep)
               .
               PUBLISH "debug" (SUBSTITUTE("EndList: &1", cEndList)).

               FOR EACH ttConstruction 
               WHERE ttConstruction.cStep EQ ttWorker.cStep:
                  DELETE ttConstruction.
               END.
            END.

            /* Clear all Requires Steps */
            FOR EACH ttConstruction 
            WHERE ttConstruction.cRequires = ttWorker.cStep:

/*                PUBLISH "debug" (SUBSTITUTE("Free up &1", ttConstruction.cStep)). */
               ttConstruction.cRequires = "".
            END.

            /* Set Worker as Idle */
            ASSIGN
               ttWorker.lIdle    = TRUE
               ttWorker.cStep    = ""
               ttWorker.iFinish  = 0
               ttWorker.iCurrent = 0
            .
         END. /* This construction is finished */
      END. /* Working Workers */

      IF  LENGTH(cOrderList) EQ LENGTH(cStartList)
      AND LENGTH(cEndList)   EQ LENGTH(cStartList)
      THEN DO:
         /* No more steps to Construct */
         LEAVE ConstructionBlock.
      END.
   END. /* ConstructionBlock: */

   ASSIGN
      opiOutput = iSecond
   .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReduction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getReduction Procedure 
PROCEDURE getReduction :
/*------------------------------------------------------------------------------
  Purpose:     Reduce an input string by removing pairs of aA, Aa, bB, etc.
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiLength    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiMinLength AS INTEGER     NO-UNDO.

DEFINE VARIABLE lcInput     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar1      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChar2      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iReductions AS INTEGER     NO-UNDO.

DEFINE VARIABLE cCharacters AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iExclude    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLength     AS INTEGER     NO-UNDO.

DEFINE BUFFER ttChar     FOR ttChar.
DEFINE BUFFER ttPrevChar FOR ttChar.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   DO iChar = 1 TO LENGTH(lcInput):
      ASSIGN
         cChar1 = SUBSTRING(lcInput, iChar, 1)
      .
      IF TRIM (cChar1) NE "" THEN DO:
         CREATE ttChar.
         ASSIGN
            ttChar.iPosition = iChar
            ttChar.cChar     = cChar1
         .
      END.
   END.


   cCharacters = " ".
   FOR EACH ttChar
   BREAK
   BY ttChar.cChar:
      IF FIRST-OF(ttChar.cChar) THEN DO:
         cCharacters = cCharacters + LC(ttChar.cChar).
      END.
   END.

   DO iExclude = 1 TO LENGTH(cCharacters):
      FOR EACH ttChar:
         ASSIGN
            ttChar.lConsider = ttChar.cChar NE SUBSTRING(cCharacters, iExclude, 1)
         .
      END.

      ReductionBlock:
      REPEAT:
         ASSIGN
            iReductions = 0
         .
   
         FOR EACH ttChar WHERE ttChar.lConsider EQ TRUE BY ttChar.iPosition:

            ACCUM "" (COUNT).
   /*          PUBLISH "debug" (SUBSTITUTE("&1. &2", (ACCUM COUNT ""), ttChar.cChar)). */
   
            IF (ACCUM COUNT "") GT 1 THEN DO:
               FIND LAST ttPrevChar 
               WHERE ttPrevChar.iPosition LT ttChar.iPosition 
               AND   ttPrevChar.lConsider EQ TRUE NO-ERROR.
               IF  AVAILABLE ttPrevChar
               AND ttPrevChar.cChar EQ ttChar.cChar
               AND COMPARE(ttPrevChar.cChar, "EQ", ttChar.cChar, "CASE-SENSITIVE") EQ FALSE  THEN DO:
   /*                PUBLISH "debug" (SUBSTITUTE("Eliminate: &1&2", ttPrevChar.cChar, ttChar.cChar)). */
                  ASSIGN
                     ttPrevChar.lConsider = FALSE
                     ttChar.lConsider     = FALSE
                  .
                  iReductions = iReductions + 1.
               END.
            END.
         END.
   
         IF iReductions = 0 THEN DO:
            /* No more reductions possible */
            ASSIGN
               iLength = (ACCUM COUNT "")
            .
            
   /*          PUBLISH "debug" (SUBSTITUTE("opiLength: &1 ACCUM COUNT: &2", opiLength, (ACCUM COUNT ""))). */
   
            LEAVE ReductionBlock.
         END.
      END. /* ReductionBlock */

      PUBLISH "debug" (SUBSTITUTE("Without &1: &2.", SUBSTRING(cCharacters, iExclude, 1), iLength)).

      IF opiMinLength EQ 0
      OR iLength      LT opiMinLength THEN DO:
         opiMinLength = iLength.
      END.

      IF SUBSTRING(cCharacters, iExclude, 1) EQ " " THEN DO:
         opiLength = iLength.
      END.

   END. /* DO iExclude = 1 TO LENGTH(cCharacters): */

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSquare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSquare Procedure 
PROCEDURE getSquare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       Input
               ------------------------------
               ipcLine   = #1 @ 82,901: 26x12
               
               Output
               ------------------------------
               opiID     = 1
               opiX      = 82
               opiY      = 901
               opiWidth  = 26
               opiHeight = 12
               opiX2     = 108
               opiY2     = 913
               
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcLine   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiID     AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiX      AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiY      AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiWidth  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiHeight AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiX2     AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiY2     AS INTEGER     NO-UNDO.

DEFINE VARIABLE cID    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAt    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSize  AS CHARACTER   NO-UNDO.

   IF NUM-ENTRIES(ipcline, " ") EQ 4 THEN DO:
      ASSIGN
         cID    = ENTRY(1, ipcLine, " ")
         cAt    = ENTRY(2, ipcLine, " ")
         cStart = ENTRY(3, ipcLine, " ")
         cSize  = ENTRY(4, ipcLine, " ")
      .

      ASSIGN
         opiID = INTEGER(SUBSTRING(cID, 2))
      .

      ASSIGN
         cStart = TRIM(cStart, ":")
      .

      ASSIGN
         opiX = INTEGER (ENTRY(1, cStart)) + 1
         opiY = INTEGER (ENTRY(2, cStart)) + 1
      NO-ERROR.

      ASSIGN
         opiWidth  = INTEGER(ENTRY(1, cSize, "x"))
         opiHeight = INTEGER(ENTRY(2, cSize, "x"))
      NO-ERROR.

      ASSIGN
         opiX2 = opiX + opiWidth  - 1
         opiY2 = opiY + opiHeight - 1
      .
      
/*       PUBLISH "debug" (SUBSTITUTE("Square #&1: (&2;&3)-(&4;&5)", opiID, opiX, opiY, opiX2, opiY2)).  */
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSumMetadata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSumMetadata Procedure 
PROCEDURE getSumMetadata :
/*------------------------------------------------------------------------------
  Purpose:     Calculates the sum of the metadata entries
  Parameters:  
  Notes:       AOC2018 Day 8
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutput2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE lcInput     AS LONGCHAR NO-UNDO.

/* Variables for reading input */
DEFINE VARIABLE iIndex       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumber      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cInputType   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNewID       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCurrentID   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSumMetadata AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNumber      AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttChildNode  FOR ttNode.
DEFINE BUFFER ttParentNode FOR ttNode.

   FILE-INFO:FILE-NAME = ipcInput.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO:
      COPY-LOB 
         FROM FILE file-info:FULL-PATHNAME 
         TO   OBJECT lcInput
      .
   END.
   ELSE DO:
      lcInput = ipcInput.
   END.

   EMPTY TEMP-TABLE ttNode.

   DO iIndex = 1 TO NUM-ENTRIES(lcInput, " "):
      /* Input numbers */
      cNumber = TRIM (ENTRY(iIndex, lcInput, " ")).

      ASSIGN
         iNumber = INTEGER (cNumber)
      NO-ERROR.

      IF ERROR-STATUS:ERROR EQ TRUE THEN DO:
         MESSAGE ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         NEXT.
      END.

      CASE cInputType:
         WHEN "" THEN DO:
            /* First number = Header: Child Nodes */
            iNewID = iNewID + 1.
            CREATE ttNode.
            ASSIGN
               ttNode.iID = iNewID
               ttNode.iNrChildNodes = iNumber
            .
            ASSIGN
               iCurrentID = ttNode.iID
               cInputType = "inrMetaData"
            .
         END. /* First number = Header: Child Nodes */
         WHEN "inrMetaData" THEN DO:
            /* Second number of Node Header: Number of Metadata entries */
            FIND ttNode WHERE ttNode.iID = iCurrentID.
            ASSIGN
               ttNode.iNrMetaData = iNumber
            .
            IF ttNode.iNrChildNodes EQ 0 THEN DO:
               /* Child Nodes done or not available --> Metadata */
               cInputType = "Metadata".
            END.
            ELSE DO:
               /* Process Childs */
               ASSIGN
                  cInputType = "ChildNode"
               .
            END. /* Process Childs */
         END. /* Second number of Node Header: Number of Metadata entries */
         WHEN "ChildNode" THEN DO:
            iNewID = iNewID + 1.
            CREATE ttNode.
            ASSIGN
               ttNode.iID           = iNewID
               ttNode.iNrChildNodes = iNumber
               ttNode.iID_Parent    = iCurrentID
            .
            FIND ttParentNode WHERE ttParentNode.iID EQ ttNode.iID_Parent.
            ASSIGN
               ttParentNode.iNrChildCreated = ttParentNode.iNrChildCreated + 1
               ttNode.iChildNr              = ttParentNode.iNrChildCreated
               iCurrentID                   = ttNode.iID
               cInputType                   = "inrMetaData"
            .
         END.
         WHEN "Metadata" THEN DO:
            FIND ttNode WHERE ttNode.iID EQ iCurrentID.
            ASSIGN
               ttNode.cMetadata = SUBSTITUTE("&1&2&3",
                                             ttNode.cMetadata,
                                             (IF ttNode.cMetadata EQ "" THEN "" ELSE ","),
                                             iNumber)
               ttNode.iSumMetadata = ttNode.iSumMetadata + iNumber
            .
            IF NUM-ENTRIES(ttNode.cMetadata) EQ ttNode.iNrMetadata THEN DO:
               /* Finished Metadata */
               iSumMetadata = iSumMetadata + ttNode.iSumMetadata.
               FIND ttParentNode WHERE ttParentNode.iID EQ ttNode.iID_Parent NO-ERROR.
               IF AVAILABLE ttParentNode THEN DO:

                  IF ttParentNode.iNrChildNodes EQ ttNode.iChildNr
                  OR ttParentNode.iNrChildNodes EQ 0 THEN DO:
                     /* Child Nodes done or not available --> Metadata */
                     ASSIGN
                        iCurrentID = ttParentNode.iID
                        cInputType = "Metadata"
                     .
                  END.
                  ELSE DO:
                     /* Process Childs */
                     ASSIGN
                        iCurrentID = ttParentNode.iID
                        cInputType = "ChildNode".
                     .
                  END. /* Process Childs */
               END.
            END.
         END.
      END CASE.

   END. /* Input numbers */

   ASSIGN
      opiOutput = iSumMetadata
   .

   /* Part Two */
   /* Get values of Root node. */
   FIND ttNode WHERE ttNode.iID = 1. /* Root Node */

   ASSIGN
      ttNode.iValue = getNodeValue(ttNode.iID)
   .

   ASSIGN
      opiOutput2 = ttNode.iValue
   .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTT Procedure 
PROCEDURE getTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR ttUnit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-outputBoard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputBoard Procedure 
PROCEDURE outputBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFileName   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiRound      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiUnit       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiMinX       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxX       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiMinY       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxY       AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opcOutputFile AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iX          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChar       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPoints     AS CHARACTER   NO-UNDO.

   opcOutputFile = SUBSTITUTE("&1_&2.txt",
                              SUBSTRING(ipcFileName, 1, R-INDEX(ipcFilename, ".") - 1),
                              STRING(ipiRound * 100 + ipiUnit, "99999999")).
   OUTPUT TO VALUE(opcOutputFile).
   PUT UNFORMATTED 
      SUBSTITUTE("After &1 rounds:", ipiRound) SKIP.

   DO iY = ipiMinY TO ipiMaxY:
      cLine = "".
      DO iX = ipiMinX TO ipiMaxX:
         FIND  ttBoard
         WHERE ttBoard.iX EQ iX
         AND   ttBoard.iY EQ iY NO-ERROR.
         IF AVAILABLE ttBoard THEN DO:
            cChar = ttBoard.cType.
         END.
         ELSE DO:
            cChar = " ".
         END.
         cLine = SUBSTITUTE("&1&2", cLine, cChar).
      END.
      FOR EACH ttUnit
      WHERE ttUnit.iY EQ iY
      BY ttUnit.iX:
         cLine = SUBSTITUTE("&1 &2(&3)",cLine, ttUnit.cType, ttUnit.iHitPoints).
      END.

      PUBLISH "debug" (OUTPUT cLine).
      PUT UNFORMATTED
         cLine SKIP.
   END.
   PUT UNFORMATTED 
      SKIP(1).
   OUTPUT CLOSE.
   
   PUBLISH "debug" (SUBSTITUTE("FILE:&1", opcOutputFile)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-playMarbleGame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE playMarbleGame Procedure 
PROCEDURE playMarbleGame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInput      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiHighscore  AS INT64       NO-UNDO.

DEFINE BUFFER ttCurrentMarble  FOR ttMarble.
DEFINE BUFFER ttPrevMarble     FOR ttMarble.
DEFINE BUFFER ttNextMarble     FOR ttMarble.
DEFINE BUFFER ttNextNextMarble FOR ttMarble.
DEFINE BUFFER ttPrevNextMarble FOR ttMarble.
DEFINE BUFFER ttPrevPrevMarble FOR ttMarble.

DEFINE VARIABLE ipiPlayers    AS INTEGER     NO-UNDO.
DEFINE VARIABLE ipiLastMarble AS INTEGER     NO-UNDO.

DEFINE VARIABLE iNewID     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCurrentID AS INTEGER     NO-UNDO.
DEFINE VARIABLE iValue     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPlayer    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBackID    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBack      AS INTEGER     NO-UNDO.

   EMPTY TEMP-TABLE ttPlayer.
   EMPTY TEMP-TABLE ttMarble.

   ASSIGN
      ipiPlayers    = INTEGER(ENTRY(1, ipcInput, " "))
      ipiLastMarble = INTEGER(ENTRY(7, ipcInput, " "))
   .

   /* Create Players */
   DO iPlayer = 1 TO ipiPlayers:
      CREATE ttPlayer.
      ASSIGN
         ttPlayer.iPlayerID = iPlayer
         ttPlayer.iScore    = 0
      .
   END.
   /* Create First marble */
   iNewID = iNewID + 1.
   CREATE ttMarble.
   ASSIGN
      ttMarble.iID      = iNewID
      ttMarble.iID_Prev = ttMarble.iID
      ttMarble.iID_Next = ttMarble.iID
      ttMarble.iValue   = iValue
   .
   ASSIGN
      iCurrentID = ttMarble.iID
      iPlayer    = 1
      iValue     = 1
   .

   PlaceBlock:
   REPEAT:
      FIND ttCurrentMarble  WHERE ttCurrentMarble.iID EQ iCurrentID.

      IF iValue MOD 23 EQ 0 THEN DO:
         /* Special move */
         FIND ttPlayer WHERE ttPlayer.iPlayer EQ iPlayer.
         ASSIGN
            ttPlayer.iScore = ttPlayer.iScore + iValue
         .
         
         iBackID = ttCurrentMarble.iID_Prev.
         DO iBack = 1 TO 7:
            FIND ttPrevMarble WHERE ttPrevMarble.iID EQ iBackID.
            iBackID = ttPrevMarble.iID_Prev.
         END.
         /* Remove Marble */
         FIND ttPrevNextMarble WHERE ttPrevNextMarble.iID EQ ttPrevMarble.iID_Next.
         FIND ttPrevPrevMarble WHERE ttPrevPrevMarble.iId EQ ttPrevMarble.iID_Prev.
         ASSIGN
            ttPrevNextMarble.iID_Prev = ttPrevPrevMarble.iID
            ttPrevPrevMarble.iID_Next = ttPrevNextMarble.iID
         .
         ASSIGN
            ttPlayer.iScore = ttPlayer.iScore + ttPrevMarble.iValue
         .
         ASSIGN
            iCurrentID = ttPrevMarble.iID_Next
         .
         DELETE ttPrevMarble.
      END. /* Special move */
      ELSE DO:
         /* Normal move */
         /* Place next marble between ttNext and ttNextNext */
         FIND ttNextMarble     WHERE ttNextMarble.iID     EQ ttCurrentMarble.iID_Next.
         FIND ttNextNextMarble WHERE ttNextNextMarble.iID EQ ttNextMarble.iID_Next.
         iNewID = iNewID + 1.
         CREATE ttMarble.
         ASSIGN
            ttMarble.iID      = iNewID
            ttMarble.iValue   = iValue
         .
         ASSIGN
            ttNextMarble.iID_Next = ttMarble.iID
            ttNextNextMarble.iID_Prev = ttMarble.iID
         .
         ASSIGN
            ttMarble.iID_Prev = ttNextMarble.iID
            ttMarble.iID_Next = ttNextNextMarble.iID
         .
         ASSIGN
            iCurrentID = ttMarble.iID
         .
      END. /* Normal move */
      iPlayer = iPlayer + 1.
      IF iPlayer GT ipiPlayers THEN DO:
         iPlayer = 1.
      END.

      IF iValue EQ ipiLastMarble THEN DO:
         LEAVE PlaceBlock.
      END.
      
      ASSIGN
         iValue     = iValue + 1
      .

   END. /* PlaceBlock: */

   FOR EACH ttPlayer
   BY ttPlayer.iScore DESCENDING:
      ASSIGN
         opiHighScore = ttPlayer.iScore
      .
      LEAVE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getNodeValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeValue Procedure 
FUNCTION getNodeValue RETURNS INTEGER
(  /* parameter-definitions */ 
   INPUT ipiID AS INTEGER
) :
/*------------------------------------------------------------------------------
  Purpose:  Calcluate Node value
    Notes:  
   /* Value is the sum of the metadata entries is Node has 0 child nodes */
   /* Value is the sum of the sub nodes if the Node has child nodes */
    
------------------------------------------------------------------------------*/
DEFINE BUFFER ttNode    FOR ttNode.
DEFINE BUFFER ttSubNode FOR ttNode.

DEFINE VARIABLE iSubNode  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cMetadata AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMetadata AS INTEGER     NO-UNDO.


   FIND ttNode WHERE ttNode.iID EQ ipiID NO-ERROR.
   IF NOT AVAILABLE ttNode THEN DO:
      RETURN 0.
   END.
   ELSE DO:
      IF ttNode.iValue EQ 0 THEN DO:
         IF ttNode.iNrChildNodes EQ 0 THEN DO:
            ttNode.iValue = ttNode.iSumMetadata.
         END.
         ELSE DO:
            /* Calculate first time */
            DO iSubNode = 1 TO NUM-ENTRIES(ttNode.cMetadata):
               cMetadata = ENTRY(iSubNode, ttNode.cMetadata).
               ASSIGN
                  iMetadata = INTEGER(cMetadata)
               NO-ERROR.
               IF ERROR-STATUS:ERROR EQ FALSE THEN DO:
                  FIND  ttSubNode
                  WHERE ttSubNode.iID_Parent EQ ttNode.iID
                  AND   ttSubNode.iChildNr   EQ iMetadata NO-ERROR.
                  IF AVAILABLE ttSubNode THEN DO:
                     ttNode.iValue = ttNode.iValue +
                        getNodeValue(ttSubNode.iID).
                  END.
               END.
            END.
         END.
      END.
      RETURN ttNode.iValue.
   END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPatternIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPatternIn Procedure 
FUNCTION getPatternIn RETURNS CHARACTER
(  /* parameter-definitions */ 
   INPUT ipiGeneration AS INTEGER,
   INPUT ipiPotNr      AS INTEGER
) :
/*------------------------------------------------------------------------------
  Purpose:  Returns Pattern around a Pot in a Generation
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER ttPot       FOR ttPot.
DEFINE BUFFER ttPotLeft1  FOR ttPot.
DEFINE BUFFER ttPotLeft2  FOR ttPot.
DEFINE BUFFER ttPotRight1 FOR ttPot.
DEFINE BUFFER ttPotRight2 FOR ttPot.
                          
DEFINE VARIABLE cPattern AS CHARACTER   NO-UNDO EXTENT 5.

   FIND ttPot       WHERE ttPot.iGeneration       EQ ipiGeneration AND ttPot.iPotNr       EQ ipiPotNr     NO-ERROR.
   FIND ttPotLeft1  WHERE ttPotLeft1.iGeneration  EQ ipiGeneration AND ttPotLeft1.iPotNr  EQ ipiPotNr - 1 NO-ERROR.
   FIND ttPotLeft2  WHERE ttPotLeft2.iGeneration  EQ ipiGeneration AND ttPotLeft2.iPotNr  EQ ipiPotNr - 2 NO-ERROR.
   FIND ttPotRight1 WHERE ttPotRight1.iGeneration EQ ipiGeneration AND ttPotRight1.iPotNr EQ ipiPotNr + 1 NO-ERROR.
   FIND ttPotRight2 WHERE ttPotRight2.iGeneration EQ ipiGeneration AND ttPotRight2.iPotNr EQ ipiPotNr + 2 NO-ERROR.

   IF NOT AVAILABLE ttPot THEN DO:
      CREATE ttPot.
      ASSIGN
         ttPot.iGeneration = ipiGeneration
         ttPot.iPotNr      = ipiPotNr
         ttPot.cPlant      = "."
      .
   END.
   IF NOT AVAILABLE ttPotLeft1 THEN DO:
      CREATE ttPotLeft1.
      ASSIGN
         ttPotLeft1.iGeneration = ipiGeneration
         ttPotLeft1.iPotNr      = ipiPotNr - 1
         ttPotLeft1.cPlant      = "."
      .
   END.
   IF NOT AVAILABLE ttPotLeft2 THEN DO:
      CREATE ttPotLeft2.
      ASSIGN
         ttPotLeft2.iGeneration = ipiGeneration
         ttPotLeft2.iPotNr      = ipiPotNr - 2
         ttPotLeft2.cPlant      = "."
      .
   END.
   IF NOT AVAILABLE ttPotRight1 THEN DO:
      CREATE ttPotRight1.
      ASSIGN
         ttPotRight1.iGeneration = ipiGeneration
         ttPotRight1.iPotNr      = ipiPotNr + 1
         ttPotRight1.cPlant      = "."
      .
   END.
   IF NOT AVAILABLE ttPotRight2 THEN DO:
      CREATE ttPotRight2.
      ASSIGN
         ttPotRight2.iGeneration = ipiGeneration
         ttPotRight2.iPotNr      = ipiPotNr + 2
         ttPotRight2.cPlant      = "."
      .
   END.
   ASSIGN
      cPattern    = "."
      cPattern[1] = "#" WHEN ttPotLeft2.cPlant  EQ "#"
      cPattern[2] = "#" WHEN ttPotLeft1.cPlant  EQ "#"
      cPattern[3] = "#" WHEN ttPot.cPlant       EQ "#"
      cPattern[4] = "#" WHEN ttPotRight1.cPlant EQ "#"
      cPattern[5] = "#" WHEN ttPotRight2.cPlant EQ "#"
   .

   RETURN SUBSTITUTE("&1&2&3&4&5", 
                     cPattern[1], 
                     cPattern[2], 
                     cPattern[3], 
                     cPattern[4], 
                     cPattern[5]).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

