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
   FIELD iID        AS INTEGER
   FIELD iX_From    AS INTEGER
   FIELD iY_From    AS INTEGER
   FIELD iWidth     AS INTEGER
   FIELD iHeight    AS INTEGER
   FIELD iX_To      AS INTEGER
   FIELD iY_To      AS INTEGER
   FIELD iMaxOccupy AS INTEGER
INDEX indID IS UNIQUE iID
INDEX indXY IS PRIMARY iX_From iY_From iX_To iY_To.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-hasThrees) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hasThrees Procedure 
FUNCTION hasThrees RETURNS LOGICAL
(  /* parameter-definitions */ 
   INPUT ipcBoxID AS CHARACTER   
)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hasTwos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hasTwos Procedure 
FUNCTION hasTwos RETURNS LOGICAL
(  /* parameter-definitions */ 
   INPUT ipcBoxID AS CHARACTER   
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
         HEIGHT             = 15
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
   
         PUBLISH "debug" (SUBSTITUTE("&1.&2. &3 &4 &5", iLoop, iTerm, cTerm, iFrequency, AVAILABLE ttFrequency)).
   
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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-hasThrees) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hasThrees Procedure 
FUNCTION hasThrees RETURNS LOGICAL
(  /* parameter-definitions */ 
   INPUT ipcBoxID AS CHARACTER   
) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cOnes AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTwos AS CHARACTER   NO-UNDO.

   DO iChar = 1 TO LENGTH(ipcBoxID):
      cChar = SUBSTRING(ipcBoxID, iChar, 1).
      IF INDEX(cOnes, cChar) NE 0 THEN DO:
         IF INDEX(cTwos, cChar) NE 0 THEN DO:
            /* Third time this character --> hasThrees = true */
            RETURN TRUE.
         END.
         ELSE DO:
            cTwos = cTwos + cChar.
         END.
      END.
      ELSE DO:
         cOnes = cOnes + cChar.
      END.
   END.
   
   RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hasTwos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hasTwos Procedure 
FUNCTION hasTwos RETURNS LOGICAL
(  /* parameter-definitions */ 
   INPUT ipcBoxID AS CHARACTER   
) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.


DEFINE VARIABLE cOnes AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTwos AS CHARACTER   NO-UNDO.

   DO iChar = 1 TO LENGTH(ipcBoxID):
      cChar = SUBSTRING(ipcBoxID, iChar, 1).
      IF INDEX(cOnes, cChar) NE 0 THEN DO:
         /* Second time this character --> hasTwos = true */
         cTwos = cTwos + cChar.
      END.
      ELSE DO:
         cOnes = cOnes + cChar.
      END.
   END.
   
   RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

