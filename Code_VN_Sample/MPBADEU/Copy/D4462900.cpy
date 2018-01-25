      ******************************************************************00000010
      * DCLGEN TABLE(MDDT755)                                          *00000020
      *        LIBRARY(DES.GRDES.COPYLIB(D4462900))                    *00000030
      *        ACTION(REPLACE)                                         *00000040
      *        LANGUAGE(COBOL)                                         *00000050
      *        APOST                                                   *00000060
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00000070
      ******************************************************************00000080
           EXEC SQL DECLARE MDDT755 TABLE                               00000090
           ( G3178_CDGENTI                  CHAR(4) NOT NULL,           00000100
             G3178_PANUMPAR                 CHAR(3) NOT NULL,           00000110
             G3178_VALPARM                  CHAR(10) NOT NULL,          00000120
             G3178_DESLARG                  CHAR(100) NOT NULL,         00000130
             G3178_FECALTA                  DATE NOT NULL,              00000140
             G3178_FEBAJA                   DATE NOT NULL,              00000150
             G3178_CDENTUMO                 CHAR(4) NOT NULL,           00000160
             G3178_CDOFIUMO                 CHAR(4) NOT NULL,           00000170
             G3178_USUARUMO                 CHAR(8) NOT NULL,           00000180
             G3178_CDTERUMO                 CHAR(8) NOT NULL,           00000190
             G3178_CONTCUR                  TIMESTAMP NOT NULL          00000200
           ) END-EXEC.                                                  00000210
      ******************************************************************00000220
      * COBOL DECLARATION FOR TABLE MDDT755                            *00000230
      ******************************************************************00000240
       01  DCLMDDT755.                                                  00000250
           10 G3178-CDGENTI        PIC X(4).                            00000260
           10 G3178-PANUMPAR       PIC X(3).                            00000270
           10 G3178-VALPARM        PIC X(10).                           00000280
           10 G3178-DESLARG        PIC X(100).                          00000290
           10 G3178-FECALTA        PIC X(10).                           00000300
           10 G3178-FEBAJA         PIC X(10).                           00000310
           10 G3178-CDENTUMO       PIC X(4).                            00000320
           10 G3178-CDOFIUMO       PIC X(4).                            00000330
           10 G3178-USUARUMO       PIC X(8).                            00000340
           10 G3178-CDTERUMO       PIC X(8).                            00000350
           10 G3178-CONTCUR        PIC X(26).                           00000360
      ******************************************************************00000370
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *00000380
      ******************************************************************00000390
