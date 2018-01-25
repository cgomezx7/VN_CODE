      ******************************************************************00000010
      * DCLGEN TABLE(MDDT750)                                          *00000020
      *        LIBRARY(DES.GRDES.COPYLIB(D4462800))                    *00000030
      *        ACTION(REPLACE)                                         *00000040
      *        LANGUAGE(COBOL)                                         *00000050
      *        APOST                                                   *00000060
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00000070
      ******************************************************************00000080
           EXEC SQL DECLARE MDDT750 TABLE                               00000090
           ( G3177_CDGENTI                  CHAR(4) NOT NULL,           00000100
             G3177_CDGPRODU                 CHAR(3) NOT NULL,           00000110
             G3177_CDGSUBP                  CHAR(3) NOT NULL,           00000120
             G3177_PANUMPAR                 CHAR(3) NOT NULL,           00000130
             G3177_VALPARM                  CHAR(10) NOT NULL,          00000140
             G3177_FECALTA                  DATE NOT NULL,              00000150
             G3177_FEBAJA                   DATE NOT NULL,              00000160
             G3177_CDENTUMO                 CHAR(4) NOT NULL,           00000170
             G3177_CDOFIUMO                 CHAR(4) NOT NULL,           00000180
             G3177_USUARUMO                 CHAR(8) NOT NULL,           00000190
             G3177_CDTERUMO                 CHAR(8) NOT NULL,           00000200
             G3177_CONTCUR                  TIMESTAMP NOT NULL          00000210
           ) END-EXEC.                                                  00000220
      ******************************************************************00000230
      * COBOL DECLARATION FOR TABLE MDDT750                            *00000240
      ******************************************************************00000250
       01  DCLMDDT750.                                                  00000260
           10 G3177-CDGENTI        PIC X(4).                            00000270
           10 G3177-CDGPRODU       PIC X(3).                            00000280
           10 G3177-CDGSUBP        PIC X(3).                            00000290
           10 G3177-PANUMPAR       PIC X(3).                            00000300
           10 G3177-VALPARM        PIC X(10).                           00000310
           10 G3177-FECALTA        PIC X(10).                           00000320
           10 G3177-FEBAJA         PIC X(10).                           00000330
           10 G3177-CDENTUMO       PIC X(4).                            00000340
           10 G3177-CDOFIUMO       PIC X(4).                            00000350
           10 G3177-USUARUMO       PIC X(8).                            00000360
           10 G3177-CDTERUMO       PIC X(8).                            00000370
           10 G3177-CONTCUR        PIC X(26).                           00000380
      ******************************************************************00000390
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *00000400
      ******************************************************************00000410
