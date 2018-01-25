      ******************************************************************00000010
      * DCLGEN TABLE(POS_DISP_PMAS)                                    *00000020
      *        LIBRARY(DES.GRDES.COPYLIB(D7397400))                    *00000030
      *        ACTION(REPLACE)                                         *00000040
      *        LANGUAGE(COBOL)                                         *00000050
      *        APOST                                                   *00000060
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00000070
      ******************************************************************00000080
           EXEC SQL DECLARE POS_DISP_PMAS TABLE                         00000090
           ( G6524_IDEMPRD                  CHAR(4) NOT NULL,           00000100
             G6524_IDCENTD                  CHAR(4) NOT NULL,           00000110
             G6524_IDPRODD                  CHAR(3) NOT NULL,           00000120
             G6524_CODSPROD                 CHAR(3) NOT NULL,           00000130
             G6524_CTOSALDO                 CHAR(3) NOT NULL,           00000140
             G6524_CODMONSW                 CHAR(3) NOT NULL,           00000150
             G6524_IMPSLDC                  DECIMAL(17, 2) NOT NULL,    00000160
             G6524_FECCONSO                 DATE NOT NULL,              00000170
             G6524_IDEMPCCO                 CHAR(4) NOT NULL,           00000180
             G6524_IDCENCCO                 CHAR(4) NOT NULL,           00000190
             G6524_CDENTUMO                 CHAR(4) NOT NULL,           00000200
             G6524_CDOFIUMO                 CHAR(4) NOT NULL,           00000210
             G6524_USUAUDIT                 CHAR(30) NOT NULL,          00000220
             G6524_CDTERUMO                 CHAR(8) NOT NULL,           00000230
             G6524_CONTCUR                  TIMESTAMP NOT NULL          00000240
           ) END-EXEC.                                                  00000250
      ******************************************************************00000260
      * COBOL DECLARATION FOR TABLE POS_DISP_PMAS                      *00000270
      ******************************************************************00000280
       01  DCLPOS-DISP-PMAS.                                            00000290
           10 G6524-IDEMPRD        PIC X(4).                            00000300
           10 G6524-IDCENTD        PIC X(4).                            00000310
           10 G6524-IDPRODD        PIC X(3).                            00000320
           10 G6524-CODSPROD       PIC X(3).                            00000330
           10 G6524-CTOSALDO       PIC X(3).                            00000340
           10 G6524-CODMONSW       PIC X(3).                            00000350
           10 G6524-IMPSLDC        PIC S9(15)V9(2) USAGE COMP-3.        00000360
           10 G6524-FECCONSO       PIC X(10).                           00000370
           10 G6524-IDEMPCCO       PIC X(4).                            00000380
           10 G6524-IDCENCCO       PIC X(4).                            00000390
           10 G6524-CDENTUMO       PIC X(4).                            00000400
           10 G6524-CDOFIUMO       PIC X(4).                            00000410
           10 G6524-USUAUDIT       PIC X(30).                           00000420
           10 G6524-CDTERUMO       PIC X(8).                            00000430
           10 G6524-CONTCUR        PIC X(26).                           00000440
      ******************************************************************00000450
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *00000460
      ******************************************************************00000470
