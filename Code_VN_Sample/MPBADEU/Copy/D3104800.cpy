      ******************************************************************00000010
      * DCLGEN TABLE(MPDT007)                                          *00000020
      *        LIBRARY(DES.GRDES.COPYLIB(D3104800))                    *00000030
      *        ACTION(REPLACE)                                         *00000040
      *        LANGUAGE(COBOL)                                         *00000050
      *        QUOTE                                                   *00000060
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00000070
      ******************************************************************00000080
           EXEC SQL DECLARE MPDT007 TABLE                               00000090
           ( E1003_CDGENTI                  CHAR(4) NOT NULL,           00000100
             E1003_CENTALTA                 CHAR(4) NOT NULL,           00000110
             E1003_CUENTNU                  CHAR(12) NOT NULL,          00000120
             E1003_FECALTA                  DATE NOT NULL,              00000130
             E1003_CDGPRODU                 CHAR(3) NOT NULL,           00000140
             E1003_CDGSUBP                  CHAR(3) NOT NULL,           00000150
             E1003_CONPROD                  CHAR(3) NOT NULL,           00000160
             E1003_CODGCAMP                 CHAR(6) NOT NULL,           00000170
             E1003_CDCONVEN                 CHAR(4) NOT NULL,           00000180
             E1003_FRMAPAGO                 DECIMAL(2, 0) NOT NULL,     00000190
             E1003_FEULTCAR                 DATE NOT NULL,              00000200
             E1003_FECBAJA                  DATE NOT NULL,              00000210
             E1003_MOTBAJA                  CHAR(2) NOT NULL,           00000220
             E1003_FOPAGTEM                 DECIMAL(2, 0) NOT NULL,     00000230
             E1003_FIFOPAGT                 DATE NOT NULL,              00000240
             E1003_FFFOPAGT                 DATE NOT NULL,              00000250
             E1003_CDESTCTA                 DECIMAL(2, 0) NOT NULL,     00000260
             E1003_FEULESCT                 DATE NOT NULL,              00000270
             E1003_INBLQOPE                 CHAR(1) NOT NULL,           00000280
             E1003_INBLQCON                 CHAR(1) NOT NULL,           00000290
             E1003_INCTAEMP                 DECIMAL(1, 0) NOT NULL,     00000300
             E1003_INNOMEMP                 DECIMAL(1, 0) NOT NULL,     00000310
             E1003_NOMREEMP                 CHAR(30) NOT NULL,          00000320
             E1003_INSITCTA                 DECIMAL(1, 0) NOT NULL,     00000330
             E1003_FERESOLU                 DATE NOT NULL,              00000340
             E1003_INCTTRAS                 CHAR(1) NOT NULL,           00000350
             E1003_CDPROLIQ                 DECIMAL(2, 0) NOT NULL,     00000360
             E1003_GRUPOLIQ                 DECIMAL(2, 0) NOT NULL,     00000370
             E1003_CDPROCUO                 DECIMAL(2, 0) NOT NULL,     00000380
             E1003_GRUPOCUO                 DECIMAL(2, 0) NOT NULL,     00000390
             E1003_REEMIEXT                 CHAR(12) NOT NULL,          00000400
             E1003_TIPBON                   CHAR(6) NOT NULL,           00000410
             E1003_CALPART                  CHAR(2) NOT NULL,           00000420
             E1003_NUMULPLA                 DECIMAL(12, 0) NOT NULL,    00000430
             E1003_CDGPROMO                 CHAR(3) NOT NULL,           00000440
             E1003_INPERCUO                 CHAR(1) NOT NULL,           00000450
             E1003_NUMESTOT                 DECIMAL(2, 0) NOT NULL,     00000460
             E1003_CDREGIME                 DECIMAL(4, 0) NOT NULL,     00000470
             E1003_NUMFACSC                 DECIMAL(2, 0) NOT NULL,     00000480
             E1003_NUMULMOD                 DECIMAL(9, 0) NOT NULL,     00000490
             E1003_TIPOPERS                 CHAR(1) NOT NULL,           00000500
             E1003_CLIIDENT                 DECIMAL(9, 0) NOT NULL,     00000510
             E1003_NUBENEF                  DECIMAL(5, 0) NOT NULL,     00000520
             E1003_OFIGESTO                 CHAR(4) NOT NULL,           00000530
             E1003_CDFORMAT                 CHAR(10) NOT NULL,          00000540
             E1003_INDOCACR                 CHAR(1) NOT NULL,           00000550
             E1003_CDENTUMO                 CHAR(4) NOT NULL,           00000560
             E1003_CDOFIUMO                 CHAR(4) NOT NULL,           00000570
             E1003_USUARUMO                 CHAR(8) NOT NULL,           00000580
             E1003_CDTERUMO                 CHAR(8) NOT NULL,           00000590
             E1003_CONTCUR                  TIMESTAMP NOT NULL          00000600
           ) END-EXEC.                                                  00000610
      ******************************************************************00000620
      * COBOL DECLARATION FOR TABLE MPDT007                            *00000630
      ******************************************************************00000640
       01  DCLMPDT007.                                                  00000650
           10 E1003-CDGENTI        PIC X(4).                            00000660
           10 E1003-CENTALTA       PIC X(4).                            00000670
           10 E1003-CUENTNU        PIC X(12).                           00000680
           10 E1003-FECALTA        PIC X(10).                           00000690
           10 E1003-CDGPRODU       PIC X(3).                            00000700
           10 E1003-CDGSUBP        PIC X(3).                            00000710
           10 E1003-CONPROD        PIC X(3).                            00000720
           10 E1003-CODGCAMP       PIC X(6).                            00000730
           10 E1003-CDCONVEN       PIC X(4).                            00000740
           10 E1003-FRMAPAGO       PIC S9(2)V USAGE COMP-3.             00000750
           10 E1003-FEULTCAR       PIC X(10).                           00000760
           10 E1003-FECBAJA        PIC X(10).                           00000770
           10 E1003-MOTBAJA        PIC X(2).                            00000780
           10 E1003-FOPAGTEM       PIC S9(2)V USAGE COMP-3.             00000790
           10 E1003-FIFOPAGT       PIC X(10).                           00000800
           10 E1003-FFFOPAGT       PIC X(10).                           00000810
           10 E1003-CDESTCTA       PIC S9(2)V USAGE COMP-3.             00000820
           10 E1003-FEULESCT       PIC X(10).                           00000830
           10 E1003-INBLQOPE       PIC X(1).                            00000840
           10 E1003-INBLQCON       PIC X(1).                            00000850
           10 E1003-INCTAEMP       PIC S9(1)V USAGE COMP-3.             00000860
           10 E1003-INNOMEMP       PIC S9(1)V USAGE COMP-3.             00000870
           10 E1003-NOMREEMP       PIC X(30).                           00000880
           10 E1003-INSITCTA       PIC S9(1)V USAGE COMP-3.             00000890
           10 E1003-FERESOLU       PIC X(10).                           00000900
           10 E1003-INCTTRAS       PIC X(1).                            00000910
           10 E1003-CDPROLIQ       PIC S9(2)V USAGE COMP-3.             00000920
           10 E1003-GRUPOLIQ       PIC S9(2)V USAGE COMP-3.             00000930
           10 E1003-CDPROCUO       PIC S9(2)V USAGE COMP-3.             00000940
           10 E1003-GRUPOCUO       PIC S9(2)V USAGE COMP-3.             00000950
           10 E1003-REEMIEXT       PIC X(12).                           00000960
           10 E1003-TIPBON         PIC X(6).                            00000970
           10 E1003-CALPART        PIC X(2).                            00000980
           10 E1003-NUMULPLA       PIC S9(12)V USAGE COMP-3.            00000990
           10 E1003-CDGPROMO       PIC X(3).                            00001000
           10 E1003-INPERCUO       PIC X(1).                            00001010
           10 E1003-NUMESTOT       PIC S9(2)V USAGE COMP-3.             00001020
           10 E1003-CDREGIME       PIC S9(4)V USAGE COMP-3.             00001030
           10 E1003-NUMFACSC       PIC S9(2)V USAGE COMP-3.             00001040
           10 E1003-NUMULMOD       PIC S9(9)V USAGE COMP-3.             00001050
           10 E1003-TIPOPERS       PIC X(1).                            00001060
           10 E1003-CLIIDENT       PIC S9(9)V USAGE COMP-3.             00001070
           10 E1003-NUBENEF        PIC S9(5)V USAGE COMP-3.             00001080
           10 E1003-OFIGESTO       PIC X(4).                            00001090
           10 E1003-CDFORMAT       PIC X(10).                           00001100
           10 E1003-INDOCACR       PIC X(1).                            00001110
           10 E1003-CDENTUMO       PIC X(4).                            00001120
           10 E1003-CDOFIUMO       PIC X(4).                            00001130
           10 E1003-USUARUMO       PIC X(8).                            00001140
           10 E1003-CDTERUMO       PIC X(8).                            00001150
           10 E1003-CONTCUR        PIC X(26).                           00001160
      ******************************************************************00001170
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 51      *00001180
      ******************************************************************00001190
