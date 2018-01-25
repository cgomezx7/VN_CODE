      ******************************************************************00000010
      *                                                                *00000020
      *  PROGRAMA :  MPCHSEL                                           *00000030
      *-                                                              -*00000040
      *  FECHA CREACION: 24/07/2017           AUTOR: VIEWNEXT.         *00000050
      *-                                                              -*00000060
      *  APLICACION: MP                                                *00000070
      *-                                                              -*00000080
      *  INSTALACION: ISBAN                                            *00000090
      *-                                                              -*00000100
      *  DESCRIPCION: PROCESO BATCH QUE SACA LAS TARJETAS QUE SE VAN   *00000110
      *               A REMITIR Y LAS QUE NO.                          *00000120
      *-                                                              -*00000130
      *  FICHEROS DE ENTRADA:                                          *00000180
      *        MPCHSEE1 : FICHERO DE ENTRADA CON LAS TARJETAS.         *00000190
      *-                                                              -*00000200
      *  FICHEROS DE SALIDA:                                           *00000210
      *        MPCHSES1 : FICHERO DE SALIDA CON LAS TARJETAS A         *00000220
      *                   REMITIR.                                     *00000230
      *        MPCHSES2 : FICHERO DE SALIDA CON LAS TARJETAS SIN       *00000220
      *                   REMITIR.                                     *00000230
      *-                                                              -*00000200
      *  RUTINAS Y MODULOS:                                            *00000250
      *        XX_CANCELACION_PROCESOS_BATCH: FUNCIÛN PARA LA          *00000260
      *                   CANCELACIÛN DEL PROGRAMA CON ERROR.          *00000270
      ******************************************************************00000290
      *               M O D I F I C A C I O N E S                      *00000300
      *               ***************************                      *00000310
      *                                                                *00000320
      * USUARIO    FECHA             DESCRIPCIÛN                       *00000330
      * -------- ----------   ---------------------------------------- *00000340
      ******************************************************************00000350
                                                                        00000360
      ******************************************************************00000370
      * IDENTIFICATION DIVISION                                        *00000380
      ******************************************************************00000390
       IDENTIFICATION DIVISION.                                         00000400
       PROGRAM-ID.    MPCHSEL.                                          00000410
       AUTHOR.        VIEWNEXT.                                         00000420
       DATE-WRITTEN.  24/07/2017.                                       00000430
       DATE-COMPILED.                                                   00000440
                                                                        00000450
      ******************************************************************00000460
      * ENVIRONMENT DIVISION                                           *00000470
      ******************************************************************00000480
       ENVIRONMENT DIVISION.                                            00000490
                                                                        00000500
      *----------------------------------------------------------------*00000510
      * CONFIGURATION SECTION                                          *00000520
      *----------------------------------------------------------------*00000530
       CONFIGURATION SECTION.                                           00000540
                                                                        00000550
       SOURCE-COMPUTER. IBM-3090.                                       00000560
       OBJECT-COMPUTER. IBM-3090.                                       00000570
       SPECIAL-NAMES.                                                   00000580
           DECIMAL-POINT IS COMMA.                                      00000590
                                                                        00000600
      *----------------------------------------------------------------*00000610
      * INPUT OUTPUT SECTION                                           *00000620
      *----------------------------------------------------------------*00000630
       INPUT-OUTPUT SECTION.                                            00000640
       FILE-CONTROL.                                                    00000650
                                                                        00000660
      * -- FICHERO DE ENTRADA CON LAS TARJETAS.                         00000670
           SELECT MPCHSEE1 ASSIGN MPCHSEE1                              00000680
                  ACCESS MODE IS SEQUENTIAL                             00000690
                  FILE STATUS IS FS-MPCHSEE1.                           00000700
                                                                        00000710
      * -- FICHERO DE SALIDA CON LAS TARJETAS A REMITIR.                00000720
           SELECT MPCHSES1 ASSIGN MPCHSES1                              00000740
                  ACCESS MODE IS SEQUENTIAL                             00000750
                  FILE STATUS IS FS-MPCHSES1.                           00000760
                                                                        00000770
      * -- FICHERO DE SALIDA CON LAS TARJETAS SIN REMITIR.              00000720
           SELECT MPCHSES2 ASSIGN MPCHSES2                              00000740
                  ACCESS MODE IS SEQUENTIAL                             00000750
                  FILE STATUS IS FS-MPCHSES2.                           00000760
                                                                        00000770
      ******************************************************************00000780
      * DATA DIVISION                                                  *00000790
      ******************************************************************00000800
       DATA DIVISION.                                                   00000810
                                                                        00000820
      *----------------------------------------------------------------*00000830
      * FILE SECTION                                                   *00000840
      *----------------------------------------------------------------*00000850
       FILE SECTION.                                                    00000860
                                                                        00000870
       FD  MPCHSEE1                                                     00000880
           BLOCK CONTAINS 0 RECORDS                                     00000890
           RECORDING MODE IS F                                          00000900
           LABEL RECORD ARE STANDARD                                    00000910
           RECORD CONTAINS 651 CHARACTERS                               00000920
           DATA RECORD IS REG-MPCHSEE1.                                 00000930
       01  REG-MPCHSEE1                    PIC X(651).                  00000940
                                                                        00000950
       FD  MPCHSES1                                                     00000960
           BLOCK CONTAINS 0 RECORDS                                     00000970
           RECORDING MODE IS F                                          00000980
           LABEL RECORD ARE STANDARD                                    00000990
           RECORD CONTAINS 651 CHARACTERS                               00001000
           DATA RECORD IS REG-MPCHSES1.                                 00001010
       01  REG-MPCHSES1                    PIC X(651).                  00001020
                                                                        00001030
       FD  MPCHSES2                                                     00000960
           BLOCK CONTAINS 0 RECORDS                                     00000970
           RECORDING MODE IS F                                          00000980
           LABEL RECORD ARE STANDARD                                    00000990
           RECORD CONTAINS 651 CHARACTERS                               00001000
           DATA RECORD IS REG-MPCHSES2.                                 00001010
       01  REG-MPCHSES2                    PIC X(651).                  00001020
                                                                        00001030
      *----------------------------------------------------------------*00001040
      * WORKING-STORAGE SECTION                                        *00001050
      *----------------------------------------------------------------*00001060
       WORKING-STORAGE SECTION.                                         00001070
                                                                        00001090
      ******************************************************************00001100
      *                        S W I T C H E S                         *00001110
      ******************************************************************00001120
       01  SW-SWITCHES.                                                 00001130
           05  SW-FIN-MPCHSEE1             PIC X(01)   VALUE 'N'.       00001260
               88  SI-FIN-MPCHSEE1                     VALUE 'S'.       00001270
               88  NO-FIN-MPCHSEE1                     VALUE 'N'.       00001280
                                                                        00001290
      ******************************************************************00001300
      *                      C O N S T A N T E S                       *00001310
      ******************************************************************00001320
       01  CT-CONSTANTES.                                               00001330
           05  CA-CONSTANTES-ALFANUMERICAS.                             00001340
               10  CA-FS-OK                PIC X(02)   VALUE '00'.      00001350
               10  CA-FS-EOF               PIC X(02)   VALUE '10'.      00001360
               10  CA-MPCHSEL              PIC X(08)   VALUE 'MPCHSEL'. 00001420
               10  CA-RESP                 PIC X(014)  VALUE 'MEDIOS DE 00001450
      -                            'PAGO'.                              00001460
      *TIPOS DE ERRORES DE CANCELACION.                                 00001470
               10  CA-ERROR-F              PIC X(01)   VALUE 'F'.       00001480
               10  CA-ERROR-D              PIC X(01)   VALUE 'D'.       00001490
               10  CA-ERROR-R              PIC X(01)   VALUE 'R'.       00001500
      *FICHEROS                                                         00001510
               10  CA-MPCHSEE1             PIC X(08)   VALUE 'MPCHSEE1'.00001520
               10  CA-MPCHSES1             PIC X(08)   VALUE 'MPCHSES1'.00001530
               10  CA-MPCHSES2             PIC X(08)   VALUE 'MPCHSES2'.00001530
      *ERRORES                                                          00001560
               10  CA-ERR-ABRIR-MPCHSEE1   PIC X(34)   VALUE 'ERROR AL A00001570
      -                            'BRIR EL FICHERO MPCHSEE1'.          00001580
               10  CA-ERR-ABRIR-MPCHSES1   PIC X(34)   VALUE 'ERROR AL A00001590
      -                            'BRIR EL FICHERO MPCHSES1'.          00001600
               10  CA-ERR-ABRIR-MPCHSES2   PIC X(34)   VALUE 'ERROR AL A00001590
      -                            'BRIR EL FICHERO MPCHSES2'.          00001600
               10  CA-ERR-CERRAR-MPCHSEE1  PIC X(35)   VALUE 'ERROR AL C00001610
      -                            'ERRAR EL FICHERO MPCHSEE1'.         00001620
               10  CA-ERR-CERRAR-MPCHSES1  PIC X(35)   VALUE 'ERROR AL C00001630
      -                            'ERRAR EL FICHERO MPCHSES1'.         00001640
               10  CA-ERR-CERRAR-MPCHSES2  PIC X(35)   VALUE 'ERROR AL C00001630
      -                            'ERRAR EL FICHERO MPCHSES2'.         00001640
               10  CA-ERR-LEER-MPCHSEE1    PIC X(33)   VALUE 'ERROR AL L00001650
      -                            'EER EL FICHERO MPCHSEE1'.           00001660
               10  CA-ERR-ESCRIB-MPCHSES1  PIC X(34)   VALUE 'ERROR AL E00001670
      -                            'SCRIBIR FICHERO MPCHSES1'.          00001680
               10  CA-ERR-ESCRIB-MPCHSES2  PIC X(34)   VALUE 'ERROR AL E00001670
      -                            'SCRIBIR FICHERO MPCHSES2'.          00001680
      *PARRAFOS                                                         00001760
               10  CA-PRF-1100             PIC X(19)   VALUE '1100-ABRIR00001770
      -                            '-FICHEROS'.                         00001780
               10  CA-PRF-2100             PIC X(22)   VALUE '2100-ESCRI00001810
      -                            'BIR-MPCHSES1'.                      00001820
               10  CA-PRF-2200             PIC X(22)   VALUE '2200-ESCRI00001810
      -                            'BIR-MPCHSES2'.                      00001820
               10  CA-PRF-9100             PIC X(18)   VALUE '9100-LEER-00001830
      -                            'MPCHSEE1'.                          00001840
               10  CA-PRF-3100             PIC X(20)   VALUE '3100-CERRA00001850
      -                            'R-FICHEROS'.                        00001860
                                                                        00001870
           05  CN-CONSTANTES-NUMERICAS.                                 00001880
               10  CN-1                    PIC 9(01)   VALUE 1.         00001890
               10  CN-4                    PIC 9(01)   VALUE 4.         00001890
               10  CN-5                    PIC 9(01)   VALUE 5.         00001890
                                                                        00001920
      ******************************************************************00001930
      *                    C O N T A D O R E S                         *00001940
      ******************************************************************00001950
       01  CT-CONTADORES.                                               00001960
      *--  CONTADORES PARA MOSTRAR ESTADISTICA FINAL.                   00001970
           05  CT-REG-MPCHSEE1             PIC 9(9).                    00001980
           05  CT-REG-MPCHSES1             PIC 9(9).                    00001990
           05  CT-REG-MPCHSES2             PIC 9(9).                    00001990
                                                                        00002000
      ******************************************************************00002010
      * VARIABLES PARA DISPLAYAR ESTADISTICAS DEL PROGRAMA             *00002020
      ******************************************************************00002030
       01  WK-ESTADISTICA.                                              00002040
           05  WK-CAB-1                    PIC X(55)   VALUE ALL '*'.   00002050
           05  WK-CAB-2                    PIC X(55)   VALUE '* ESTADIST00002060
      -                            'ICAS DE MPCHSEL                     00002070
      -                            '        *'.                         00002080
           05  WK-CAB-3.                                                00002090
               10  FILLER                  PIC X(45)   VALUE '* REGISTRO00002100
      -                            'S LEIDOS EN MPCHSEE1:            '. 00002110
               10  WK-LEIDOS-ED            PIC ZZZZZZZZ9.               00002120
               10  FILLER                  PIC X(01)   VALUE '*'.       00002130
                                                                        00002140
           05  WK-CAB-4.                                                00002150
               10  FILLER                  PIC X(45)   VALUE '* REGISTRO00002160
      -                            'S ESCRITOS EN MPCHSES1:           '.00002170
               10  WK-ESCRI1-ED            PIC ZZZZZZZZ9.               00002180
               10  FILLER                  PIC X(01)   VALUE '*'.       00002190
                                                                        00002200
           05  WK-CAB-5.                                                00002150
               10  FILLER                  PIC X(45)   VALUE '* REGISTRO00002160
      -                            'S ESCRITOS EN MPCHSES2:           '.00002170
               10  WK-ESCRI2-ED            PIC ZZZZZZZZ9.               00002180
               10  FILLER                  PIC X(01)   VALUE '*'.       00002190
                                                                        00002210
      ******************************************************************00002330
      * VARIABLES DE INFORMACION DE LA FUNCION                         *00002340
      * XX_CANCELACION_PROCESOS_BATCH.                                 *00002350
      ******************************************************************00002360
       01  WK-CANCELACION.                                              00002370
      *--  PARAMETROS GENERALES.                                        00002380
           05  WK-TIPO-ERROR               PIC X(01)   VALUE SPACES.    00002390
           05  WK-COD-RETORNO              PIC X(04)   VALUE SPACES.    00002400
           05  WK-RESPONSABLE              PIC X(30)   VALUE SPACES.    00002410
           05  WK-DESCRIPCION              PIC X(80)   VALUE SPACES.    00002420
           05  WK-PROGRAMA                 PIC X(08)   VALUE 'MPCHSEL'. 00002430
           05  WK-PARRAFO                  PIC X(30)   VALUE SPACES.    00002440
           05  WK-RUTINA                   PIC X(30)   VALUE SPACES.    00002450
           05  WK-PARAMETROS               PIC X(30)   VALUE SPACES.    00002460
      *--  ERRORES DE FICHEROS.                                         00002470
           05  WK-ERROR-FICHERO.                                        00002480
               10  WK-DDNAME               PIC X(08)   VALUE SPACES.    00002490
               10  WK-FILE-STATUS          PIC X(02)   VALUE SPACES.    00002500
               10  WK-DATOS-REGISTRO       PIC X(1200) VALUE SPACES.    00002510
      *--  ERRORES DE DB2.                                              00002520
           05  WK-ERROR-DB2.                                            00002530
               10  WK-SQLCA                PIC X(148)  VALUE SPACES.    00002540
               10  WK-TABLA-DB2            PIC X(18)   VALUE SPACES.    00002550
               10  WK-DATOS-ACCESO         PIC X(104)  VALUE SPACES.    00002560
                                                                        00002570
      ******************************************************************00002580
      *                      F I L E  S T A T U S                      *00002590
      ******************************************************************00002600
       01  FS-FILE-STATUS.                                              00002610
           05  FS-MPCHSEE1                 PIC X(02).                   00002620
           05  FS-MPCHSES1                 PIC X(02).                   00002630
           05  FS-MPCHSES2                 PIC X(02).                   00002630
                                                                        00002770
      ******************************************************************00002580
      *                      V A R I A B L E S                         *00002590
      ******************************************************************00002600
       01  WK-VARIABLES.
           05  WK-REGISTRO.
               10  WK-CDGENTI-009          PIC X(04).                      00000
               10  WK-CDGMAR-009           PIC S9(02)V USAGE COMP-3.       00000
               10  WK-INDTIPT-009          PIC S9(02)V USAGE COMP-3.       00000
               10  WK-CENTALTA-009         PIC X(04).                      00000
               10  WK-CUENTNU-009          PIC X(12).                      00000
               10  WK-NUBENCT-009          PIC S9(05)V USAGE COMP-3.       00000
               10  WK-NUPLASTI-009         PIC S9(12)V USAGE COMP-3.       00000
               10  WK-TIPBON-009           PIC X(06).                      00000
               10  WK-XPAN-009             PIC X(22).                      00000
               10  WK-XPANANT-009          PIC X(22).
               10  WK-FECALTA-009          PIC X(10).
               10  WK-FECALTAN-009         PIC X(10).
               10  WK-FEULTUSO-009         PIC X(10).
               10  WK-INSITTAR-009         PIC S9(02)V USAGE COMP-3.
               10  WK-INULTTAR-009         PIC X(02).
               10  WK-INDNOREN-009         PIC S9(01)V USAGE COMP-3.
               10  WK-CODBLQ-009           PIC S9(02)V USAGE COMP-3.
               10  WK-FEULTBLQ-009         PIC X(10).
               10  WK-TEXBLQ-009           PIC X(30).
               10  WK-FECBAJA-009          PIC X(10).
               10  WK-MOTBAJA-009          PIC X(02).
               10  WK-CADTARJ-009          PIC S9(06)V USAGE COMP-3.
               10  WK-CADANTTJ-009         PIC S9(06)V USAGE COMP-3.
               10  WK-FECBCUO-009          PIC S9(06)V USAGE COMP-3.
               10  WK-OFFSET1-009          PIC S9(04)V USAGE COMP-3.
               10  WK-OFFSETAN-009         PIC S9(04)V USAGE COMP-3.
               10  WK-OFFSET2-009          PIC S9(04)V USAGE COMP-3.
               10  WK-XPVV1-009            PIC X(04).
               10  WK-XPVV2-009            PIC X(04).
               10  WK-NUMACT1-009          PIC S9(01)V USAGE COMP-3.
               10  WK-NUMACT2-009          PIC S9(01)V USAGE COMP-3.
               10  WK-NUMANT1-009          PIC S9(01)V USAGE COMP-3.
               10  WK-NUMANT2-009          PIC S9(01)V USAGE COMP-3.
               10  WK-INCAMLIM-009         PIC X(01).
               10  WK-INACTLIM-009         PIC X(01).
               10  WK-INRESACT-009         PIC X(01).
               10  WK-INRESCOM-009         PIC X(01).
               10  WK-INLIMFAC-009         PIC X(01).
               10  WK-VISAPHON-009         PIC S9(04)V USAGE COMP-3.
               10  WK-NUMKPIN-009          PIC S9(02)V USAGE COMP-3.
               10  WK-NUMKPIN2-009         PIC S9(02)V USAGE COMP-3.
               10  WK-IDEMPLEA-009         PIC X(20).
               10  WK-NOMENRED-009         PIC X(26).
               10  WK-INDFOTO-009          PIC X(01).
               10  WK-REFFOTO-009          PIC X(30).
               10  WK-CODBARR-009          PIC S9(14)V USAGE COMP-3.
               10  WK-CDGPROMO-009         PIC X(03).
               10  WK-INEXECUO-009         PIC X(01).
               10  WK-INTAREME-009         PIC X(01).
               10  WK-INAPRETA-009         PIC X(01).
               10  WK-ISSUENUM-009         PIC S9(03)V USAGE COMP-3.
               10  WK-ISSUEANT-009         PIC S9(03)V USAGE COMP-3.
               10  WK-XPINOFF-009          PIC X(01).
               10  WK-IMGARAN-009          PIC S9(15)V9(2) USAGE COMP-3.
               10  WK-CDENTUMO-009         PIC X(04).
               10  WK-CDOFIUMO-009         PIC X(04).
               10  WK-USUARUMO-009         PIC X(08).
               10  WK-CDTERUMO-009         PIC X(08).
               10  WK-CONTCUR-009          PIC X(26).
               10  WK-CDGENTI-007          PIC X(04).
               10  WK-CENTALTA-007         PIC X(04).
               10  WK-CUENTNU-007          PIC X(12).
               10  WK-FECALTA-007          PIC X(10).
               10  WK-CDGPRODU-007         PIC X(03).
               10  WK-CDGSUBP-007          PIC X(03).
               10  WK-CONPROD-007          PIC X(03).
               10  WK-CODGCAMP-007         PIC X(06).
               10  WK-CDCONVEN-007         PIC X(04).
               10  WK-FRMAPAGO-007         PIC S9(02)V USAGE COMP-3.
               10  WK-FEULTCAR-007         PIC X(10).
               10  WK-FECBAJA-007          PIC X(10).
               10  WK-MOTBAJA-007          PIC X(02).
               10  WK-FOPAGTEM-007         PIC S9(02)V USAGE COMP-3.
               10  WK-FIFOPAGT-007         PIC X(10).
               10  WK-FFFOPAGT-007         PIC X(10).
               10  WK-CDESTCTA-007         PIC S9(02)V USAGE COMP-3.
               10  WK-FEULESCT-007         PIC X(10).
               10  WK-INBLQOPE-007         PIC X(01).
               10  WK-INBLQCON-007         PIC X(01).
               10  WK-INCTAEMP-007         PIC S9(01)V USAGE COMP-3.
               10  WK-INNOMEMP-007         PIC S9(01)V USAGE COMP-3.
               10  WK-NOMREEMP-007         PIC X(30).
               10  WK-INSITCTA-007         PIC S9(01)V USAGE COMP-3.
               10  WK-FERESOLU-007         PIC X(10).
               10  WK-INCTTRAS-007         PIC X(01).
               10  WK-CDPROLIQ-007         PIC S9(02)V USAGE COMP-3.
               10  WK-GRUPOLIQ-007         PIC S9(02)V USAGE COMP-3.
               10  WK-CDPROCUO-007         PIC S9(02)V USAGE COMP-3.
               10  WK-GRUPOCUO-007         PIC S9(02)V USAGE COMP-3.
               10  WK-REEMIEXT-007         PIC X(12).
               10  WK-TIPBON-007           PIC X(06).
               10  WK-CALPART-007          PIC X(02).
               10  WK-NUMULPLA-007         PIC S9(12)V USAGE COMP-3.
               10  WK-CDGPROMO-007         PIC X(03).
               10  WK-INPERCUO-007         PIC X(01).
               10  WK-NUMESTOT-007         PIC S9(02)V USAGE COMP-3.
               10  WK-CDREGIME-007         PIC S9(04)V USAGE COMP-3.
               10  WK-NUMFACSC-007         PIC S9(02)V USAGE COMP-3.
               10  WK-NUMULMOD-007         PIC S9(09)V USAGE COMP-3.
               10  WK-TIPOPERS-007         PIC X(01).
               10  WK-CLIIDENT-007         PIC S9(09)V USAGE COMP-3.
               10  WK-NUBENEF-007          PIC S9(05)V USAGE COMP-3.
               10  WK-OFIGESTO-007         PIC X(04).
               10  WK-CDFORMAT-007         PIC X(10).
               10  WK-INDOCACR-007         PIC X(01).
               10  WK-CDENTUMO-007         PIC X(04).
               10  WK-CDOFIUMO-007         PIC X(04).
               10  WK-USUARUMO-007         PIC X(08).
               10  WK-CDTERUMO-007         PIC X(08).
               10  WK-CONTCUR-007          PIC X(26).

           05  WK-BLZ-CORTO                PIC X(05).

      ******************************************************************00002780
      * LINKAGE SECTION                                                *00002790
      ******************************************************************00002800
       LINKAGE SECTION.                                                 00002810
                                                                        00002820
      ******************************************************************00002830
      *           P R O C E D U R E   D I V I S I O N                  *00002840
      ******************************************************************00002850
       PROCEDURE DIVISION.                                              00002860
                                                                        00002870
           PERFORM 1000-INICIO                                          00002880
              THRU 1000-INICIO-EXIT                                     00002890
                                                                        00002900
           PERFORM 2000-PROCESO                                         00002910
              THRU 2000-PROCESO-EXIT                                    00002920
             UNTIL SI-FIN-MPCHSEE1                                      00002930
                                                                        00002940
           PERFORM 3000-FIN                                             00002950
              THRU 3000-FIN-EXIT                                        00002960
                                                                        00002970
           .                                                            00002980
                                                                        00002990
      ******************************************************************00003000
      ***                   1000-INICIO                              ***00003010
      ***                   -----------                              ***00003020
      * SE INICIALIZAN LAS VARIABLES, SE ABREN LOS FICHEROS DE ENTRADA *00003030
      * Y SALIDA Y SE REALIZA LA PRIMERA LECTURA.                      *00003040
      ******************************************************************00003050
       1000-INICIO.                                                     00003060
                                                                        00003070
           INITIALIZE WK-VARIABLES                                      00003080
                      CT-CONTADORES                                     00003080

           ACCEPT WK-BLZ-CORTO FROM SYSIN                               00003180

           SET NO-FIN-MPCHSEE1               TO TRUE                    00003110
                                                                        00003120
           PERFORM 1100-ABRIR-FICHEROS                                  00003130
              THRU 1100-ABRIR-FICHEROS-EXIT                             00003140
                                                                        00003150
           PERFORM 9100-LEER-MPCHSEE1                                   00003160
              THRU 9100-LEER-MPCHSEE1-EXIT                              00003170
                                                                        00003180
           .                                                            00003190
       1000-INICIO-EXIT.                                                00003200
           EXIT.                                                        00003210
                                                                        00003220
      ******************************************************************00003230
      ***                   1100-ABRIR-FICHEROS                      ***00003240
      ***                   -------------------                      ***00003250
      * SE REALIZA LA APERTURA DE LOS FICHEROS DE ENTRADA Y SALIDA.    *00003260
      ******************************************************************00003270
       1100-ABRIR-FICHEROS.                                             00003280
                                                                        00003290
           OPEN INPUT  MPCHSEE1                                         00003300
                OUTPUT MPCHSES1                                         00003310
                       MPCHSES2                                         00003310
                                                                        00003320
           IF  FS-MPCHSEE1 NOT = CA-FS-OK                               00003330
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00003340
               MOVE CA-ERR-ABRIR-MPCHSEE1    TO WK-DESCRIPCION          00003350
               MOVE CA-PRF-1100              TO WK-PARRAFO              00003360
               MOVE CA-MPCHSEE1              TO WK-DDNAME               00003370
               MOVE FS-MPCHSEE1              TO WK-FILE-STATUS          00003380
                                                                        00003390
               PERFORM 9000-CANCELACION                                 00003400
                  THRU 9000-CANCELACION-EXIT                            00003410
                                                                        00003420
           END-IF                                                       00003430
                                                                        00003440
           IF  FS-MPCHSES1 NOT = CA-FS-OK                               00003450
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00003460
               MOVE CA-ERR-ABRIR-MPCHSES1    TO WK-DESCRIPCION          00003470
               MOVE CA-PRF-1100              TO WK-PARRAFO              00003480
               MOVE CA-MPCHSES1              TO WK-DDNAME               00003490
               MOVE FS-MPCHSES1              TO WK-FILE-STATUS          00003500
                                                                        00003510
               PERFORM 9000-CANCELACION                                 00003520
                  THRU 9000-CANCELACION-EXIT                            00003530
                                                                        00003540
           END-IF                                                       00003550
                                                                        00003560
           IF  FS-MPCHSES2 NOT = CA-FS-OK                               00003450
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00003460
               MOVE CA-ERR-ABRIR-MPCHSES2    TO WK-DESCRIPCION          00003470
               MOVE CA-PRF-1100              TO WK-PARRAFO              00003480
               MOVE CA-MPCHSES2              TO WK-DDNAME               00003490
               MOVE FS-MPCHSES2              TO WK-FILE-STATUS          00003500
                                                                        00003510
               PERFORM 9000-CANCELACION                                 00003520
                  THRU 9000-CANCELACION-EXIT                            00003530
                                                                        00003540
           END-IF                                                       00003550
                                                                        00003560
           .                                                            00003570
       1100-ABRIR-FICHEROS-EXIT.                                        00003580
           EXIT.                                                        00003590
                                                                        00003600
      ******************************************************************00003610
      ***                   2000-PROCESO                             ***00003620
      ***                   ------------                             ***00003630
      * SE REALIZA EL TRATAMIENTO DE LAS TARJETAS ESCRIBIENDO EN LOS   *00003640
      * FICHERO DE SALIDA.                                             *00003650
      ******************************************************************00003660
       2000-PROCESO.                                                    00003670
                                                                        00003680
           IF  WK-XPAN-009(CN-4:CN-5) = WK-BLZ-CORTO
               PERFORM 2200-ESCRIBIR-MPCHSES2
                  THRU 2200-ESCRIBIR-MPCHSES2-EXIT

           ELSE
               PERFORM 2100-ESCRIBIR-MPCHSES1
                  THRU 2100-ESCRIBIR-MPCHSES1-EXIT

           END-IF

           PERFORM 9100-LEER-MPCHSEE1                                   00003750
              THRU 9100-LEER-MPCHSEE1-EXIT                              00003760
                                                                        00003770
           .                                                            00003780
       2000-PROCESO-EXIT.                                               00003790
           EXIT.                                                        00003800
                                                                        00003810
      ******************************************************************00004540
      ***                   2100-ESCRIBIR-MPCHSES1                   ***00004550
      ***                   ----------------------                   ***00004560
      * SE REALIZA LA ESCRITURA DEL FICHERO MPCHSES1.                  *00004570
      ******************************************************************00004580
       2100-ESCRIBIR-MPCHSES1.                                          00004590
                                                                        00004600
           WRITE REG-MPCHSES1 FROM WK-REGISTRO                          00004830
                                                                        00004840
           IF  FS-MPCHSES1 NOT = CA-FS-OK                               00004850
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00004860
               MOVE CA-ERR-ESCRIB-MPCHSES1   TO WK-DESCRIPCION          00004870
               MOVE CA-PRF-2100              TO WK-PARRAFO              00004880
               MOVE CA-MPCHSES1              TO WK-DDNAME               00004890
               MOVE FS-MPCHSES1              TO WK-FILE-STATUS          00004900
               MOVE WK-REGISTRO              TO WK-DATOS-REGISTRO       00004910
                                                                        00004920
               PERFORM 9000-CANCELACION                                 00004930
                  THRU 9000-CANCELACION-EXIT                            00004940
                                                                        00004950
           ELSE                                                         00004960
               ADD CN-1                      TO CT-REG-MPCHSES1         00004970
                                                                        00004980
           END-IF                                                       00004990
                                                                        00005000
           .                                                            00005010
       2100-ESCRIBIR-MPCHSES1-EXIT.                                     00005020
           EXIT.                                                        00005030
                                                                        00005040
      ******************************************************************00004540
      ***                   2200-ESCRIBIR-MPCHSES2                   ***00004550
      ***                   ----------------------                   ***00004560
      * SE REALIZA LA ESCRITURA DEL FICHERO MPCHSES2.                  *00004570
      ******************************************************************00004580
       2200-ESCRIBIR-MPCHSES2.                                          00004590
                                                                        00004600
           WRITE REG-MPCHSES2 FROM WK-REGISTRO                          00004830
                                                                        00004840
           IF  FS-MPCHSES2 NOT = CA-FS-OK                               00004850
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00004860
               MOVE CA-ERR-ESCRIB-MPCHSES2   TO WK-DESCRIPCION          00004870
               MOVE CA-PRF-2200              TO WK-PARRAFO              00004880
               MOVE CA-MPCHSES2              TO WK-DDNAME               00004890
               MOVE FS-MPCHSES2              TO WK-FILE-STATUS          00004900
               MOVE WK-REGISTRO              TO WK-DATOS-REGISTRO       00004910
                                                                        00004920
               PERFORM 9000-CANCELACION                                 00004930
                  THRU 9000-CANCELACION-EXIT                            00004940
                                                                        00004950
           ELSE                                                         00004960
               ADD CN-1                      TO CT-REG-MPCHSES2         00004970
                                                                        00004980
           END-IF                                                       00004990
                                                                        00005000
           .                                                            00005010
       2200-ESCRIBIR-MPCHSES2-EXIT.                                     00005020
           EXIT.                                                        00005030
                                                                        00005040
      ******************************************************************00005050
      ***                   3000-FIN                                 ***00005060
      ***                   --------                                 ***00005070
      * SE REALIZA EL CIERRE DE LOS FICHEROS, MOSTRAMOS LAS            *00005080
      * ESTADISTICAS DEL PROGRAMA Y FINALIZACION DE LA EJECUCION.      *00005090
      ******************************************************************00005100
       3000-FIN.                                                        00005110
                                                                        00005120
           PERFORM 3100-CERRAR-FICHEROS                                 00005130
              THRU 3100-CERRAR-FICHEROS-EXIT                            00005140
                                                                        00005150
           PERFORM 3200-ESTADISTICAS                                    00005160
              THRU 3200-ESTADISTICAS-EXIT                               00005170
                                                                        00005180
           STOP RUN                                                     00005190
                                                                        00005200
           .                                                            00005210
       3000-FIN-EXIT.                                                   00005220
           EXIT.                                                        00005230
                                                                        00005240
      ******************************************************************00005250
      ***                   3100-CERRAR-FICHEROS                     ***00005260
      ***                   ---------------------                    ***00005270
      * SE REALIZA EL CIERRE DE LOS FICHEROS DE ENTRADA Y SALIDA.      *00005280
      ******************************************************************00005290
       3100-CERRAR-FICHEROS.                                            00005300
                                                                        00005310
           CLOSE MPCHSEE1                                               00005320
                 MPCHSES1                                               00005330
                 MPCHSES2                                               00005330
                                                                        00005340
           IF  FS-MPCHSEE1 NOT = CA-FS-OK                               00005350
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00005360
               MOVE CA-ERR-CERRAR-MPCHSEE1   TO WK-DESCRIPCION          00005370
               MOVE CA-PRF-3100              TO WK-PARRAFO              00005380
               MOVE CA-MPCHSEE1              TO WK-DDNAME               00005390
               MOVE FS-MPCHSEE1              TO WK-FILE-STATUS          00005400
                                                                        00005410
               PERFORM 9000-CANCELACION                                 00005420
                  THRU 9000-CANCELACION-EXIT                            00005430
                                                                        00005440
           END-IF                                                       00005450
                                                                        00005460
           IF  FS-MPCHSES1 NOT = CA-FS-OK                               00005470
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00005480
               MOVE CA-ERR-CERRAR-MPCHSES1   TO WK-DESCRIPCION          00005490
               MOVE CA-PRF-3100              TO WK-PARRAFO              00005500
               MOVE CA-MPCHSES1              TO WK-DDNAME               00005510
               MOVE FS-MPCHSES1              TO WK-FILE-STATUS          00005520
                                                                        00005530
               PERFORM 9000-CANCELACION                                 00005540
                  THRU 9000-CANCELACION-EXIT                            00005550
                                                                        00005560
           END-IF                                                       00005570
                                                                        00005580
           IF  FS-MPCHSES2 NOT = CA-FS-OK                               00005470
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00005480
               MOVE CA-ERR-CERRAR-MPCHSES2   TO WK-DESCRIPCION          00005490
               MOVE CA-PRF-3100              TO WK-PARRAFO              00005500
               MOVE CA-MPCHSES2              TO WK-DDNAME               00005510
               MOVE FS-MPCHSES2              TO WK-FILE-STATUS          00005520
                                                                        00005530
               PERFORM 9000-CANCELACION                                 00005540
                  THRU 9000-CANCELACION-EXIT                            00005550
                                                                        00005560
           END-IF                                                       00005570
                                                                        00005580
           .                                                            00005590
       3100-CERRAR-FICHEROS-EXIT.                                       00005600
           EXIT.                                                        00005610
                                                                        00005620
      ******************************************************************00005630
      ***                   3200-ESTADISTICAS                        ***00005640
      ***                   -----------------                        ***00005650
      * SE MUESTRAN LAS ESTADISTICAS DEL PROGRAMA.                     *00005660
      ******************************************************************00005670
       3200-ESTADISTICAS.                                               00005680
                                                                        00005690
           MOVE CT-REG-MPCHSEE1              TO WK-LEIDOS-ED            00005700
           MOVE CT-REG-MPCHSES1              TO WK-ESCRI1-ED            00005710
           MOVE CT-REG-MPCHSES2              TO WK-ESCRI2-ED            00005710
                                                                        00005720
           DISPLAY WK-CAB-1                                             00005730
           DISPLAY WK-CAB-2                                             00005740
           DISPLAY WK-CAB-1                                             00005750
           DISPLAY WK-CAB-3                                             00005760
           DISPLAY WK-CAB-4                                             00005770
           DISPLAY WK-CAB-5                                             00005770
           DISPLAY WK-CAB-1                                             00005780
                                                                        00005790
           .                                                            00005800
       3200-ESTADISTICAS-EXIT.                                          00005810
           EXIT.                                                        00005820
                                                                        00005830
      ******************************************************************00005840
      ***                   9100-LEER-MPCHSEE1                       ***00005850
      ***                   ------------------                       ***00005860
      * SE REALIZA LA LECTURA DEL FICHERO DE ENTRADA.                  *00005870
      ******************************************************************00005880
       9100-LEER-MPCHSEE1.                                              00005890
                                                                        00005900
           INITIALIZE WK-REGISTRO                                       00005910
                                                                        00005920
           READ MPCHSEE1 INTO WK-REGISTRO                               00005930
           AT END                                                       00005940
                SET SI-FIN-MPCHSEE1          TO TRUE                    00005950
                                                                        00005960
             NOT AT END                                                 00005970
                ADD CN-1                     TO CT-REG-MPCHSEE1         00005980
                                                                        00005990
           END-READ                                                     00006000
                                                                        00006010
           IF  FS-MPCHSEE1 NOT = CA-FS-OK AND CA-FS-EOF                 00006020
               MOVE CA-ERROR-F               TO WK-TIPO-ERROR           00006030
               MOVE CA-ERR-LEER-MPCHSEE1     TO WK-DESCRIPCION          00006040
               MOVE CA-PRF-9100              TO WK-PARRAFO              00006050
               MOVE CA-MPCHSEE1              TO WK-DDNAME               00006060
               MOVE FS-MPCHSEE1              TO WK-FILE-STATUS          00006070
                                                                        00006080
               PERFORM 9000-CANCELACION                                 00006090
                  THRU 9000-CANCELACION-EXIT                            00006100
                                                                        00006110
           END-IF                                                       00006120
                                                                        00006130
           .                                                            00006140
       9100-LEER-MPCHSEE1-EXIT.                                         00006150
           EXIT.                                                        00006160
                                                                        00006170
      ******************************************************************00006180
      ***                   9000-CANCELACION                         ***00006190
      ***                   ----------------                         ***00006200
      * LLAMA A LA FUNCI”N XX_CANCELACION_PROCESOS_BATCH.              *00006210
      ******************************************************************00006220
       9000-CANCELACION.                                                00006230
                                                                        00006240
           MOVE CA-RESP                      TO WK-RESPONSABLE          00006250
                                                                        00006260
           EVALUATE  WK-TIPO-ERROR                                      00006270
               WHEN  CA-ERROR-D                                         00006280
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH             00006290
                         TIPO_ERROR('WK-TIPO-ERROR')                    00006300
                         COD_RETORNO('WK-COD-RETORNO')                  00006310
                         RESPONSABLE('WK-RESPONSABLE')                  00006320
                         DESCRIPCION('WK-DESCRIPCION')                  00006330
                         PROGRAMA('WK-PROGRAMA')                        00006340
                         PARRAFO('WK-PARRAFO')                          00006350
                         SQLCA('WK-SQLCA')                              00006360
                         TABLA_DB2('WK-TABLA-DB2')                      00006370
                         DATOS_ACCESO('WK-DATOS-ACCESO')                00006380
                     END-FUN                                            00006390
                                                                        00006400
               WHEN  CA-ERROR-R                                         00006410
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH             00006420
                         TIPO_ERROR('WK-TIPO-ERROR')                    00006430
                         COD_RETORNO('WK-COD-RETORNO')                  00006440
                         RESPONSABLE('WK-RESPONSABLE')                  00006450
                         DESCRIPCION('WK-DESCRIPCION')                  00006460
                         PROGRAMA('WK-PROGRAMA')                        00006470
                         PARRAFO('WK-PARRAFO')                          00006480
                         RUTINA('WK-RUTINA')                            00006490
                         PARAMETROS('WK-PARAMETROS')                    00006500
                     END-FUN                                            00006510
                                                                        00006520
               WHEN  CA-ERROR-F                                         00006530
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH             00006540
                         TIPO_ERROR('WK-TIPO-ERROR')                    00006550
                         RESPONSABLE('WK-RESPONSABLE')                  00006560
                         DESCRIPCION('WK-DESCRIPCION')                  00006570
                         PROGRAMA('WK-PROGRAMA')                        00006580
                         PARRAFO('WK-PARRAFO')                          00006590
                         DDNAME('WK-DDNAME')                            00006600
                         FILE_STATUS('WK-FILE-STATUS')                  00006610
                         DATOS_REGISTRO('WK-DATOS-REGISTRO')            00006620
                     END-FUN                                            00006630
                                                                        00006640
           END-EVALUATE                                                 00006650
                                                                        00006660
           .                                                            00006670
       9000-CANCELACION-EXIT.                                           00006680
           EXIT.                                                        00006690
