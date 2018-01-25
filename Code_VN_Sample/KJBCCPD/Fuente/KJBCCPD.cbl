      ******************************************************************00000010
      *-                                                              -*00000020
      *  PROGRAMA:    KJBCCPD.                                         *00000030
      *-                                                              -*00000040
      *  FECHA CREACIóN: 30/11/2016.           AUTOR: VIEWNEXT.        *00000050
      *-                                                              -*00000060
      *  APLICACIóN:  KJ.                                              *00000070
      *-                                                              -*00000080
      *  INSTALACION: ISBAN.                                           *00000090
      *-                                                              -*00000100
      *  DESCRIPCION: RUTINA MIXTA DE CONSOLIDACION DE POSICIONES DE   *00000110
      *               DISPOSITIVOS.                                    *00000120
      *-                                                              -*00000130
      *  COPYS Y DCLGENS:                                              *00000140
      *     KJYCCPD  : COPY DE COMUNICACIÓN DE LA RUTINA.              *00000150
      *     D7397400 : DCLGEN DE LA TABLA POS_DISP_PMAS.               *00000160
      *-                                                              -*00000170
      *  TABLAS:                                                       *00000180
      *     POS_DISP_PMAS : TABLA QUE CONTIENE POSICIONES DE SALDOS    *00000190
      *                     CONSOLIDADOS DE DISPOSITIVOS PMAS.         *00000200
      *-                                                              -*00000210
      *  CODIGOS DE RETORNO:                                           *00000220
      *   - '00' ---> PROCESO CORRECTO                                 *00000230
      *   - '88' ---> ERROR FUNCIONAL                                  *00000240
      *   - '99' ---> ERROR DE DB2                                     *00000250
      *-                                                              -*00000260
      ******************************************************************00000270
      ******************************************************************00000280
      *                  M O D I F I C A C I O N E S                   *00000290
      *                  ---------------------------                   *00000300
      *                                                                *00000310
      * USUARIO  FECHA        DESCRIPCION                              *00000320
      * -------- ----------   ---------------------------------------- *00000330
      *                                                                *00000340
      ******************************************************************00000350
                                                                        00000360
      ******************************************************************00000370
      * IDENTIFICATION DIVISION                                        *00000380
      ******************************************************************00000390
       IDENTIFICATION DIVISION.                                         00000400
       PROGRAM-ID.    KJBCCPD.                                          00000410
       AUTHOR.        VIEWNEXT.                                         00000420
       DATE-WRITTEN.  30-11-2016.                                       00000430
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
         SOURCE-COMPUTER.  IBM-3090.                                    00000550
         OBJECT-COMPUTER.  IBM-3090.                                    00000560
                                                                        00000570
      ******************************************************************00000580
      * DATA DIVISION                                                  *00000590
      ******************************************************************00000600
       DATA DIVISION.                                                   00000610
                                                                        00000620
      ******************************************************************00000630
      *                W O R K I N G    S T O R A G E                  *00000640
      ******************************************************************00000650
       WORKING-STORAGE SECTION.                                         00000660
       77  AUDITCOB      PIC X(60) VALUE                                00000010
           'AUDITCOB*[MODNAME-[DATEUPD-[TIMEUPD-[LVNO-[PROGRAMMERNAME'. 00000020
                                                                        00000680
      ******************************************************************00000690
      *-                     S W I T C H E S                          -*00000700
      ******************************************************************00000710
       01  SW-SWITCHES.                                                 00000720
           05  SW-DB2-RETURN-CODE          PIC S9(09) COMP VALUE ZEROES.00000730
               88  DB2-OK                              VALUE 0.         00000740
               88  DB2-CLV-NOT-FOUND                   VALUE +100.      00000750
               88  DB2-RECURSO-NO-DISPONIBLE           VALUE -911.      00000760
               88  DB2-TABLA-BLOQUEADA                 VALUE -904.      00000770
                                                                        00000780
      ******************************************************************00000790
      *                    C O N S T A N T E S                         *00000800
      ******************************************************************00000810
       01  CONSTANTES.                                                  00000820
           05  CONSTANTES-ALFANUMERICAS.                                00000830
               10  CA-01                   PIC X(02)   VALUE '01'.      00000840
               10  CA-BATCH                PIC X(05)   VALUE 'BATCH'.   00000850
      * -- RETORNOS DE LA RUTINA                                        00000860
               10  CA-00                   PIC X(02)   VALUE '00'.      00000870
               10  CA-88                   PIC X(02)   VALUE '88'.      00000880
               10  CA-99                   PIC X(02)   VALUE '99'.      00000890
      * -- NOMBRE DE RUTINAS Y TABLAS                                   00000900
               10  CA-KJBCCPD              PIC X(07)   VALUE 'KJBCCPD'. 00000910
               10  CA-POS-DISP-PMAS        PIC X(13)   VALUE 'POS_DISP_P00000920
      -                              'MAS'.                             00000930
      * -- LITERALES                                                    00000940
               10  CA-CODMONSW             PIC X(08)   VALUE 'CODMONSW'.00000950
               10  CA-CTOSALDO             PIC X(08)   VALUE 'CTOSALDO'.00000960
               10  CA-CODSPROD             PIC X(08)   VALUE 'CODSPROD'.00000970
               10  CA-IDPROD               PIC X(06)   VALUE 'IDPROD'.  00000980
               10  CA-IDEMPR               PIC X(06)   VALUE 'IDEMPR'.  00000990
               10  CA-IDCENT               PIC X(06)   VALUE 'IDCENT'.  00001000
      * -- OPERACIONES                                                  00001010
               10  CA-UPDATE               PIC X(06)   VALUE 'UPDATE'.  00001020
               10  CA-INSERT               PIC X(06)   VALUE 'INSERT'.  00001020
      * -- DESCRIPCION DE ERRORES                                       00001030
               10  CA-CAMPO-OBLIGATORIO    PIC X(24)   VALUE 'FALTA CAMP00001040
      -                              'O OBLIGATORIO:'.                  00001050
               10  CA-ERR-RECURSO          PIC X(39)   VALUE 'TABLA BLOQ00001060
      -                              'UEADA O RECURSO NO DISPONIBLE'.   00001070
               10  CA-ERROR-DB2            PIC X(10)   VALUE 'ERROR DB2 00001080
      -                              ''.                                00001090
               10  CA-ERR-DATOS            PIC X(31)   VALUE 'ERROR NO H00001100
      -                              'AY DATOS EN LA TABLA'.            00001110
      * -- PARRAFOS                                                     00001120
               10  CA-PRF-1100             PIC X(20)   VALUE '1100-VALID00001130
      -                              'AR-ENTRADA'.                      00001140
               10  CA-PRF-2100             PIC X(21)   VALUE '2100-UPDAT00001150
      -                              'E-DISP-PMAS'.                     00001160
               10  CA-PRF-2110             PIC X(21)   VALUE '2110-INSER00001150
      -                              'T-DISP-PMAS'.                     00001160
                                                                        00001170
      ******************************************************************00001180
      *                    V A R I A B L E S                           *00001190
      ******************************************************************00001200
       01  WK-VARIABLES.                                                00001210
           05  WK-IMPSLDC                  PIC S9(15)V9(2) USAGE COMP-3.00001220
                                                                        00001230
      ******************************************************************00001240
      *                        M E N S A J E S                         *00001250
      ******************************************************************00001260
       01  MENSAJES.                                                    00001270
      * -- KJ0252: CENTRO OBLIGATORIO.                                  00001280
           10  MM-KJ0252                   PIC X(06)   VALUE 'KJ0252'.  00001290
      * -- KJ0253: IDENTIFICADOR DE PRODUCTO OBLIGATORIO.               00001300
           10  MM-KJ0253                   PIC X(06)   VALUE 'KJ0253'.  00001310
      * -- KJ0368: SUBTIPO DE PRODUCTO OBLIGATORIO.                     00001320
           10  MM-KJ0368                   PIC X(06)   VALUE 'KJ0368'.  00001330
      * -- KJ0294: MONEDA OBLIGATORIA.                                  00001340
           10  MM-KJ0294                   PIC X(06)   VALUE 'KJ0294'.  00001350
      * -- KJ0346: EL CONCEPTO DEL SALDO ES OBLIGATORIO.                00001360
           10  MM-KJ0346                   PIC X(06)   VALUE 'KJ0346'.  00001370
      * -- KJ0370: EMPRESA OBLIGATORIA.                                 00001380
           10  MM-KJ0370                   PIC X(06)   VALUE 'KJ0370'.  00001390
      * -- XX0013: REGISTRO NO ENCONTRADO.                              00001400
           10  MM-XX0013                   PIC X(06)   VALUE 'XX0013'.  00001410
      * -- XX3333: ERROR TECNICO. CONTACTE CON SU %XXAU .               00001420
           10  MM-XX3333                   PIC X(06)   VALUE 'XX3333'.  00001430
      * -- XX9520: EN ESTE MOMENTO NO SE PUEDE ATENDER SU PETICION.     00001440
      *            INTENTELO MAS TARDE.                                 00001450
           10  MM-XX9520                   PIC X(06)   VALUE 'XX9520'.  00001460
                                                                        00001470
      ******************************************************************00001480
      *             C O P Y S    Y   D C L G E N S                     *00001490
      ******************************************************************00001500
      * -- COPY DE COMUNICACION CON DB2.                                00001510
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00001520
                                                                        00001530
      * -- DCLGEN DE LA TABLA POS_DISP_PMAS.                            00001540
           EXEC SQL INCLUDE D7397400 END-EXEC.                          00001550
                                                                        00001560
      *----------------------------------------------------------------*00001570
      * LINKAGE SECTION                                                *00001580
      *----------------------------------------------------------------*00001590
       LINKAGE SECTION.                                                 00001600
      *INCLUDE-PARM-FUN ONLINE                                          00001610
           EXEC-FUN _COPY DATOS_RUTINA END-FUN                          00001620
      *END-INCLUDE                                                      00001630
                                                                        00001640
      * -- COPY DE COMUNICACION CON LA RUTINA                           00001650
           COPY KJYCCPD.                                                00001660
                                                                        00001670
      ******************************************************************00001680
      *                       PROCEDURE DIVISION                       *00001690
      ******************************************************************00001700
       PROCEDURE DIVISION USING                                         00001710
      *INCLUDE-PARM-FUN CICS                                            00001720
                                DFHEIBLK                                00001730
                                DFHCOMMAREA                             00001740
      *END-INCLUDE                                                      00001750
                                KJYCCPD.                                00001760
                                                                        00001770
           PERFORM 1000-INICIO                                          00001780
              THRU 1000-INICIO-EXIT                                     00001790
                                                                        00001800
           PERFORM 2000-PROCESO                                         00001810
              THRU 2000-PROCESO-EXIT                                    00001820
                                                                        00001830
           PERFORM 3000-FIN                                             00001840
              THRU 3000-FIN-EXIT                                        00001850
                                                                        00001860
           .                                                            00001870
                                                                        00001880
      ******************************************************************00001890
      * 1000-INICIO.                                                   *00001900
      *  SE INICIALIZAN LAS VARIABLES UTILIZADAS Y SE VALIDA QUE LOS   *00001910
      *  DATOS DE ENTRADA VENGAN INFORMADOS.                           *00001920
      ******************************************************************00001930
       1000-INICIO.                                                     00001940
                                                                        00001950
           INITIALIZE KJYCCPD-DATOS-CONTROL                             00001960
                      WK-VARIABLES                                      00001970
                                                                        00001980
           MOVE CA-00                        TO KJYCCPD-RETORNO         00001990
                                                                        00002000
           PERFORM 1100-VALIDAR-ENTRADA                                 00002010
              THRU 1100-VALIDAR-ENTRADA-EXIT                            00002020
                                                                        00002030
           .                                                            00002040
       1000-INICIO-EXIT.                                                00002050
           EXIT.                                                        00002060
                                                                        00002070
      ******************************************************************00002080
      * 1100-VALIDAR-ENTRADA.                                          *00002090
      *  SE VALIDA QUE LOS CAMPOS DE ENTRADA OBLIGATORIOS VENGAN       *00002100
      *  INFORMADOS.                                                   *00002110
      ******************************************************************00002120
       1100-VALIDAR-ENTRADA.                                            00002130
                                                                        00002140
           IF  KJYCCPD-IDEMPR = SPACES OR LOW-VALUES                    00002150
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002160
               MOVE MM-KJ0370                TO KJYCCPD-MENSAJE         00002170
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002180
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002190
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00002200
                                                                        00002210
               STRING CA-CAMPO-OBLIGATORIO                              00002220
                SPACE CA-IDEMPR                                         00002230
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00002240
                                                                        00002250
               PERFORM 3000-FIN                                         00002260
                  THRU 3000-FIN-EXIT                                    00002270
                                                                        00002280
           END-IF                                                       00002290
                                                                        00002300
           IF  KJYCCPD-IDCENT = SPACES OR LOW-VALUES                    00002310
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002320
               MOVE MM-KJ0252                TO KJYCCPD-MENSAJE         00002330
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002340
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002350
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00002360
                                                                        00002370
               STRING CA-CAMPO-OBLIGATORIO                              00002380
                SPACE CA-IDCENT                                         00002390
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00002400
                                                                        00002410
               PERFORM 3000-FIN                                         00002420
                  THRU 3000-FIN-EXIT                                    00002430
                                                                        00002440
           END-IF                                                       00002450
                                                                        00002460
           IF  KJYCCPD-IDPROD = SPACES OR LOW-VALUES                    00002470
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002480
               MOVE MM-KJ0253                TO KJYCCPD-MENSAJE         00002490
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002500
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002510
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00002520
                                                                        00002530
               STRING CA-CAMPO-OBLIGATORIO                              00002540
                SPACE CA-IDPROD                                         00002550
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00002560
                                                                        00002570
               PERFORM 3000-FIN                                         00002580
                  THRU 3000-FIN-EXIT                                    00002590
                                                                        00002600
           END-IF                                                       00002610
                                                                        00002620
           IF  KJYCCPD-CODSPROD = SPACES OR LOW-VALUES                  00002630
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002640
               MOVE MM-KJ0368                TO KJYCCPD-MENSAJE         00002650
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002660
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002670
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00002680
                                                                        00002690
               STRING CA-CAMPO-OBLIGATORIO                              00002700
                SPACE CA-CODSPROD                                       00002710
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00002720
                                                                        00002730
               PERFORM 3000-FIN                                         00002740
                  THRU 3000-FIN-EXIT                                    00002750
                                                                        00002760
           END-IF                                                       00002770
                                                                        00002780
           IF  KJYCCPD-CTOSALDO = SPACES OR LOW-VALUES                  00002790
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002800
               MOVE MM-KJ0346                TO KJYCCPD-MENSAJE         00002810
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002820
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002830
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00002840
                                                                        00002850
               STRING CA-CAMPO-OBLIGATORIO                              00002860
                SPACE CA-CTOSALDO                                       00002870
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00002880
                                                                        00002890
               PERFORM 3000-FIN                                         00002900
                  THRU 3000-FIN-EXIT                                    00002910
                                                                        00002920
           END-IF                                                       00002930
                                                                        00002940
           IF  KJYCCPD-CODMONSW = SPACES OR LOW-VALUES                  00002950
               MOVE CA-88                    TO KJYCCPD-RETORNO         00002960
               MOVE MM-KJ0294                TO KJYCCPD-MENSAJE         00002970
               MOVE CA-PRF-1100              TO KJYCCPD-PARRAFO         00002980
               MOVE CA-KJBCCPD               TO KJYCCPD-RUTINA          00002990
               MOVE KJYCCPD-ENTRADA          TO KJYCCPD-DATOS           00003000
                                                                        00003010
               STRING CA-CAMPO-OBLIGATORIO                              00003020
                SPACE CA-CODMONSW                                       00003030
               DELIMITED BY SIZE           INTO KJYCCPD-DESCRIPCION     00003040
                                                                        00003050
               PERFORM 3000-FIN                                         00003060
                  THRU 3000-FIN-EXIT                                    00003070
                                                                        00003080
           END-IF                                                       00003090
                                                                        00003100
           .                                                            00003110
       1100-VALIDAR-ENTRADA-EXIT.                                       00003120
           EXIT.                                                        00003130
                                                                        00003140
      ******************************************************************00003150
      * 2000-PROCESO.                                                  *00003160
      *  PROCESO PRINCIPAL DEL PROGRAMA.                               *00003170
      ******************************************************************00003180
       2000-PROCESO.                                                    00003190
                                                                        00003200
           PERFORM 2100-UPDATE-DISP-PMAS                                00003210
              THRU 2100-UPDATE-DISP-PMAS-EXIT                           00003220
                                                                        00003230
           .                                                            00003240
       2000-PROCESO-EXIT.                                               00003250
           EXIT.                                                        00003260
                                                                        00003270
      ******************************************************************00003280
      * 2100-UPDATE-DISP-PMAS                                          *00003290
      * SE REALIZA UN ACTUALIZACION SOBRE LA TABLA POS_DISP_PMAS.      *00003300
      ******************************************************************00003310
       2100-UPDATE-DISP-PMAS.                                           00003320
                                                                        00003330
           INITIALIZE DCLPOS-DISP-PMAS                                  00003340
                                                                        00003350
           MOVE KJYCCPD-IDEMPR               TO G6524-IDEMPRD           00003360
           MOVE KJYCCPD-IDCENT               TO G6524-IDCENTD           00003370
           MOVE KJYCCPD-IDPROD               TO G6524-IDPRODD           00003380
           MOVE KJYCCPD-CODSPROD             TO G6524-CODSPROD          00003390
           MOVE KJYCCPD-CTOSALDO             TO G6524-CTOSALDO          00003400
           MOVE KJYCCPD-CODMONSW             TO G6524-CODMONSW          00003410
           MOVE KJYCCPD-IMPORTE-CONSO        TO G6524-IMPSLDC           00003420
           MOVE KJYCCPD-FECHA-CONTABLE       TO G6524-FECCONSO          00003430
      *EXCLUDE-PARM-FUN CICS                                            00003440
           MOVE KJYCCPD-IDEMPR               TO G6524-CDENTUMO          00003450
           MOVE KJYCCPD-IDCENT               TO G6524-CDOFIUMO          00003460
           MOVE CA-KJBCCPD                   TO G6524-USUAUDIT          00003470
           MOVE CA-BATCH                     TO G6524-CDTERUMO          00003480
      *END-EXCLUDE                                                      00003490
      *INCLUDE-PARM-FUN ONLINE                                          00003500
           MOVE ARQ-IDEMPR-ASIG              TO G6524-CDENTUMO          00003510
           MOVE ARQ-IDCENT-ASIG              TO G6524-CDOFIUMO          00003520
           MOVE ARQ-USUARIO                  TO G6524-USUAUDIT          00003530
           MOVE ARQ-PUESTO-FISICO            TO G6524-CDTERUMO          00003540
      *END-INCLUDE                                                      00003550
                                                                        00003560
           EXEC SQL                                                     00003570
           UPDATE POS_DISP_PMAS                                         00003580
              SET G6524_IMPSLDC  = :G6524-IMPSLDC ,                     00003590
                  G6524_FECCONSO = :G6524-FECCONSO,                     00003600
                  G6524_CDENTUMO = :G6524-CDENTUMO,                     00003610
                  G6524_CDOFIUMO = :G6524-CDOFIUMO,                     00003620
                  G6524_USUAUDIT = :G6524-USUAUDIT,                     00003630
                  G6524_CDTERUMO = :G6524-CDTERUMO,                     00003640
                  G6524_CONTCUR  =  CURRENT TIMESTAMP                   00003650
            WHERE G6524_IDEMPRD  = :G6524-IDEMPRD                       00003660
              AND G6524_IDCENTD  = :G6524-IDCENTD                       00003670
              AND G6524_IDPRODD  = :G6524-IDPRODD                       00003680
              AND G6524_CODSPROD = :G6524-CODSPROD                      00003690
              AND G6524_CTOSALDO = :G6524-CTOSALDO                      00003700
              AND G6524_CODMONSW = :G6524-CODMONSW                      00003710
           END-EXEC                                                     00003720
                                                                        00003730
           MOVE SQLCODE                      TO SW-DB2-RETURN-CODE      00003740
                                                                        00003750
           EVALUATE  TRUE                                               00003760
               WHEN  DB2-OK                                             00003770
                     CONTINUE                                           00003780
                                                                        00003790
               WHEN  DB2-CLV-NOT-FOUND                                  00003800
                     PERFORM 2110-INSERT-DISP-PMAS
                        THRU 2110-INSERT-DISP-PMAS-EXIT
                                                                        00003940
               WHEN  OTHER                                              00003950
                     IF  DB2-RECURSO-NO-DISPONIBLE OR                   00003960
                         DB2-TABLA-BLOQUEADA                            00003970
                         MOVE MM-XX9520      TO KJYCCPD-MENSAJE         00003980
                         MOVE CA-ERR-RECURSO TO KJYCCPD-DESCRIPCION     00003990
                                                                        00004000
                     ELSE                                               00004010
                         MOVE MM-XX3333      TO KJYCCPD-MENSAJE         00004020
                         MOVE CA-ERROR-DB2   TO KJYCCPD-DESCRIPCION     00004030
                                                                        00004040
                     END-IF                                             00004050
                                                                        00004060
                     MOVE CA-99              TO KJYCCPD-RETORNO         00004070
                     MOVE SQLCODE            TO KJYCCPD-SQLCODE         00004080
                     MOVE SQLCA              TO KJYCCPD-SQLCA           00004090
                     MOVE CA-PRF-2100        TO KJYCCPD-PARRAFO         00004100
                     MOVE CA-KJBCCPD         TO KJYCCPD-RUTINA          00004110
                     MOVE DCLPOS-DISP-PMAS   TO KJYCCPD-DATOS           00004120
                     MOVE CA-POS-DISP-PMAS   TO KJYCCPD-TABLA           00004130
                     MOVE CA-UPDATE          TO KJYCCPD-FUNCION         00004140
                                                KJYCCPD-ACCESO          00004150
                     PERFORM 3000-FIN                                   00004160
                        THRU 3000-FIN-EXIT                              00004170
                                                                        00004180
           END-EVALUATE                                                 00004190
                                                                        00004200
           .                                                            00004210
       2100-UPDATE-DISP-PMAS-EXIT.                                      00004220
           EXIT.                                                        00004230
                                                                        00004240
      ******************************************************************00003280
      * 2110-INSERT-DISP-PMAS.                                         *00003290
      * SE REALIZA UN ACTUALIZACION SOBRE LA TABLA POS_DISP_PMAS.      *00003300
      ******************************************************************00003310
       2110-INSERT-DISP-PMAS.                                           00003320

           MOVE KJYCCPD-IDEMPR               TO G6524-IDEMPCCO
           MOVE KJYCCPD-IDCENT               TO G6524-IDCENCCO

           EXEC  SQL                                                    00006900
               INSERT  INTO  POS_DISP_PMAS                              00006910
                             ( G6524_IDEMPRD ,                          00006920
                               G6524_IDCENTD ,                          00006930
                               G6524_IDPRODD ,                          00006940
                               G6524_CODSPROD,                          00006950
                               G6524_CTOSALDO,                          00006960
                               G6524_CODMONSW,                          00006970
                               G6524_IMPSLDC ,                          00006980
                               G6524_FECCONSO,                          00006990
                               G6524_IDEMPCCO,                          00007000
                               G6524_IDCENCCO,                          00007010
                               G6524_CDENTUMO,                          00007020
                               G6524_CDOFIUMO,                          00007030
                               G6524_USUAUDIT,                          00007030
                               G6524_CDTERUMO,                          00007030
                               G6524_CONTCUR)                           00007040
                     VALUES                                             00007050
                             ( :G6524-IDEMPRD ,                         00007060
                               :G6524-IDCENTD ,                         00007070
                               :G6524-IDPRODD ,                         00007080
                               :G6524-CODSPROD,                         00007090
                               :G6524-CTOSALDO,                         00007100
                               :G6524-CODMONSW,                         00007110
                               :G6524-IMPSLDC ,                         00007120
                               :G6524-FECCONSO,                         00007130
                               :G6524-IDEMPCCO,                         00007140
                               :G6524-IDCENCCO,                         00007150
                               :G6524-CDENTUMO,                         00007160
                               :G6524-CDOFIUMO,                         00007170
                               :G6524-USUAUDIT,                         00007170
                               :G6524-CDTERUMO,                         00007170
                               CURRENT TIMESTAMP)                       00007180
           END-EXEC                                                     00007190
                                                                        00007200
           MOVE SQLCODE                      TO SW-DB2-RETURN-CODE      00007210
                                                                        00007220
           EVALUATE  TRUE                                               00003760
               WHEN  DB2-OK                                             00003770
                     CONTINUE                                           00003780
                                                                        00003790
               WHEN  OTHER                                              00003950
                     IF  DB2-RECURSO-NO-DISPONIBLE OR                   00003960
                         DB2-TABLA-BLOQUEADA                            00003970
                         MOVE MM-XX9520      TO KJYCCPD-MENSAJE         00003980
                         MOVE CA-ERR-RECURSO TO KJYCCPD-DESCRIPCION     00003990
                                                                        00004000
                     ELSE                                               00004010
                         MOVE MM-XX3333      TO KJYCCPD-MENSAJE         00004020
                         MOVE CA-ERROR-DB2   TO KJYCCPD-DESCRIPCION     00004030
                                                                        00004040
                     END-IF                                             00004050
                                                                        00004060
                     MOVE CA-99              TO KJYCCPD-RETORNO         00004070
                     MOVE SQLCODE            TO KJYCCPD-SQLCODE         00004080
                     MOVE SQLCA              TO KJYCCPD-SQLCA           00004090
                     MOVE CA-PRF-2110        TO KJYCCPD-PARRAFO         00004100
                     MOVE CA-KJBCCPD         TO KJYCCPD-RUTINA          00004110
                     MOVE DCLPOS-DISP-PMAS   TO KJYCCPD-DATOS           00004120
                     MOVE CA-POS-DISP-PMAS   TO KJYCCPD-TABLA           00004130
                     MOVE CA-INSERT          TO KJYCCPD-FUNCION         00004140
                                                KJYCCPD-ACCESO          00004150
                     PERFORM 3000-FIN                                   00004160
                        THRU 3000-FIN-EXIT                              00004170
                                                                        00004180
           END-EVALUATE                                                 00004190
                                                                        00004200
           .
       2110-INSERT-DISP-PMAS-EXIT.                                      00003320
           EXIT.
                                                                        00003330
      ******************************************************************00004250
      * 3000-FIN.                                                      *00004260
      *  FINALIZA LA EJECUCION DEL PROGRAMA Y DEVUELVE EL CONTROL.     *00004270
      ******************************************************************00004280
       3000-FIN.                                                        00004290
                                                                        00004300
           GOBACK                                                       00004310
                                                                        00004320
           .                                                            00004330
       3000-FIN-EXIT.                                                   00004340
           EXIT.                                                        00004350
