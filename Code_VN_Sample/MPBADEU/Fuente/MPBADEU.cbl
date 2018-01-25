      ******************************************************************00000010
      *-                                                              -*00000020
      *  PROGRAMA   : MPBADEU.                                         *00000030
      *-                                                              -*00000040
      *  FECHA CREACION : 24-07-2017.   AUTOR: VIEWNEXT.               *00000050
      *-                                                              -*00000060
      *  APLICACION : MP.                                              *00000070
      *-                                                              -*00000080
      *  INSTALACION: ISBAN.                                           *00000090
      *-                                                              -*00000100
      *  DESCRIPCION: RUTINA MIXTA ENCARGADA DE RECUPERAR EL VALOR     *00000110
      *               ASOCIADO AL PAR·METRO 'ABU'.                     *00000120
      *-                                                              -*00000150
      *  COPYS Y DCLGENS:                                              *00000160
      *    MPYADEU  : COPY DE COMUNICACION CON LA RUTINA.              *00000170
      *    D3104800 : DCLGEN DE LA TABLA MPDT007.                      *00000180
      *    D4462800 : DCLGEN DE LA TABLA MDDT750.                      *00000180
      *    D4462900 : DCLGEN DE LA TABLA MDDT755.                      *00000180
      *-                                                              -*00000560
      *  TABLAS:                                                       *00000570
      *    MPDT007  : CONTIENE LA RELACION CONTRACTUAL ENTRE LOS       *00000580
      *               CLIENTES DE LA ENTIDAD Y LA APLICACION DE MEDIOS *00000590
      *               DE PAGO, INCLUYE LAS CARACTERISTICAS DE          *00000590
      *               FUNCIONAMIENTO.                                  *00000590
      *    MDDT750  : RELACION PRODUCTO SUBTIPO CREDITO - PARAMETRO.   *00000600
      *    MDDT755  : DEFINICION PARAMETRO POR PRODUCTO SUBTIPO DE     *00000620
      *               CREDITO.                                         *00000630
      *-                                                              -*00000640
      *  CODIGOS DE RETORNO:                                           *00000650
      *    '00'     : CORRECTO.                                        *00000660
      *    '88'     : ERROR FUNCIONAL.                                 *00000670
      *    '99'     : ERROR DB2.                                       *00000680
      *-                                                              -*00000690
      ******************************************************************00000700
      *                  M O D I F I C A C I O N E S                   *00000710
      ******************************************************************00000720
      *                                                                *00000730
      * USUARIO    FECHA             DESCRIPCION                       *00000740
      * -------- ---------- ------------------------------------------ *00000750
      ******************************************************************00000760
                                                                        00000770
      ******************************************************************00000780
      * IDENTIFICATION DIVISION                                        *00000790
      ******************************************************************00000800
       IDENTIFICATION DIVISION.                                         00000810
       PROGRAM-ID.   MPBADEU.                                           00000820
       AUTHOR.       VIEWNEXT.                                          00000830
       DATE-WRITTEN. 24-07-2017.                                        00000840
       DATE-COMPILED.                                                   00000850
                                                                        00000860
      ******************************************************************00000870
      * ENVIRONMENT DIVISION                                           *00000880
      ******************************************************************00000890
       ENVIRONMENT DIVISION.                                            00000900
                                                                        00000910
      *----------------------------------------------------------------*00000920
      * CONFIGURATION SECTION                                          *00000930
      *----------------------------------------------------------------*00000940
       CONFIGURATION SECTION.                                           00000950
       SOURCE-COMPUTER. IBM-3090.                                       00000960
       OBJECT-COMPUTER. IBM-3090.                                       00000970
       SPECIAL-NAMES.                                                   00000980
           DECIMAL-POINT IS COMMA.                                      00000990
                                                                        00001000
      ******************************************************************00001010
      * DATA DIVISION                                                  *00001020
      ******************************************************************00001030
       DATA DIVISION.                                                   00001040
                                                                        00001050
      *----------------------------------------------------------------*00001060
      * WORKING-STORAGE SECTION                                        *00001070
      *----------------------------------------------------------------*00001080
       WORKING-STORAGE SECTION.                                         00001090
                                                                        00001110
      ******************************************************************00001120
      *                        S W I T C H E S                         *00001130
      ******************************************************************00001140
       01  SW-SWITCHES.                                                 00001150
           05  SW-DB2-RETURN-CODE          PIC S9(09) COMP              00001160
                                                       VALUE ZEROES.    00001170
               88  DB2-OK                              VALUE 0.         00001180
               88  DB2-CLV-NOT-FOUND                   VALUE +100.      00001190
               88  DB2-CLV-NOT-FOUND-MAX               VALUE -305.      00001200
               88  DB2-CLV-DUPLI-INSERT                VALUE -803.      00001210
               88  DB2-CLV-DUPLI-SELECT                VALUE -811.      00001220
               88  DB2-RECURSO-NO-DISPONIBLE           VALUE -911.      00001230
               88  DB2-TABLA-BLOQUEADA                 VALUE -904.      00001240
               88  DB2-FECHA-ERRONEA                   VALUE -180.      00001250
               88  DB2-MAX-NULO                        VALUE -305.      00001260
                                                                        00001270
           05  SW-ENCONTRADO               PIC X(01)   VALUE 'N'.       00001280
               88  SI-ENCONTRADO                       VALUE 'S'.       00001290
               88  NO-ENCONTRADO                       VALUE 'N'.       00001300
                                                                        00001310
      ******************************************************************00001320
      *                    C O N S T A N T E S                         *00001330
      ******************************************************************00001340
       01  CT-CONSTANTES.                                               00001350
           05  CA-CONSTANTES-ALFANUMERICAS.                             00001360
               10  CA-00                   PIC X(02)   VALUE '00'.      00001460
               10  CA-88                   PIC X(02)   VALUE '88'.      00001520
               10  CA-99                   PIC X(02)   VALUE '99'.      00001530
               10  CA-ABU                  PIC X(03)   VALUE 'ABU'.     00001530
               10  CA-FECHA                PIC X(05)   VALUE 'FECHA'.   00001730
               10  CA-MPBADEU              PIC X(07)   VALUE 'MPBADEU'. 00001730
               10  CA-MPDT007              PIC X(07)   VALUE 'MPDT007'. 00001730
               10  CA-MDDT750              PIC X(07)   VALUE 'MDDT750'. 00001730
               10  CA-MDDT755              PIC X(07)   VALUE 'MDDT755'. 00001730
               10  CA-CDGENTI              PIC X(07)   VALUE 'CDGENTI'. 00001730
               10  CA-CTOPROD              PIC X(07)   VALUE 'CTOPROD'. 00001730
               10  CA-CENTCUEN             PIC X(08)   VALUE 'CENTCUEN'.00001730
               10  CA-PRODSUBP             PIC X(08)   VALUE 'PRODSUBP'.00001730
               10  CA-AAAA-MM-DD           PIC X(10)   VALUE 'AAAA-MM-DD00001750
      -                            ''.                                  00001760
               10  CA-PRRF-1100            PIC X(20)   VALUE '1100-VALID00002000
      -                            'AR-ENTRADA'.                        00002010
               10  CA-PRRF-2100            PIC X(20)   VALUE '2100-ACCED00002000
      -                            'ER-MPDT007'.                        00002010
               10  CA-PRRF-2200            PIC X(20)   VALUE '2200-ACCED00002000
      -                            'ER-MDDT750'.                        00002010
               10  CA-PRRF-2300            PIC X(20)   VALUE '2300-ACCED00002000
      -                            'ER-MDDT755'.                        00002010
               10  CA-ERROR-CDGENTI        PIC X(37)   VALUE 'CAMPO CDGE00002000
      -                            'NTI DE ENTRADA NO INFORMADO'.       00002010
               10  CA-ERROR-FECHA          PIC X(35)   VALUE 'CAMPO FECH00002000
      -                            'A DE ENTRADA NO INFORMADO'.         00002010
               10  CA-ERROR-FECHA-ILOG     PIC X(36)   VALUE 'EL CAMPO F00002000
      -                            'ECHA DE ENTRADA ES ILOGICO'.        00002010
               10  CA-ERROR-CENTCUEN       PIC X(49)   VALUE 'ERROR EN L00002000
      -                            'OS CAMPOS DE ENTRADA CENTALTA Y CUEN00002010
      -                            'TNU'.                               00002010
               10  CA-ERROR-PRODSUBP       PIC X(49)   VALUE 'ERROR EN L00002000
      -                            'OS CAMPOS DE ENTRADA CDGPRODU Y CDGS00002010
      -                            'UBP'.                               00002010
               10  CA-ERROR-CTOPROD-INF    PIC X(50)   VALUE 'ERROR, CEN00002000
      -                            'T, CTO, PROD Y SUBPROD DE ENTRADA IN00002010
      -                            'FORM'.                              00002010
               10  CA-ERROR-CTOPROD-NO-INF PIC X(50)   VALUE 'ERROR, CEN00002000
      -                            'T, CTO, PROD Y SUBPROD DE ENT SIN IN00002010
      -                            'FORM'.                              00002010
               10  CA-REG-NO-ENC-007       PIC X(43)   VALUE 'REGISTRO N00002000
      -                            'O ENCONTRADO EN LA TABLA MPDT007'.  00002010
               10  CA-ERROR-RECURSO        PIC X(39)   VALUE 'TABLA BLOQ00002810
      -                            'UEADA O RECURSO NO DISPONIBLE'.     00002820
               10  CA-ERROR-CONS-007       PIC X(39)   VALUE 'ERROR EN L00002810
      -                            'A CONSULTA A LA TABLA MPDT007'.     00002820
               10  CA-ERROR-CONS-750       PIC X(39)   VALUE 'ERROR EN L00002810
      -                            'A CONSULTA A LA TABLA MDDT750'.     00002820
               10  CA-REG-NO-ENC-755       PIC X(43)   VALUE 'REGISTRO N00002000
      -                            'O ENCONTRADO EN LA TABLA MDDT755'.  00002010
               10  CA-ERROR-CONS-755       PIC X(39)   VALUE 'ERROR EN L00002810
      -                            'A CONSULTA A LA TABLA MDDT755'.     00002820
                                                                        00002830
      ******************************************************************00002990
      *                        V A R I A B L E S                       *00003000
      ******************************************************************00003010
       01  WK-VARIABLES.                                                00003020
           05  WK-RETORNO                  PIC 9(04)   VALUE ZEROES.    00003030

      ******************************************************************00003370
      *                       M E N S A J E S                          *00003380
      ******************************************************************00003390
       01  MM-MENSAJES.                                                 00003400
      *--- MP1088 : OPERACION REALIZADA CORRECTAMENTE.                  00003410
           05  MM-MP1088                   PIC X(06)   VALUE 'MP1088'.  00003420
      *--- MM-MP1120: CAMPO ENTIDAD OBLIGATORIO.                        00003430
           05  MM-MP1120                   PIC X(06)   VALUE 'MP1120'.  00003440
      *--- MM-MP8283: FECHA NO INFORMADA.                               00003450
           05  MM-MP8283                   PIC X(06)   VALUE 'MP8283'.  00003460
      *--- MM-MP0610: LA FECHA DE ENTRADA A LA RUTINA ES ILOGICA.       00003470
           05  MM-MP0610                   PIC X(06)   VALUE 'MP0610'.  00003480
      *--- MM-MP0659: CAMPOS DE ENTRADA OBLIGATORIOS.                   00003490
           05  MM-MP0659                   PIC X(06)   VALUE 'MP0659'.  00003500
      *--- MM-MP8199: DATOS DE ENTRADA ERRONEOS.                        00003490
           05  MM-MP8199                   PIC X(06)   VALUE 'MP8199'.  00003500
      *--- MM-MP0070: NO EXISTE REGISTRO PARA EL CRITERIO DE SELECCION. 00003510
           05  MM-MP0070                   PIC X(06)   VALUE 'MP0070'.  00003530
      *--  XX3333: ERROR TECNICO. CONTACTE CON SU %XXAU.                00003810
           05  MM-XX3333                   PIC X(06)   VALUE 'XX3333'.  00003820
      *--  XX9520: EN ESTE MOMENTO NO SE PUEDE ATENDER SU PETICION.     00003830
      *--  INTENTELO MAS TARDE.                                         00003840
           05  MM-XX9520                   PIC X(06)   VALUE 'XX9520'.  00003850
                                                                        00003860
      ******************************************************************00003870
      *                     D C L G E N S                              *00003880
      ******************************************************************00003890
      *--  AREA DE TRABAJO PARA DB2.                                    00004140
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00004150
                                                                        00004160
      *--  DCLGEN DE LA TABLA MPDT007.                                  00004170
           EXEC SQL INCLUDE D3104800 END-EXEC.                          00004180
                                                                        00004190
      *--  DCLGEN DE LA TABLA MDDT750.                                  00004200
           EXEC SQL INCLUDE D4462800 END-EXEC.                          00004210
                                                                        00004220
      *--  DCLGEN DE LA TABLA MDDT755.                                  00004230
           EXEC SQL INCLUDE D4462900 END-EXEC.                          00004240
                                                                        00004250
      *----------------------------------------------------------------*00004400
      * LINKAGE SECTION                                                *00004410
      *----------------------------------------------------------------*00004420
       LINKAGE SECTION.                                                 00004430
                                                                        00004440
      *INCLUDE-PARM-FUN CICS                                            00004450
           EXEC-FUN _COPY DATOS_RUTINA END-FUN                          00004460
      *END-INCLUDE                                                      00004470
                                                                        00004480
           COPY MPYADEU.                                                00004490
                                                                        00004500
      ******************************************************************00004510
      * PROCEDURE DIVISION                                             *00004520
      ******************************************************************00004530
       PROCEDURE DIVISION USING                                         00004540
      *INCLUDE-PARM-FUN CICS                                            00004550
                                DFHEIBLK                                00004560
                                DFHCOMMAREA                             00004570
      *END-INCLUDE                                                      00004580
                                MPYADEU.                                00004590
                                                                        00004600
           PERFORM 1000-INICIO                                          00004610
              THRU 1000-INICIO-EXIT                                     00004620
                                                                        00004630
           PERFORM 2000-PROCESO                                         00004640
              THRU 2000-PROCESO-EXIT                                    00004650
                                                                        00004660
           PERFORM 3000-FIN                                             00004670
              THRU 3000-FIN-EXIT                                        00004680
                                                                        00004690
           .                                                            00004700
                                                                        00004710
      ******************************************************************00004720
      * 1000-INICIO.                                                   *00004730
      * SE INICIALIZAN LAS AREAS DE SALIDA Y RETORNO DE LA COPY DE     *00004740
      * COMUNICACION DE LA RUTINA. SE VALIDAN LOS CAMPOS DE ENTRADA    *00004750
      * OBLIGATORIOS.                                                  *00004760
      ******************************************************************00004780
       1000-INICIO.                                                     00004790
                                                                        00004800
           INITIALIZE MPYADEU-SALIDA                                    00004810
                      MPYADEU-DATOS-CONTROL                             00004820
                      WK-VARIABLES
                                                                        00004840
           MOVE CA-00                        TO MPYADEU-COD-RET         00004850
           MOVE MM-MP1088                    TO MPYADEU-MENSAJE         00004860
                                                                        00004870
           PERFORM 1100-VALIDAR-ENTRADA                                 00004920
              THRU 1100-VALIDAR-ENTRADA-EXIT                            00004930
                                                                        00004940
           .                                                            00004950
       1000-INICIO-EXIT.                                                00004960
           EXIT.                                                        00004970
                                                                        00004980
      ******************************************************************00004990
      * 1100-VALIDAR-ENTRADA.                                          *00005000
      * SE VALIDAN LOS CAMPOS DE ENTRADA OBLIGATORIOS                  *00005010
      ******************************************************************00005020
       1100-VALIDAR-ENTRADA.                                            00005030
                                                                        00005040
           IF  MPYADEU-CDGENTI-E = SPACES OR LOW-VALUES                 00005050
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP1120                TO MPYADEU-MENSAJE         00005070
               MOVE CA-CDGENTI               TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-CDGENTI         TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           END-IF                                                       00005170
                                                                        00005180
           IF  MPYADEU-FECHA-E = SPACES OR LOW-VALUES OR ZEROES         00005050
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP8283                TO MPYADEU-MENSAJE         00005070
               MOVE CA-FECHA                 TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-FECHA           TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           ELSE

               EXEC-FUN XX_VALIDA_CONVIERTE_FECHA
                    VALIDAR()
                    FECHAE('MPYADEU-FECHA-E')
                    FENTRADA('CA-AAAA-MM-DD')
                    RETORNO('WK-RETORNO')
               END-FUN

               IF  WK-RETORNO NOT = ZEROES                              00005050
                   MOVE CA-88                TO MPYADEU-COD-RET         00005060
                   MOVE MM-MP0610            TO MPYADEU-MENSAJE         00005070
                   MOVE CA-FECHA             TO MPYADEU-ELEMENTO        00005080
                   MOVE CA-MPBADEU           TO MPYADEU-RUTINA          00005120
                   MOVE CA-PRRF-1100         TO MPYADEU-PARRAFO         00005120
                   MOVE CA-ERROR-FECHA-ILOG  TO MPYADEU-TEXTO           00005120
                                                                        00005130
                   PERFORM 3000-FIN                                     00005140
                      THRU 3000-FIN-EXIT                                00005150
                                                                        00005160
               END-IF                                                   00005170
                                                                        00005160
           END-IF                                                       00005170
                                                                        00005180
           IF  ((MPYADEU-CENTALTA-E = SPACES OR LOW-VALUES) AND
               (MPYADEU-CUENTNU-E NOT = SPACES AND LOW-VALUES)) OR
               ((MPYADEU-CENTALTA-E NOT = SPACES AND LOW-VALUES) AND
               (MPYADEU-CUENTNU-E = SPACES OR LOW-VALUES))
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP0659                TO MPYADEU-MENSAJE         00005070
               MOVE CA-CENTCUEN              TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-CENTCUEN        TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           END-IF
                                                                        00005160
           IF  ((MPYADEU-CDGPRODU-E = SPACES OR LOW-VALUES) AND
               (MPYADEU-CDGSUBP-E NOT = SPACES AND LOW-VALUES)) OR
               ((MPYADEU-CDGPRODU-E NOT = SPACES AND LOW-VALUES) AND
               (MPYADEU-CDGSUBP-E = SPACES OR LOW-VALUES))
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP0659                TO MPYADEU-MENSAJE         00005070
               MOVE CA-PRODSUBP              TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-PRODSUBP        TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           END-IF
                                                                        00005160
           IF  ((MPYADEU-CENTALTA-E NOT = SPACES AND LOW-VALUES) AND
               (MPYADEU-CUENTNU-E NOT = SPACES AND LOW-VALUES)) AND
               ((MPYADEU-CDGPRODU-E NOT = SPACES AND LOW-VALUES) AND
               (MPYADEU-CDGSUBP-E NOT = SPACES AND LOW-VALUES))
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP8199                TO MPYADEU-MENSAJE         00005070
               MOVE CA-CTOPROD               TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-CTOPROD-INF     TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           END-IF
                                                                        00005160
           IF  ((MPYADEU-CENTALTA-E = SPACES OR LOW-VALUES) AND
               (MPYADEU-CUENTNU-E = SPACES OR LOW-VALUES)) AND
               ((MPYADEU-CDGPRODU-E = SPACES OR LOW-VALUES) AND
               (MPYADEU-CDGSUBP-E = SPACES OR LOW-VALUES))
               MOVE CA-88                    TO MPYADEU-COD-RET         00005060
               MOVE MM-MP0659                TO MPYADEU-MENSAJE         00005070
               MOVE CA-CTOPROD               TO MPYADEU-ELEMENTO        00005080
               MOVE CA-MPBADEU               TO MPYADEU-RUTINA          00005120
               MOVE CA-PRRF-1100             TO MPYADEU-PARRAFO         00005120
               MOVE CA-ERROR-CTOPROD-NO-INF  TO MPYADEU-TEXTO           00005120
                                                                        00005130
               PERFORM 3000-FIN                                         00005140
                  THRU 3000-FIN-EXIT                                    00005150
                                                                        00005160
           END-IF
           .                                                            00006110
       1100-VALIDAR-ENTRADA-EXIT.                                       00006120
           EXIT.                                                        00006130
                                                                        00006140
      ******************************************************************00006150
      * 2000-PROCESO.                                                  *00006160
      * SE REALIZA EL PROCESO PRINCIPAL.                               *00006170
      ******************************************************************00006180
       2000-PROCESO.                                                    00006190
                                                                        00006200
           SET NO-ENCONTRADO                 TO TRUE

           IF  ((MPYADEU-CENTALTA-E NOT = SPACES AND LOW-VALUES) AND
               (MPYADEU-CUENTNU-E NOT = SPACES AND LOW-VALUES))
                                                                        00006200
               PERFORM 2100-ACCEDER-MPDT007                             00006210
                  THRU 2100-ACCEDER-MPDT007-EXIT                        00006220
                                                                        00006230
           END-IF

           PERFORM 2200-ACCEDER-MDDT750
              THRU 2200-ACCEDER-MDDT750-EXIT

           IF  NO-ENCONTRADO

               PERFORM 2300-ACCEDER-MDDT755
                  THRU 2300-ACCEDER-MDDT755-EXIT

           END-IF
           .                                                            00006630
       2000-PROCESO-EXIT.                                               00006640
           EXIT.                                                        00006650
                                                                        00006660
      ******************************************************************00006670
      * 2100-ACCEDER-MPDT007.                                          *00006680
      * SE ACCEDE A LA TABLA MPDT007 PARA OBTENER EL                   *00006690
      * PRODUCTO/SUBPRODUCTO.                                          *00006690
      ******************************************************************00006700
       2100-ACCEDER-MPDT007.                                            00006710
                                                                        00006720
           INITIALIZE DCLMPDT007

           MOVE MPYADEU-CDGENTI-E            TO E1003-CDGENTI
           MOVE MPYADEU-CENTALTA-E           TO E1003-CENTALTA
           MOVE MPYADEU-CUENTNU-E            TO E1003-CUENTNU

           EXEC SQL                                                     00012740
                SELECT E1003_CDGPRODU,                                  00012750
                       E1003_CDGSUBP                                    00012750
                 INTO :E1003-CDGPRODU,                                  00012760
                      :E1003-CDGSUBP                                    00012760
                  FROM MPDT007                                          00012770
                 WHERE E1003_CDGENTI  = :E1003-CDGENTI
                   AND E1003_CENTALTA = :E1003-CENTALTA
                   AND E1003_CUENTNU  = :E1003-CUENTNU
           END-EXEC                                                     00012780
                                                                        00012790
           MOVE SQLCODE                      TO SW-DB2-RETURN-CODE      00012800
                                                                        00012810
           EVALUATE  TRUE                                               00012820
               WHEN  DB2-OK                                             00012830
                     SET SI-ENCONTRADO       TO TRUE                    00012840

               WHEN  DB2-CLV-NOT-FOUND
                     MOVE CA-88              TO MPYADEU-COD-RET
                     MOVE MM-MP0070          TO MPYADEU-MENSAJE
                     MOVE SQLCODE            TO MPYADEU-SQLCODE
                     MOVE SQLCA              TO MPYADEU-SQLCA
                     MOVE CA-MPDT007         TO MPYADEU-TABLA
                     MOVE CA-MPBADEU         TO MPYADEU-RUTINA
                     MOVE CA-PRRF-2100       TO MPYADEU-PARRAFO
                     MOVE CA-REG-NO-ENC-007  TO MPYADEU-TEXTO           00012850

                     PERFORM 3000-FIN
                        THRU 3000-FIN-EXIT

               WHEN  OTHER
                     MOVE CA-99              TO MPYADEU-COD-RET         00012870
                                                                        00012880
                     IF  DB2-RECURSO-NO-DISPONIBLE OR                   00012890
                         DB2-TABLA-BLOQUEADA                            00012900
                         MOVE MM-XX9520      TO MPYADEU-MENSAJE         00012910
                         MOVE CA-ERROR-RECURSO                          00012920
                                             TO MPYADEU-TEXTO           00012930
                                                                        00012940
                     ELSE                                               00012950
                         MOVE MM-XX3333      TO MPYADEU-MENSAJE         00012960
                         MOVE CA-ERROR-CONS-007                         00012970
                                             TO MPYADEU-TEXTO           00012980
                                                                        00012990
                     END-IF                                             00013000
                                                                        00013010
                     MOVE SQLCODE            TO MPYADEU-SQLCODE
                     MOVE SQLCA              TO MPYADEU-SQLCA
                     MOVE CA-MPDT007         TO MPYADEU-TABLA
                     MOVE CA-MPBADEU         TO MPYADEU-RUTINA
                     MOVE CA-PRRF-2100       TO MPYADEU-PARRAFO
                                                                        00013080
                     PERFORM 3000-FIN                                   00013090
                        THRU 3000-FIN-EXIT                              00013100
                                                                        00013110
           END-EVALUATE                                                 00013120
           .                                                            00007520
       2100-ACCEDER-MPDT007-EXIT.                                       00007530
           EXIT.                                                        00007540
                                                                        00007550
      ******************************************************************00006670
      * 2200-ACCEDER-MDDT750.                                          *00006680
      * SE ACCEDE A LA TABLA MDDT750 PARA RECUPERAR EL VALOR DEL       *00006690
      * PARAMETRO.                                                     *00006690
      ******************************************************************00006700
       2200-ACCEDER-MDDT750.                                            00006710
                                                                        00006720
           INITIALIZE DCLMDDT750

           MOVE MPYADEU-CDGENTI-E            TO G3177-CDGENTI
           MOVE CA-ABU                       TO G3177-PANUMPAR
           MOVE MPYADEU-FECHA-E              TO G3177-FECALTA           00017840
                                                G3177-FEBAJA            00017850

           IF  SI-ENCONTRADO
               MOVE E1003-CDGPRODU           TO G3177-CDGPRODU
               MOVE E1003-CDGSUBP            TO G3177-CDGSUBP

           ELSE
               MOVE MPYADEU-CDGPRODU-E       TO G3177-CDGPRODU
               MOVE MPYADEU-CDGSUBP-E        TO G3177-CDGSUBP

           END-IF

           EXEC SQL                                                     00012740
                SELECT G3177_VALPARM                                    00012750
                 INTO :G3177-VALPARM                                    00012760
                  FROM MDDT750                                          00012770
                 WHERE G3177_CDGENTI   = :G3177-CDGENTI
                   AND G3177_CDGPRODU  = :G3177-CDGPRODU
                   AND G3177_CDGSUBP   = :G3177-CDGSUBP
                   AND G3177_PANUMPAR  = :G3177-PANUMPAR
                   AND G3177_FECALTA  <= :G3177-FECALTA                 00017950
                   AND G3177_FEBAJA   >= :G3177-FEBAJA                  00017960
                 FETCH FIRST 1 ROW ONLY                                 00017970
           END-EXEC                                                     00012780
                                                                        00012790
           MOVE SQLCODE                      TO SW-DB2-RETURN-CODE      00012800
                                                                        00012810
           EVALUATE  TRUE                                               00012820
               WHEN  DB2-OK                                             00012830
                     SET SI-ENCONTRADO       TO TRUE

                     MOVE G3177-VALPARM      TO MPYADEU-VALPARM-S       00012840

               WHEN  DB2-CLV-NOT-FOUND
                     SET NO-ENCONTRADO       TO TRUE

               WHEN  OTHER
                     MOVE CA-99              TO MPYADEU-COD-RET         00012870
                                                                        00012880
                     IF  DB2-RECURSO-NO-DISPONIBLE OR                   00012890
                         DB2-TABLA-BLOQUEADA                            00012900
                         MOVE MM-XX9520      TO MPYADEU-MENSAJE         00012910
                         MOVE CA-ERROR-RECURSO                          00012920
                                             TO MPYADEU-TEXTO           00012930
                                                                        00012940
                     ELSE                                               00012950
                         MOVE MM-XX3333      TO MPYADEU-MENSAJE         00012960
                         MOVE CA-ERROR-CONS-750                         00012970
                                             TO MPYADEU-TEXTO           00012980
                                                                        00012990
                     END-IF                                             00013000
                                                                        00013010
                     MOVE SQLCODE            TO MPYADEU-SQLCODE
                     MOVE SQLCA              TO MPYADEU-SQLCA
                     MOVE CA-MDDT750         TO MPYADEU-TABLA
                     MOVE CA-MPBADEU         TO MPYADEU-RUTINA
                     MOVE CA-PRRF-2200       TO MPYADEU-PARRAFO
                                                                        00013080
                     PERFORM 3000-FIN                                   00013090
                        THRU 3000-FIN-EXIT                              00013100
                                                                        00013110
           END-EVALUATE                                                 00013120
           .                                                            00007520
       2200-ACCEDER-MDDT750-EXIT.                                       00007530
           EXIT.                                                        00007540

      ******************************************************************00006670
      * 2300-ACCEDER-MDDT755.                                          *00006680
      * SE ACCEDE A LA TABLA MDDT755 PARA RECUPERAR EL VALOR DEL       *00006690
      * PARAMETRO.                                                     *00006690
      ******************************************************************00006700
       2300-ACCEDER-MDDT755.                                            00006710
                                                                        00006720
           INITIALIZE DCLMDDT755

           MOVE MPYADEU-CDGENTI-E            TO G3178-CDGENTI
           MOVE CA-ABU                       TO G3178-PANUMPAR
           MOVE MPYADEU-FECHA-E              TO G3178-FECALTA           00017840
                                                G3178-FEBAJA            00017850

           EXEC SQL                                                     00012740
                SELECT G3178_VALPARM                                    00012750
                 INTO :G3178-VALPARM                                    00012760
                  FROM MDDT755                                          00012770
                 WHERE G3178_CDGENTI   = :G3178-CDGENTI
                   AND G3178_PANUMPAR  = :G3178-PANUMPAR
                   AND G3178_FECALTA  <= :G3178-FECALTA                 00017950
                   AND G3178_FEBAJA   >= :G3178-FEBAJA                  00017960
                 FETCH FIRST 1 ROW ONLY                                 00017970
           END-EXEC                                                     00012780
                                                                        00012790
           MOVE SQLCODE                      TO SW-DB2-RETURN-CODE      00012800
                                                                        00012810
           EVALUATE  TRUE                                               00012820
               WHEN  DB2-OK                                             00012830
                     MOVE G3178-VALPARM      TO MPYADEU-VALPARM-S       00012840

               WHEN  DB2-CLV-NOT-FOUND
                     MOVE CA-88              TO MPYADEU-COD-RET
                     MOVE MM-MP0070          TO MPYADEU-MENSAJE
                     MOVE SQLCODE            TO MPYADEU-SQLCODE
                     MOVE SQLCA              TO MPYADEU-SQLCA
                     MOVE CA-MDDT755         TO MPYADEU-TABLA
                     MOVE CA-MPBADEU         TO MPYADEU-RUTINA
                     MOVE CA-PRRF-2300       TO MPYADEU-PARRAFO
                     MOVE CA-REG-NO-ENC-755  TO MPYADEU-TEXTO           00012850

                     PERFORM 3000-FIN
                        THRU 3000-FIN-EXIT

               WHEN  OTHER
                     MOVE CA-99              TO MPYADEU-COD-RET         00012870
                                                                        00012880
                     IF  DB2-RECURSO-NO-DISPONIBLE OR                   00012890
                         DB2-TABLA-BLOQUEADA                            00012900
                         MOVE MM-XX9520      TO MPYADEU-MENSAJE         00012910
                         MOVE CA-ERROR-RECURSO                          00012920
                                             TO MPYADEU-TEXTO           00012930
                                                                        00012940
                     ELSE                                               00012950
                         MOVE MM-XX3333      TO MPYADEU-MENSAJE         00012960
                         MOVE CA-ERROR-CONS-755                         00012970
                                             TO MPYADEU-TEXTO           00012980
                                                                        00012990
                     END-IF                                             00013000
                                                                        00013010
                     MOVE SQLCODE            TO MPYADEU-SQLCODE
                     MOVE SQLCA              TO MPYADEU-SQLCA
                     MOVE CA-MDDT755         TO MPYADEU-TABLA
                     MOVE CA-MPBADEU         TO MPYADEU-RUTINA
                     MOVE CA-PRRF-2300       TO MPYADEU-PARRAFO
                                                                        00013080
                     PERFORM 3000-FIN                                   00013090
                        THRU 3000-FIN-EXIT                              00013100
                                                                        00013110
           END-EVALUATE                                                 00013120
           .                                                            00007520
       2300-ACCEDER-MDDT755-EXIT.                                       00007530
           EXIT.                                                        00007540

      ******************************************************************00017580
      * 3000-FIN.                                                      *00017590
      * FINALIZA LA EJECUCION DE LA RUTINA.                            *00017600
      ******************************************************************00017610
       3000-FIN.                                                        00017620
                                                                        00017630
           GOBACK                                                       00017640
                                                                        00017650
           .                                                            00017660
       3000-FIN-EXIT.                                                   00017670
           EXIT.                                                        00017680
                                                                        00017690
