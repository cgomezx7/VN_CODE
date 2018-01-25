      ***************************************************************** 00000010
      *  MPYITI06: PLANTILLA REPORTES OPERACIONES - INGRESOS CON PIN  * 00000020
      *       LREC: 350                                               * 00000030
      ***************************************************************** 00000040
      *                                                                 00000050
       01 MPYITI06-REGISTRO.                                            00000060
          05 MPYITI06-CABECERA.                                         00000070
            10 CTA-NUMFORM              PIC X(10)  VALUE 'FORMULARIO'.  00000080
            10 FILLER                   PIC X(01)  VALUE ';'.           00000090
            10 CTA-CODAUTOR             PIC X(09)  VALUE 'COD.AUTOR'.   00000100
            10 FILLER                   PIC X(01)  VALUE ';'.           00000110
            10 CTA-NOMAUTOR             PIC X(25)                       00000120
                 VALUE 'NOMBRE DEL AUTORIZADO.   '.                     00000130
            10 FILLER                   PIC X(125) VALUE SPACES.        00000140
            10 FILLER                   PIC X(01)  VALUE ';'.           00000150
            10 CTA-TIPAUTOR             PIC X(01).                      00000160
            10 FILLER                   PIC X(01)  VALUE ';'.           00000170
            10 CTA-DOCAUTOR             PIC X(09)  VALUE 'DOC.AUTOR'.   00000180
            10 FILLER                   PIC X(01)  VALUE ';'.           00000190
            10 CTA-ALIEMPRE             PIC X(25)                       00000200
                 VALUE 'NOMBRE DE LA EMPRESA.    '.                     00000210
            10 FILLER                   PIC X(25) VALUE SPACES.         00000220
            10 FILLER                   PIC X(01)  VALUE ';'.           00000230
            10 CTA-IBANEMPR             PIC X(24)                       00000240
                 VALUE 'IBAN DE LA EMPRESA.     '.                      00000250
            10 FILLER                   PIC X(01)  VALUE ';'.           00000260
            10 CTANUMOFIC               PIC X(04)  VALUE 'OFIC'.        00000270
            10 FILLER                   PIC X(01)  VALUE ';'.           00000280
            10 CTA-MOTIVO-ERR           PIC X(50)                       00000290
             VALUE 'MOTIVO DEL ERROR.                                 '.00000300
            10 FILLER                   PIC X(01)  VALUE ';'.           00000310
            10 CTA-FECEMIS              PIC X(10)  VALUE 'F. EMISION'.  00000320
            10 FILLER                   PIC X(01)  VALUE ';'.           00000330
            10 FILLER                   PIC X(38)  VALUE SPACES.        00000340
      *                                                                 00000350
          05 MPYITI06-DETALLE.                                          00000360
            10 MPYITI06-NUMFORM         PIC 9(10).                      00000370
            10 FILLER                   PIC X(01)  VALUE ';'.           00000380
            10 MPYITI06-CODAUTOR        PIC 9(09).                      00000390
            10 FILLER                   PIC X(01)  VALUE ';'.           00000400
            10 MPYITI06-NOMAUTOR        PIC X(150).                     00000410
            10 FILLER                   PIC X(01)  VALUE ';'.           00000420
            10 MPYITI06-TIPAUTOR        PIC X(01).                      00000430
            10 FILLER                   PIC X(01)  VALUE ';'.           00000440
            10 MPYITI06-DOCAUTOR        PIC 9(09).                      00000450
            10 FILLER                   PIC X(01)  VALUE ';'.           00000460
            10 MPYITI06-ALIEMPRE        PIC X(50).                      00000470
            10 FILLER                   PIC X(01)  VALUE ';'.           00000480
            10 MPYITI06-IBANEMPR        PIC X(24).                      00000490
            10 FILLER                   PIC X(01)  VALUE ';'.           00000500
            10 MPYITI06-NUMOFIC         PIC X(04).                      00000510
            10 FILLER                   PIC X(01)  VALUE ';'.           00000520
            10 MPYITI06-MOTIVO-ERR      PIC X(50).                      00000530
            10 FILLER                   PIC X(01)  VALUE ';'.           00000540
            10 MPYITI06-FECEMIS         PIC X(10).                      00000550
            10 FILLER                   PIC X(01)  VALUE ';'.           00000560
            10 FILLER                   PIC X(38)  VALUE SPACES.        00000570
