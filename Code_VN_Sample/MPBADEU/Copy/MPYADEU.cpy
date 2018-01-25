      ******************************************************************00000010
      * NOMBRE         : MPYADEU.                                      *00000020
      * DESCRIPCION    : COPY DE LA RUTINA MIXTA MPBADEU.              *00000030
      ******************************************************************00000040
       01  MPYADEU.                                                     00000050
           05  MPYADEU-ENTRADA.                                         00000060
               10  MPYADEU-CDGENTI-E       PIC X(04).                   00000070
               10  MPYADEU-CENTALTA-E      PIC X(04).                   00000080
               10  MPYADEU-CUENTNU-E       PIC X(12).                   00000090
               10  MPYADEU-FECHA-E         PIC X(10).                   00000100
               10  MPYADEU-CDGPRODU-E      PIC X(03).                   00000110
               10  MPYADEU-CDGSUBP-E       PIC X(03).                   00000120
           05  MPYADEU-SALIDA.                                          00000130
               10  MPYADEU-VALPARM-S       PIC X(10).                   00000140
           05  MPYADEU-DATOS-CONTROL.                                   00000150
               10  MPYADEU-COD-RET         PIC X(02).                   00000160
               10  MPYADEU-MENSAJE         PIC X(06).                   00000170
               10  MPYADEU-ELEMENTO        PIC X(08).                   00000180
               10  MPYADEU-SQLCODE         PIC S9(09).                  00000190
               10  MPYADEU-SQLCA           PIC X(150).                  00000200
               10  MPYADEU-TABLA           PIC X(18).                   00000210
               10  MPYADEU-RUTINA          PIC X(08).                   00000220
               10  MPYADEU-PARRAFO         PIC X(30).                   00000230
               10  MPYADEU-TEXTO           PIC X(50).                   00000240
                                                                        00000250
