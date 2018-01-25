      ***************************************************************** 00000010
      *  MPYITI02 FICHERO SALIDA ALTAS/BAJAS INGRESOS CON PIN.        * 00000020
      *  LONG = 400                                                   * 00000030
      ***************************************************************** 00000040
       01 MPYITI02.                                                     00000050
          05 MPYITI02-NUMFORM               PIC 9(10).                  00000060
          05 MPYITI02-NUMOFIC               PIC 9(4).                   00000070
          05 MPYITI02-ALIEMPRE              PIC X(50).                  00000080
          05 MPYITI02-IBANEMPR              PIC X(24).                  00000090
          05 MPYITI02-CODAUTOR              PIC X(09).                  00000100
          05 MPYITI02-NOMAUTOR              PIC X(150).                 00000110
          05 MPYITI02-TIPAUTOR              PIC X(01).                  00000120
          05 MPYITI02-DOCAUTOR              PIC X(09).                  00000130
          05 MPYITI02-FECEMIS               PIC X(10).                  00000140
          05 MPYITI02-TOKEN                 PIC X(08).                  00000150
          05 MPYITI02-MOTIVO-ERR            PIC X(50).                  00000160
          05 MPYITI02-IND-ALTA-BAJ          PIC X(01).                  00000170
          05 FILLER                         PIC X(74) VALUE SPACES.     00000180
