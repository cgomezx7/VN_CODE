      ******************************************************************00000010
      *                       *  KJYCCPD *                             *00000020
      *--------------------------------------------------------------- *00000030
      *        COPY DE COMUNICACION DE LA RUTINA KJBCCPD               *00000040
      ******************************************************************00000050
        01  KJYCCPD.                                                    00000060
            05  KJYCCPD-ENTRADA.                                        00000070
                10  KJYCCPD-IDEMPR             PIC X(04).               00000080
                10  KJYCCPD-IDCENT             PIC X(04).               00000090
                10  KJYCCPD-IDPROD             PIC X(03).               00000100
                10  KJYCCPD-CODSPROD           PIC X(03).               00000110
                10  KJYCCPD-CTOSALDO           PIC X(03).               00000120
                10  KJYCCPD-CODMONSW           PIC X(03).               00000130
                10  KJYCCPD-IMPORTE-CONSO      PIC S9(15)V9(2) COMP-3.  00000140
                10  KJYCCPD-DIA-CONTABLE       PIC X(02).               00000150
                10  KJYCCPD-FECHA-CONTABLE     PIC X(10).               00000160
                                                                        00000170
            05  KJYCCPD-DATOS-CONTROL.                                  00000180
                10  KJYCCPD-RETORNO            PIC X(02).               00000190
                10  KJYCCPD-MENSAJE            PIC X(06).               00000200
                10  KJYCCPD-DESCRIPCION        PIC X(50).               00000210
                10  KJYCCPD-RUTINA             PIC X(08).               00000220
                10  KJYCCPD-FUNCION            PIC X(08).               00000230
                10  KJYCCPD-PARRAFO            PIC X(30).               00000240
                10  KJYCCPD-TABLA              PIC X(18).               00000250
                10  KJYCCPD-ACCESO             PIC X(08).               00000260
                10  KJYCCPD-SQLCODE            PIC S9(09).              00000270
                10  KJYCCPD-SQLCA              PIC X(136).              00000280
                10  KJYCCPD-DATOS              PIC X(50).               00000290
