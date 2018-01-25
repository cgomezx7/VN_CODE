      ******************************************************************
      *                                                                *
      *  PROGRAMA   : MPCITI06                                         *
      *-                                                              -*
      *  FECHA CREACIÓN: 23/06/2017           AUTOR: VIEWNEXT CC       *
      *-                                                              -*
      *  APLICACIÓN : MP                                               *
      *-                                                              -*
      *  INSTALACIÓN: ISBAN                                            *
      *-                                                              -*
      *  DESCRIPCIÓN: PROGRAMA BATCH DE OPERACIONES DIARIAS            *
      *-                                                              -*
      *  FICHEROS DE ENTRADA:                                          *
      *     MPCITIE1: FICHERO CON LOS DATOS DE LAS OPERACIONES DIARIAS *
      *-                                                              -*
      *  FICHEROS DE SALIDA:                                           *
      *     MPCITIS1: FICHERO CON LOS DATOS DE LAS OPERACIONES DIARIAS *
      *-              EN FORMATO CSV.                                 -*
      *-                                                              -*
      ******************************************************************
      ******************************************************************
      *                  M O D I F I C A C I O N E S                   *
      *                  ***************************                   *
      *                                                                *
      * USUARIO  FECHA      DESCRIPCIÓN                                *
      * -------- ---------- ------------------------------------------ *
      *                                                                *
      ******************************************************************
      ******************************************************************
      * IDENTIFICATION DIVISION                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MPCITI06.
       AUTHOR.        VIEWNEXT.
       DATE-WRITTEN.  23/06/2017.
       DATE-COMPILED.

      ******************************************************************
      * ENVIRONMENT DIVISION                                           *
      ******************************************************************
       ENVIRONMENT DIVISION.

      *----------------------------------------------------------------*
      * CONFIGURATION SECTION                                          *
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
         SOURCE-COMPUTER.  IBM-3270.
         OBJECT-COMPUTER.  IBM-3270.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
      *  INPUT-OUTPUT SECTION                                          *
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      * -- FICHERO DE ENTRADA CON LA DESCARGA DEL DIA ACTUAL DE LA
      * -- TABLA NT_NOTAS
           SELECT MPCITIE1 ASSIGN MPCITIE1
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS FS-MPCITIE1.

      * -- FICHERO DE SALIDA CON LA INFORMACION DE LA DESCARGA DE LA
      * -- TABLA NT_NOTAS Y EL CAMPO ALTAMODIF ACTUALIZADO
           SELECT MPCITIS1 ASSIGN MPCITIS1
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS FS-MPCITIS1.

      ******************************************************************
      * DATA DIVISION                                                  *
      ******************************************************************
       DATA DIVISION.

      *----------------------------------------------------------------*
      *  FILE SECTION                                                  *
      *----------------------------------------------------------------*
       FILE SECTION.

      * -- FICHERO DE ENTRADA CON LAS ALTAS Y BAJAS DEL DIA
       FD  MPCITIE1
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 400 CHARACTERS
           DATA RECORD IS REG-MPCITIE1.
       01  REG-MPCITIE1                    PIC X(400).

      * -- FICHERO DE CON LAS ALTAS Y BAJAS DEL DIA CON DESCRIPCION
       FD  MPCITIS1
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 350 CHARACTERS
           DATA RECORD IS REG-MPCITIS1.
       01  REG-MPCITIS1                    PIC X(350).

      *----------------------------------------------------------------*
      * WORKING-STORAGE SECTION                                        *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      ******************************************************************
      *                         SWITCHES                               *
      ******************************************************************
       01  SWITCHES.
      *--  SWITCH PARA INDICAR QUE SE HA LLEGADO AL FINAL DEL FICHERO DE
      *--  ENTRADA 1.
           05  SW-MPCITIE1                 PIC X(01)   VALUE 'N'.
               88 SI-FIN-MPCITIE1                      VALUE 'S'.
               88 NO-FIN-MPCITIE1                      VALUE 'N'.

      ******************************************************************
      *                         CONSTANTES                             *
      ******************************************************************
       01  CONSTANTES.
           05  CA-CONSTANTES-ALFANUMERICAS.
               10  CA-D                    PIC X(01)   VALUE 'D'.
               10  CA-R                    PIC X(01)   VALUE 'R'.
      *--      TIPO DE ERROR: DE FICHERO
               10  CA-F                    PIC X(01)   VALUE 'F'.
      *--      POSIBLES FILE STATUS
               10  CA-FSOK                 PIC X(02)   VALUE '00'.
               10  CA-FSEOF                PIC X(02)   VALUE '10'.
      *--      NOMBRE DEL RESPONSABLE EN CASO DE FALLO
               10  CA-RESPONSABLE          PIC X(04)   VALUE SPACES.
      *--      NOMBRE DE LOS FICHEROS DEL PROGRAMA
               10  CA-MPCITIE1             PIC X(09)   VALUE 'MPCITIE1'.
               10  CA-MPCITIS1             PIC X(09)   VALUE 'MPCITIS1'.
      *--      PARRAFOS DE ERROR
               10 CA-PRRF-1100             PIC X(19)   VALUE '1100-ABRIR
      -                              '-FICHEROS'.
               10 CA-PRRF-3100             PIC X(19)   VALUE '3100-CERRA
      -                              'R-FICHERO'.
               10 CA-PRRF-9100             PIC X(20)   VALUE '9100-LEER-
      -                              'FICHERO1'.
               10 CA-PRRF-9200             PIC X(20)   VALUE '9200-ESCRI
      -                              'BIR-SALIDA'.
               10 CA-PRRF-9300             PIC X(22)   VALUE '9300-ESCRI
      -                              'BIR-CABECERA'.
      *--      ERRORES DE TRATAMIENTO DE FICHEROS
               10  CA-ERROR-FS-OPEN        PIC X(80)   VALUE 'ERROR EN A
      -                              'CCESO A FICHERO - OPEN'.
               10  CA-ERROR-FS-READ        PIC X(80)   VALUE 'ERROR EN A
      -                              'CCESO A FICHERO - READ'.
               10  CA-ERROR-FS-WRITE       PIC X(80)   VALUE 'ERROR EN A
      -                              'CCESO A FICHERO - WRITE'.
               10  CA-ERROR-FS-CLOSE       PIC X(80)   VALUE 'ERROR EN A
      -                              'CCESO A FICHERO - CLOSE'.
      *--      PARA LA ESTADISTICA DE SALIDA
               10  CA-SALIDA1              PIC X(039)  VALUE '  REGISTRO
      -                            'S ESCRITOS EN MPCITIS1: '.
               10  CA-CABECERA             PIC X(51)   VALUE '*
      -                            '          PROGRAMA MPCITI06
      -                            '    *'.
               10  CA-ENTRADA1             PIC X(039)  VALUE '  REGISTRO
      -                            'S LEIDOS EN EL MPCITIE1:     '.
               10  CA-ESPAC-AST            PIC X(011)  VALUE '
      -                            '*'.
               10  CA-ESPAC-ASTE           PIC X(003)  VALUE '  *'.
               10  CA-ASTERISCOS           PIC X(051)  VALUE '**********
      -                            '************************************
      -                            '*****'.

           05  CN-CONSTANTES-NUMERICAS.
               10  CN-1                    PIC 9(01)   VALUE 1.

      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01  CT-CONTADORES.
      *-   CONTADORES DE REGISTROS LEIDOS Y GRABADOS
           05  CT-LEIDOS-MPCITIE1          PIC 9(009)  VALUE ZEROS
                                                         USAGE COMP-3.
           05  CT-ESCRITOS-MPCITIS1        PIC 9(009)  VALUE ZEROS
                                                         USAGE COMP-3.
      *-   MASCARA PARA MOSTRAR LOS REGISTROS TRATADOS
           05  CT-EDIT-MPCITIE1            PIC ZZZ.ZZZ.ZZ9.
           05  CT-EDIT-MPCITIS1            PIC ZZZ.ZZZ.ZZ9.

      ******************************************************************
      * CAMPOS PARA LA FUNCION XX_CANCELACION_PROCESOS_BATCH           *
      ******************************************************************
       01  WK-CANCELACION-BATCH.
           05  WK-CANCELA.
      *   PARAMETROS GENERALES *****************************************
               10  WK-TIPO-ERROR           PIC X(001)  VALUE SPACES.
               10  WK-COD-RETORNO          PIC X(004)  VALUE SPACES.
               10  WK-RESPONSABLE          PIC X(030)  VALUE 'MP'.
               10  WK-DESCRIPCION          PIC X(080)  VALUE SPACES.
               10  WK-PROGRAMA             PIC X(008)  VALUE 'MPCITI06'.
               10  WK-PARRAFO              PIC X(030)  VALUE SPACES.
      *   ERRORES DE DB2 ***********************************************
           05  WK-ERROR-DB2.
               10  WK-SQLCA                PIC X(148)  VALUE SPACES.
               10  WK-TABLA-DB2            PIC X(015)  VALUE SPACES.
               10  WK-DATOS-ACCESO         PIC X(104)  VALUE SPACES.
      *   ERRORES DE RUTINA ********************************************
           05  WK-ERROR-RUTINA.
               10  WK-RUTINA               PIC X(008)  VALUE SPACES.
               10  WK-PARAMETROS           PIC X(114)  VALUE SPACES.
      *   ERRORES DE TABLA DE MEMORIA **********************************
           05  WK-ERROR-TABLA-MEMORIA.
               10  WK-TABLA-MEM            PIC X(030)  VALUE SPACES.
               10  WK-INDICE               PIC X(006)  VALUE ZEROES.
               10  WK-DATOS-TABMEM         PIC X(086)  VALUE SPACES.
      *   ERRORES DE FICHEROS ******************************************
           05  WK-ERROR-FICHERO.
               10  WK-DDNAME               PIC X(008)  VALUE SPACES.
               10  WK-FILE-STATUS          PIC X(002)  VALUE SPACES.
               10  WK-DATOS-REGISTRO       PIC X(112)  VALUE SPACES.

      ******************************************************************
      *                        FILE STATUS                             *
      ******************************************************************
       01  FS-FILE-STATUS.
           05  FS-MPCITIE1                 PIC X(02)   VALUE SPACES.
           05  FS-MPCITIS1                 PIC X(02)   VALUE SPACES.

      ******************************************************************00003260
      *                  C O P Y S  Y  D C L G E N S                   *00003270
      ******************************************************************00003280
      *COPY DEL FICHERO DE ENTRADA
       COPY MPYITI02.

      *COPY DEL FICHERO DE SALIDA
       COPY MPYITI06.

      *----------------------------------------------------------------*
      * PROCEDURE DIVISION                                             *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT

           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SI-FIN-MPCITIE1

           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT

           .

      ******************************************************************
      ***                        1000-INICIO                         ***
      **                         -----------                          **
      * - INICIALIZAMOS LAS VARIABLES A UTILIZAR.                      *
      * - ABRIMOS LOS FICHEROS DE ENTRADA Y DE SALIDA.                 *
      * - REALIZAMOS UNA LECTURA DE CADA UNO DE LOS FICHEROS DE ENTRA- *
      *   DA.                                                          *
      ******************************************************************
       1000-INICIO.

           INITIALIZE CT-CONTADORES


      *-   ABRIMOS LOS FICHEROS A UTILIZAR.
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT

           PERFORM 9300-ESCRIBIR-CABECERA
              THRU 9300-ESCRIBIR-CABECERA-EXIT

      *-   INDICAMOS QUE NO HEMOS LLEGADO AL FINAL DE NINGUN FICHERO
           SET NO-FIN-MPCITIE1               TO TRUE

      *-   LEEMOS DEL FICHERO 1.
           PERFORM 9100-LEER-FICHERO1
              THRU 9100-LEER-FICHERO1-EXIT

           .
       1000-INICIO-EXIT.
           EXIT.

      ******************************************************************
      ***                     1100-ABRIR-FICHEROS                    ***
      **                      -------------------                     **
      * SE ABREN LOS FICHEROS DE ENTRADA Y SALIDA                      *
      ******************************************************************
       1100-ABRIR-FICHEROS.

           OPEN INPUT MPCITIE1
               OUTPUT MPCITIS1

      *--  FICHERO DE ENTRADA 1
           IF  FS-MPCITIE1 NOT = CA-FSOK
               MOVE CA-F                     TO WK-TIPO-ERROR
               MOVE CA-ERROR-FS-OPEN         TO WK-DESCRIPCION
               MOVE CA-PRRF-1100             TO WK-PARRAFO
               MOVE CA-MPCITIE1              TO WK-DDNAME
               MOVE FS-MPCITIE1              TO WK-FILE-STATUS
               MOVE SPACES                   TO WK-DATOS-REGISTRO

               PERFORM 9000-CANCELACION
                  THRU 9000-CANCELACION-EXIT

           END-IF

      *--  FICHERO DE SALIDA
           IF  FS-MPCITIS1 NOT = CA-FSOK
               MOVE CA-F                     TO WK-TIPO-ERROR
               MOVE CA-ERROR-FS-OPEN         TO WK-DESCRIPCION
               MOVE CA-PRRF-1100             TO WK-PARRAFO
               MOVE CA-MPCITIS1              TO WK-DDNAME
               MOVE FS-MPCITIS1              TO WK-FILE-STATUS
               MOVE SPACES                   TO WK-DATOS-REGISTRO

               PERFORM 9000-CANCELACION
                  THRU 9000-CANCELACION-EXIT

           END-IF

           .
       1100-ABRIR-FICHEROS-EXIT.
           EXIT.

      ******************************************************************
      ***                     2000-PROCESO                           ***
      **                      ------------                            **
      * PROCESO PRINCIPAL DEL PROGRAMA                                 *
      * SE LEEN LAS OPERACIONES DIARIAS DEL FICHERO DE ENTRADA Y SE    *
      * ESCRIBEN EN EL FICHERO DE SALIDA CON FORMATO CSV               *
      ******************************************************************
       2000-PROCESO.

           PERFORM 2100-MOVER-SALIDA
              THRU 2100-MOVER-SALIDA-EXIT

           PERFORM 9200-ESCRIBIR-SALIDA
              THRU 9200-ESCRIBIR-SALIDA-EXIT

           PERFORM 9100-LEER-FICHERO1
              THRU 9100-LEER-FICHERO1-EXIT

           .
       2000-PROCESO-EXIT.
           EXIT.

      ******************************************************************
      ***                     2100-MOVER-SALIDA                      ***
      **                      -----------------                       **
      * PROCESO PRINCIPAL DEL PROGRAMA                                 *
      * SE MUEVEN LOS CAMPOS A LA COPY DE SALIDA PARA SU POSTERIOR     *
      * ESCRITURA EN EL FICHERO DE SALIDA                              *
      ******************************************************************
       2100-MOVER-SALIDA.

           INITIALIZE MPYITI06-DETALLE

           MOVE MPYITI02-NUMFORM             TO MPYITI06-NUMFORM
           MOVE MPYITI02-CODAUTOR            TO MPYITI06-CODAUTOR
           MOVE MPYITI02-NOMAUTOR            TO MPYITI06-NOMAUTOR
           MOVE MPYITI02-TIPAUTOR            TO MPYITI06-TIPAUTOR
           MOVE MPYITI02-DOCAUTOR            TO MPYITI06-DOCAUTOR
           MOVE MPYITI02-ALIEMPRE            TO MPYITI06-ALIEMPRE
           MOVE MPYITI02-IBANEMPR            TO MPYITI06-IBANEMPR
           MOVE MPYITI02-NUMOFIC             TO MPYITI06-NUMOFIC
           MOVE MPYITI02-MOTIVO-ERR          TO MPYITI06-MOTIVO-ERR
           MOVE MPYITI02-FECEMIS             TO MPYITI06-FECEMIS

           .
       2100-MOVER-SALIDA-EXIT.
           EXIT.

      ******************************************************************
      ***                    3000-FIN                                ***
      **                     --------                                 **
      ******************************************************************
       3000-FIN.

           PERFORM 3100-CERRAR-FICHERO
              THRU 3100-CERRAR-FICHERO-EXIT

           PERFORM 3200-ESTADISTICAS
              THRU 3200-ESTADISTICAS-EXIT

           STOP RUN

           .
       3000-FIN-EXIT.
           EXIT.

      ******************************************************************
      ***                    3100-CERRAR-FICHERO                     ***
      **                     -------------------                      **
      * SE CIERRAN LOS FICHEROS                                        *
      ******************************************************************
       3100-CERRAR-FICHERO.

           CLOSE MPCITIE1
                 MPCITIS1

           IF  FS-MPCITIE1 NOT = CA-FSOK
               MOVE CA-F                     TO WK-TIPO-ERROR
               MOVE CA-MPCITIE1              TO WK-DDNAME
               MOVE CA-PRRF-3100             TO WK-PARRAFO
               MOVE CA-ERROR-FS-CLOSE        TO WK-DESCRIPCION
               MOVE FS-MPCITIE1              TO WK-FILE-STATUS
               MOVE SPACES                   TO WK-DATOS-REGISTRO

               PERFORM 9000-CANCELACION
                  THRU 9000-CANCELACION-EXIT

           END-IF

           IF  FS-MPCITIS1 NOT = CA-FSOK
               MOVE CA-F                     TO WK-TIPO-ERROR
               MOVE CA-MPCITIS1              TO WK-DDNAME
               MOVE CA-PRRF-3100             TO WK-PARRAFO
               MOVE CA-ERROR-FS-CLOSE        TO WK-DESCRIPCION
               MOVE FS-MPCITIS1              TO WK-FILE-STATUS
               MOVE SPACES                   TO WK-DATOS-REGISTRO

               PERFORM 9000-CANCELACION
                  THRU 9000-CANCELACION-EXIT

           END-IF

           .
       3100-CERRAR-FICHERO-EXIT.
           EXIT.

      ******************************************************************
      ***                       3200-ESTADISTICAS                    ***
      **                        -----------------                     **
      *   SE MUESTRAN LAS ESTADISTICAS DEL PROCESO                     *
      ******************************************************************
       3200-ESTADISTICAS.

           MOVE CT-LEIDOS-MPCITIE1           TO CT-EDIT-MPCITIE1
           MOVE CT-ESCRITOS-MPCITIS1         TO CT-EDIT-MPCITIS1

           DISPLAY CA-ASTERISCOS
           DISPLAY CA-ASTERISCOS
           DISPLAY CA-CABECERA
           DISPLAY CA-ASTERISCOS
           DISPLAY CA-ENTRADA1 CT-EDIT-MPCITIE1
           DISPLAY CA-SALIDA1  CT-EDIT-MPCITIS1
           DISPLAY CA-ASTERISCOS
           DISPLAY CA-ASTERISCOS

           .
       3200-ESTADISTICAS-EXIT.
           EXIT.

      ******************************************************************
      * 9000-CANCELACION.                                              *
      * CANCELA LA EJECUCION DEL PROGRAMA PARA INFORMAR DEL ERROR.     *
      ******************************************************************
       9000-CANCELACION.

           EVALUATE  WK-TIPO-ERROR
               WHEN  CA-D
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH
                         TIPO_ERROR('WK-TIPO-ERROR')
                         COD_RETORNO('WK-COD-RETORNO')
                         RESPONSABLE('WK-RESPONSABLE')
                         DESCRIPCION('WK-DESCRIPCION')
                         PROGRAMA('WK-PROGRAMA')
                         PARRAFO('WK-PARRAFO')
                         SQLCA('WK-SQLCA')
                         TABLA_DB2('WK-TABLA-DB2')
                         DATOS_ACCESO('WK-DATOS-ACCESO')
                     END-FUN

               WHEN  CA-R
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH
                         TIPO_ERROR('WK-TIPO-ERROR')
                         COD_RETORNO('WK-COD-RETORNO')
                         RESPONSABLE('WK-RESPONSABLE')
                         DESCRIPCION('WK-DESCRIPCION')
                         PROGRAMA('WK-PROGRAMA')
                         PARRAFO('WK-PARRAFO')
                         RUTINA('WK-RUTINA')
                         PARAMETROS('WK-PARAMETROS')
                     END-FUN

               WHEN  CA-F
                     EXEC-FUN XX_CANCELACION_PROCESOS_BATCH
                         TIPO_ERROR('WK-TIPO-ERROR')
                         RESPONSABLE('WK-RESPONSABLE')
                         DESCRIPCION('WK-DESCRIPCION')
                         PROGRAMA('WK-PROGRAMA')
                         PARRAFO('WK-PARRAFO')
                         DDNAME('WK-DDNAME')
                         FILE_STATUS('WK-FILE-STATUS')
                         DATOS_REGISTRO('WK-DATOS-REGISTRO')
                     END-FUN

           END-EVALUATE

           .
       9000-CANCELACION-EXIT.
           EXIT.
      ******************************************************************
      ***                       9100-LEER-FICHERO1                   ***
      **                        ------------------                    **
      *  SE LEE DEL FICHERO MPCITIE1 Y SE COMPRUEBA SU FILE STATUS     *
      ******************************************************************
       9100-LEER-FICHERO1.

           INITIALIZE MPYITI02

           READ MPCITIE1 INTO MPYITI02

           EVALUATE  FS-MPCITIE1
               WHEN  CA-FSOK
      *-             INCREMENTAMOS EL CONTADOR DE LEIDOS EN UNO
                     ADD CN-1                  TO CT-LEIDOS-MPCITIE1

               WHEN  CA-FSEOF
      *-             NO HAY MAS DATOS A LEER DEL FICHERO 1.
                     SET SI-FIN-MPCITIE1       TO TRUE

               WHEN  OTHER
                     MOVE CA-F                 TO WK-TIPO-ERROR
                     MOVE CA-ERROR-FS-READ     TO WK-DESCRIPCION
                     MOVE CA-PRRF-9100         TO WK-PARRAFO
                     MOVE CA-MPCITIE1          TO WK-DDNAME
                     MOVE FS-MPCITIE1          TO WK-FILE-STATUS
                     MOVE MPYITI02             TO WK-DATOS-REGISTRO

                     PERFORM 9000-CANCELACION
                        THRU 9000-CANCELACION-EXIT

           END-EVALUATE

           .
       9100-LEER-FICHERO1-EXIT.
           EXIT.


      ******************************************************************
      ***                 9200-ESCRIBIR-SALIDA                       ***
      **                  --------------------                        **
      *  SE ESCRIBE EN EL FICHERO DE SALIDA                            *
      *                                                                *
      ******************************************************************
       9200-ESCRIBIR-SALIDA.

           WRITE REG-MPCITIS1 FROM MPYITI06-DETALLE

           EVALUATE  FS-MPCITIS1
               WHEN  CA-FSOK
      *-             INCREMENTAMOS EL CONTADOR DE ESCRITOS EN UNO
                     ADD CN-1                  TO CT-ESCRITOS-MPCITIS1

               WHEN  OTHER
                     MOVE CA-F                 TO WK-TIPO-ERROR
                     MOVE CA-ERROR-FS-WRITE    TO WK-DESCRIPCION
                     MOVE CA-PRRF-9300         TO WK-PARRAFO
                     MOVE CA-MPCITIS1          TO WK-DDNAME
                     MOVE FS-MPCITIS1          TO WK-FILE-STATUS
                     MOVE MPYITI06-DETALLE     TO WK-DATOS-REGISTRO

                     PERFORM 9000-CANCELACION
                        THRU 9000-CANCELACION-EXIT

           END-EVALUATE

           .
       9200-ESCRIBIR-SALIDA-EXIT.
           EXIT.

      ******************************************************************
      ***                 9200-ESCRIBIR-CABECERA                     ***
      **                  --------------------                        **
      *  SE ESCRIBE LA CABECERA DEL FICHERO DE SALIDA                  *
      ******************************************************************
       9300-ESCRIBIR-CABECERA.

           WRITE REG-MPCITIS1 FROM MPYITI06-CABECERA

           EVALUATE  FS-MPCITIS1
               WHEN  CA-FSOK
                     CONTINUE

               WHEN  OTHER
                     MOVE CA-F                 TO WK-TIPO-ERROR
                     MOVE CA-ERROR-FS-WRITE    TO WK-DESCRIPCION
                     MOVE CA-PRRF-9300         TO WK-PARRAFO
                     MOVE CA-MPCITIS1          TO WK-DDNAME
                     MOVE FS-MPCITIS1          TO WK-FILE-STATUS
                     MOVE MPYITI06-CABECERA    TO WK-DATOS-REGISTRO

                     PERFORM 9000-CANCELACION
                        THRU 9000-CANCELACION-EXIT

           END-EVALUATE

           .
       9300-ESCRIBIR-CABECERA-EXIT.
           EXIT.

