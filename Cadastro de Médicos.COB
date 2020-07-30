       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
       AUTHOR. GUSTAVO LUIGI ZANIBAO.
      *******************************************************
      * CADASTRO DE CONVENIO  *
      *******************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                    SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                                      WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
            03 CODIGO          PIC 9(04).
            03 NOME            PIC X(30).
            03 PLANOS.
               05 PLANO        PIC 99.
               05 DESCR-PLANO  PIC X(30).

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT    PIC 9(06) VALUE ZEROS.
       01 W-MENS    PIC X(50) VALUE SPACES.
       01 W-LIMPA   PIC X(50) VALUE SPACES.
       01 W-SEL     PIC X(01) VALUE SPACES.
       01 W-OPCAO   PIC X(01) VALUE SPACES.
       01 ST-ERRO   PIC X(02) VALUE "00".
       01 W-ACT     PIC 9(02) VALUE ZEROS.
       01 VALIDADO  PIC 9 VALUE 1.


      *-----------------------------------------------------------------
       SCREEN SECTION.
       
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " CONVENIO".
           05  LINE 05  COLUMN 01 
               VALUE  "    CODIGO :".
           05  LINE 07  COLUMN 01 
               VALUE  "    NOME   :".
           05  LINE 09  COLUMN 01 
               VALUE  "    PLANO  :    -".
           05  LINE 20  COLUMN 01 
               VALUE  "    MENSAGEM :".
           05  TCODIGO
               LINE 05  COLUMN 14  PIC 9(04)
               USING  CODIGO.
           05  TNOME
               LINE 07  COLUMN 14  PIC X(30)
               USING  NOME.
           05  TPLANO
               LINE 09  COLUMN 14  PIC 9(02)
               USING  PLANO.
           05  TDESCR-PLANO
               LINE 09  COLUMN 19  PIC X(30)
               USING  DESCR-PLANO.
 

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       ABRIR-ARQ.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "30"
                    OPEN OUTPUT CADCONV
                    CLOSE CADCONV
                    GO TO ABRIR-ARQ
                ELSE
                     MOVE "ERRO NA ABERTURA DO ARQUIVO" TO W-MENS
                     PERFORM ROT-MENS THRU ROT-MENS2
                    GO TO ROT-FIM.
      *
       ROT-INIC.
           MOVE 0 TO CODIGO PLANO.
           MOVE SPACES TO NOME DESCR-PLANO.
       R0.
           DISPLAY TELA.
       R1.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                   CLOSE CADCONV
                   GO TO ROT-FIM.
           IF CODIGO = ZEROS
                MOVE "CODIGO NAO PODE SER ZERO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R1.
       LER-ARQ. 
                READ CADCONV
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELA
                      MOVE "*** CODIGO JA CADASTRAD0 ***" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      MOVE "I" TO W-SEL
                      GO TO ACE-001
                   ELSE
                      DISPLAY (20, 12) ST-ERRO
                      MOVE "ERRO NA LEITURA ARQ CADCONV"   TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      GO TO ROT-FIM.
       R2.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                GO TO R1.
           IF NOME = SPACES
                MOVE "NOME NAO PODE FICAR EM BRANCO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.
    
       R3.
           ACCEPT TPLANO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO R2.
           GO TO R-PLANO.

       INC-ALT.

           IF W-SEL = "A" 
               GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "GRAVAR (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R2.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO USUARIO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "** DADOS GRAVADOS COM SUCESSO **" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-INIC.
                IF ST-ERRO = "22"
                      MOVE "*** CADASTRO JA EXISTENTE ***" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-INIC
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                                       TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *--------------------[ FIM DA ROTINA DE INCLUSAO ]--------------------
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 13)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                                   AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO W-MENS
                DISPLAY (23, 12) W-MENS
                IF W-OPCAO = "A"
                       MOVE "A" TO W-SEL
                       GO TO R2.
                IF W-OPCAO  = "N"
                         GO TO ROT-INIC.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO *** " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO " TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* INFORMACOES NAO ALTERADAS * " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *----------------------[ROTINA DE CODIGOS]----------------------

       R-PLANO.
           IF PLANO < 1 OR PLANO > 10
                MOVE "PLANO INVALIDO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R3.
           IF PLANO = 1
               MOVE "ENFERMARIA REGIONAL" TO DESCR-PLANO.
           IF PLANO = 2
               MOVE "ENFERMARIA NACIONAL" TO DESCR-PLANO.
           IF PLANO = 3
               MOVE "ENFERMARIA INTERNACIONAL" TO DESCR-PLANO.
           IF PLANO = 4
               MOVE "APTO PADRAO REGIONAL" TO DESCR-PLANO.
           IF PLANO = 5
               MOVE "APTO PADRAO NACIONAL" TO DESCR-PLANO.
           IF PLANO = 6
               MOVE "APTO PADRAO INTERNACIONAL" TO DESCR-PLANO.
           IF PLANO = 7
               MOVE "EMERGENCIA REGIONAL" TO DESCR-PLANO.
           IF PLANO = 8
               MOVE "EMERGENCIA NACIONAL" TO DESCR-PLANO.
           IF PLANO = 9
               MOVE "EMERGENCIA INTERNACIONAL" TO DESCR-PLANO.
           IF PLANO = 10
               MOVE "PLANO GLOBAL" TO DESCR-PLANO.
           DISPLAY TDESCR-PLANO   
           GO TO INC-ALT.



      *--------------------[ ROTINA DE MENSAGEM ]--------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 16) W-MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO W-MENS
                   DISPLAY (20, 16) W-MENS.
       ROT-MENS-FIM.
                EXIT.

       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.
       ROT-FIMS.
                STOP RUN.

      *        TABELA DE ST-ERRO  (FILE STATUS)
      *        ==============================
      * 00 Comando executado com sucesso 
      * 02 Comando arquivo c/chave duplicada executado com sucesso 
      * 10 Fim do arquivo 
      * 21 Erro de sequencia na gravação de arquivo sequencial 
      * 22 Chave duplicada não definida.  
      * 23 Chave não encontrada. 
      * 24 Espaço em disco está cheio 
      * 30 Arquivo não encontrado 
      * 91 Erro na estrutura do arquivo 
      * 94 Registro ou arquivo sendo utilizado por outro programa.
      * 95 GERENCIADOR DE ARQUIVO NAO CARREGADO (ISAM) 





