       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP005.
       AUTHOR. GUSTAVO LUIGI ZANIBAO.
      *******************************************************
      * CADASTRO DE CEP  *
      *******************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                    SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
            03 CEP              PIC 9(08).
            03 ENDERECO         PIC X(30).
            03 BAIRRO           PIC X(20).
            03 CIDADE           PIC X(20).
            03 ESTADO.
               05 COD-ESTADO    PIC XX.
               05 DESCR-ESTADO  PIC X(30).

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
               VALUE  " CEP".
           05  LINE 05  COLUMN 01 
               VALUE  "     CEP      :".
           05  LINE 07  COLUMN 01 
               VALUE  "     ENDERECO :".
           05  LINE 09  COLUMN 01 
               VALUE  "     BAIRRO   :".
           05  LINE 11  COLUMN 01 
               VALUE  "     CIDADE   :".
           05  LINE 13  COLUMN 01 
               VALUE  "     ESTADO   :    -".
           05  LINE 20  COLUMN 01 
               VALUE  "     MENSAGEM :".
           05  TCEP
               LINE 05  COLUMN 17  PIC 9(08)
               USING  CEP.
           05  TENDERECO
               LINE 07  COLUMN 17  PIC X(30)
               USING  ENDERECO.
           05  TBAIRRO
               LINE 09  COLUMN 17  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 11  COLUMN 17  PIC X(20)
               USING  CIDADE.
           05  TCOD-ESTADO
               LINE 13  COLUMN 17  PIC X(02)
               USING  COD-ESTADO.
           05  TDESCR-ESTADO
               LINE 13  COLUMN 22  PIC X(30)
               USING  DESCR-ESTADO.



      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       ABRIR-ARQ.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "30"
                    OPEN OUTPUT CADCEP
                    CLOSE CADCEP
                    GO TO ABRIR-ARQ
                ELSE
                     MOVE "ERRO NA ABERTURA DO ARQUIVO" TO W-MENS
                     PERFORM ROT-MENS THRU ROT-MENS2
                    GO TO ROT-FIM.
      *
       ROT-INIC.
           MOVE 0 TO CEP.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE COD-ESTADO
               DESCR-ESTADO.
       R0.
           DISPLAY TELA.
       R1.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
           IF CEP = ZEROS
                MOVE "CODIGO NAO PODE SER ZERO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R1.
       LER-ARQ. 
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELA
                      MOVE "*** CODIGO JA CADASTRAD0 ***" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      MOVE "I" TO W-SEL
                      GO TO ACE-001
                   ELSE
                      DISPLAY (20, 12) ST-ERRO
                      MOVE "ERRO NA LEITURA ARQ CADCEP"   TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      GO TO ROT-FIM.
       R2.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                GO TO R1.
           IF ENDERECO = SPACES
                MOVE "ENDERECO NAO PODE FICAR EM BRANCO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.

       R3.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                GO TO R2.
           IF BAIRRO = SPACES
                MOVE "BAIRRO NAO PODE FICAR EM BRANCO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R3.

       R4.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                GO TO R3.
           IF CIDADE = SPACES
                MOVE "CIDADE NAO PODE FICAR EM BRANCO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R4.
    
       R5.
           ACCEPT TCOD-ESTADO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
               GO TO R4.
           GO TO R-ESTADO.

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
                WRITE REGCEP
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
                DELETE CADCEP RECORD
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
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *----------------------[ROTINA DE CODIGOS]----------------------

       R-ESTADO.
           IF COD-ESTADO = "AC"
               MOVE "ACRE" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "AL"
               MOVE "ALAGOAS" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "AP"
               MOVE "AMAPA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "AM"
               MOVE "AMAZONAS" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "BA"
               MOVE "BAHIA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "CE"
               MOVE "CEARA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "ES"
               MOVE "ESPIRITO SANTO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "GO"
               MOVE "GOIAS" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "MA"
               MOVE "MARANHAO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "MT"
               MOVE "MATO GROSSO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "MS"
               MOVE "MATO GROSSO DO SUL" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "MG"
               MOVE "MINAS GERAIS" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "PR"
               MOVE "PARA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "PB"
               MOVE "PARAIBA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "PR"
               MOVE "PARANA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "PE"
               MOVE "PERNANBUCO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "PI"
               MOVE "PIAUI" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "RJ"
               MOVE "RIO DE JANEIRO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "RN"
               MOVE "RIO GRANDE DO NORTE" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "RS"
               MOVE "RIO GRANDE DO SUL" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "RO"
               MOVE "RONDONIA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "RR"
               MOVE "RORAIMA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "SC"
               MOVE "SANTA CATARINA" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "SP"
               MOVE "SAO PAULO" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "SE"
               MOVE "SERGIPE" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT. 
           IF COD-ESTADO = "TO"
               MOVE "TOCANTINS" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
           IF COD-ESTADO = "DF"
               MOVE "DESTRITO FEDERAL" TO DESCR-ESTADO
               DISPLAY TDESCR-ESTADO
               GO TO INC-ALT.
            MOVE "ESTADO INVALIDO" TO W-MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R5.            


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





