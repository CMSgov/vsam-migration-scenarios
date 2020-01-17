      ********************** | ****************************************
      ***                                                             *
      *** MEDICARE PART A                                             *
      *** FISS SYSTEM DOCUMENTATION                                   *
      ***                                                             *
      *|* MEMBER: FSRCCLXS                                            *
      ***                                                             *
      *** DESCRIPTION: CLAIM EXTRACT SUMMARY PORTION                  *
      ***                                                             *
      *** FIXED LRECL: 250 BYTES                                      *
      ***                                                             *
      *****************************************************************
      ***                                                             *
      *** ANY CHANGES MAY REQUIRE UPDATES TO SOME OF THE FOLLOWING:   *
      ***   `CLMB, `CLML, `CLMP, `CLMQ, `CLMR, `CLMS, `CLMX,          *
      ***   'CLMY, `IDRC, `PDCL, `PDCP, `PDCR, `259R, SFSPDCL.        *
      ***                                                             *
      *** THIS COPYBOOK IS USED IN OTHER COPYBOOKS.                   *
      ***                                                             *
      ***$************************************************************$
           10  :FSSC:-SUMMARY-DATA.
             15  :FSSC:-CLMS-KEY.
               20  :FSSC:-HIC-NO                         PIC X(12).
               20  :FSSC:-DCN.
                 25  :FSSC:-DCN-PLAN-CD                  PIC X(1).
                 25  :FSSC:-DCN-JULIAN.
                   30  :FSSC:-DCN-YR                     PIC 9(2).
                   30  :FSSC:-DCN-JUL-DT                 PIC 9(3).
                 25  :FSSC:-DCN-BTCH-NBR-X.
                   30  :FSSC:-DCN-BTCH-NBR               PIC 9(4).
                 25  :FSSC:-DCN-CLM-SEQ-NBR              PIC 9(2).
                 25  :FSSC:-DCN-SPLIT-CD                 PIC X(1).
                   88  :FSSC:-DCN13-C-CHOICES             VALUE 'C'.
                   88  :FSSC:-DCN13-V-VET-ADMN            VALUE 'V'.
C10171             88  :FSSC:-DCN13-Y-CHOICES             VALUE 'Y'.
                   88  :FSSC:-DCN13-0-DEFAULT             VALUE '0'.
                 25  :FSSC:-DCN-ORIG-CD                  PIC X(1).
                   88  :FSSC:-DCN-ORIG-0-UNKNOWN          VALUE '0'.
                   88  :FSSC:-DCN-ORIG-1-EMC-UB           VALUE '1'.
                   88  :FSSC:-DCN-ORIG-2-EMC-TAPE         VALUE '2'.
                   88  :FSSC:-DCN-ORIG-3-PRO-ADJ          VALUE '3'.
                   88  :FSSC:-DCN-ORIG-4-DDE              VALUE '4'.
                   88  :FSSC:-DCN-ORIG-5-EMC-NOT-UB       VALUE '5'.
                   88  :FSSC:-DCN-ORIG-6-OTHER-UB         VALUE '6'.
                   88  :FSSC:-DCN-ORIG-7-OTHER-N-UB       VALUE '7'.
                   88  :FSSC:-DCN-ORIG-8-HARD-COPY        VALUE '8'.
                   88  :FSSC:-DCN-ORIG-9-OTHER-HC         VALUE '9'.
                 25  :FSSC:-DCN-BSI                      PIC X(3).
                 25  :FSSC:-DCN-FUTURE                   PIC X(1).
                   88  :FSSC:-DCN18-A-IUR-VAD-IMPLT       VALUE 'A'.
                   88  :FSSC:-DCN18-B-IUR-DT-OF-DTH       VALUE 'B'.
                   88  :FSSC:-DCN18-I-IUR-IPF-STAY        VALUE 'I'.
                   88  :FSSC:-DCN18-S-IUR-IHS-TRIBL       VALUE 'S'.
                   88  :FSSC:-DCN18-R-IUR-LTCH-STAY       VALUE 'R'.
                   88  :FSSC:-DCN18-U-IUR-TRL24-GEN       VALUE 'U'.
                   88  :FSSC:-DCN18-Z-IUR-INV-PA-ST       VALUE 'Z'.
                 25  :FSSC:-DCN-FUTURE2                  PIC X(3).
                 25  :FSSC:-DCN-SITE-ID                  PIC X(2).
               20  :FSSC:-TRAILER-SEQ                    PIC 9(2).
                 88  :FSSC:-TRAILER-SEQ-00-BASE           VALUE 00.
                 88  :FSSC:-TRAILER-SEQ-LINES             VALUES
                                                           01 THRU 25.
             15  :FSSC:-COMPRESS-IND                     PIC X(1).
                 88  :FSSC:-COMPRESS-C-COMPRESS           VALUE 'C'.
                 88  :FSSC:-COMPRESS-U-UNCOMPRESS         VALUE 'U'.
             15  :FSSC:-PHYS-LENGTH                 COMP PIC S9(4).
             15  :FSSC:-CURR-STATUS-LOC.
               20  :FSSC:-CURR-STATUS                    PIC X(1).
FS0231           88  :FSSC:-CURR-STAT-BLANK               VALUE SPACES.
FS0231           88  :FSSC:-CURR-STAT-A-ROUTING           VALUE 'A'.
                 88  :FSSC:-CURR-STAT-S-SUSPEND           VALUE 'S'.
                 88  :FSSC:-CURR-STAT-P-PAID              VALUE 'P'.
                 88  :FSSC:-CURR-STAT-D-DENIED            VALUE 'D'.
CR7873           88  :FSSC:-CURR-STAT-I-INACTIVE          VALUE 'I'.    CR7873R1
                 88  :FSSC:-CURR-STAT-R-REJECT            VALUE 'R'.
CR7873           88  :FSSC:-CURR-STAT-T-RTP               VALUE 'T'.    CR7873R1
FS0521           88  :FSSC:-CURR-STAT-M-MOVE              VALUE 'M'.
                 88  :FSSC:-CURR-STAT-FINAL               VALUES
                                                           'P' 'D' 'R'.
FS0231           88  :FSSC:-CURR-STAT-ROUTING             VALUES
FS0231                                                     'A' SPACES.
               20  :FSSC:-CURR-LOCATION.
                 25  :FSSC:-CURR-LOC-1                   PIC X(1).
                   88  :FSSC:-CURR-LOC-B-BATCH            VALUE 'B'.
                   88  :FSSC:-CURR-LOC-M-MANUAL           VALUE 'M'.
                 25  :FSSC:-CURR-LOC-2                   PIC X(4).
                   88  :FSSC:-CURR-LOC-0100-PREPROC       VALUE '0100'.
FS0070             88  :FSSC:-CURR-LOC-01LC-UNMATCH       VALUE '01LC'.
                   88  :FSSC:-CURR-LOC-9000-CABLE         VALUE '9000'.
                   88  :FSSC:-CURR-LOC-9900-CABLED        VALUE '9900'.
                   88  :FSSC:-CURR-LOC-9996-FLOOR         VALUE '9996'.
                   88  :FSSC:-CURR-LOC-9997-FINAL         VALUE '9997'.
CR7873             88  :FSSC:-CURR-LOC-9998-OFFLINE       VALUE '9998'. CR7873R1
CR9630             88  :FSSC:-CURR-LOC-019M-XWALKDN       VALUE '019M'.
                   88  :FSSC:-CURR-LOC-75XX-POSTPAY       VALUE '7500'
                                                       THRU '7599'.
             15  :FSSC:-MEDA-PROV-ID.
               20  :FSSC:-MEDA-PROV-6.
                 25  :FSSC:-PROV-STATE-CD                PIC X(2).
                 25  :FSSC:-PROV-ID.
                   30  :FSSC:-PROV-TYP-FACIL-CD          PIC X(1).
                   30  FILLER                            PIC X(2).
                   30  :FSSC:-PROV-EMER-IND              PIC X(1).
                     88 :FSSC:-PROV6-E-EMERGENCY          VALUE 'E'.
                     88 :FSSC:-PROV6-F-FEDERAL            VALUE 'F'.
               20  :FSSC:-PROV-DEPT-ID                   PIC X(3).
               20  FILLER                                PIC X(4).
             15  :FSSC:-OTHER-SUMMARY-DATA.
               20  :FSSC:-UB04-FILLER-F1                 PIC X(1).
               20  :FSSC:-BILL-TYP-CD.
                 25  :FSSC:-BILL-CATEGORY.
                   30 :FSSC:-LOB-CD                      PIC 9(1).
                   30 :FSSC:-SERV-TYP-CD                 PIC 9(1).
FS7390               88 :FSSC:-SERV-TYP-1-INPAT-PRTA      VALUE 1.
                 25  :FSSC:-FREQ-CD                      PIC X(1).
                   88  :FSSC:-FREQ-0-REJECT               VALUE '0'.
FS7350             88  :FSSC:-FREQ-1-ORIGINAL             VALUE '1'.
FS7350             88  :FSSC:-FREQ-2-INTERM-START         VALUE '2'.
FS7350             88  :FSSC:-FREQ-3-INTERM-CONTIN        VALUE '3'.
FS7350             88  :FSSC:-FREQ-4-INTERM-END           VALUE '4'.
                   88  :FSSC:-FREQ-7-PROV-DEBIT           VALUE '7'.
                   88  :FSSC:-FREQ-8-PROV-CREDIT          VALUE '8'.
                   88  :FSSC:-FREQ-2-HH-RAP               VALUE '2'.
                   88  :FSSC:-FREQ-9-HH-FINAL             VALUE '9'.
FS7372             88  :FSSC:-FREQ-Z-TEMP-ENCOUNTER       VALUE 'Z'.
CR7873             88  :FSSC:-FREQ-FINAL-BILL             VALUE         CR7873R1
CR7873                                                    '1' '4' '5'   CR7873R1
CR7873                                                    '9'.          CR7873R1
CR7873             88  :FSSC:-FREQ-INTERIM-BILL           VALUE         CR7873R1
CR7873                                                    '2' '3'.      CR7873R1
                   88  :FSSC:-FREQ-NOTICE                 VALUE
                                                          'A' 'B' 'D'.
FS7372             88  :FSSC:-FREQ-HOSPICE-NOTICE         VALUE
FS7372                                                    'A' THRU 'E'.
                   88  :FSSC:-FREQ-A-NOTICE-ELECT         VALUE 'A'.
                   88  :FSSC:-FREQ-B-NOTICE-REVOKE        VALUE 'B'.
CR8562             88  :FSSC:-FREQ-C-HSPC-CHNG-PVDR       VALUE 'C'.
                   88  :FSSC:-FREQ-D-NOTICE-CANCEL        VALUE 'D'.
CR8562             88  :FSSC:-FREQ-E-HSPC-CHNG-OWNR       VALUE 'E'.
CR7873             88  :FSSC:-FREQ-CANCEL-BILL            VALUE         CR7873R1
CR7873                                                    '8' 'F' 'G'   CR7873R1
CR7873                                                    'H' 'I' 'J'   CR7873R1
CR7873                                                    'K' 'M'.      CR7873R1
CR9590             88  :FSSC:-FREQ-ADJUST                 VALUE
CR9590                                                     '7' 'F' 'G'
CR9590                                                     'H' 'I' 'J'
CR9590                                                     'K' 'M' 'P'
CR9590                                                     'Q'.
                   88  :FSSC:-FREQ-ADJUST-CANCEL          VALUE
                                                           '7' '8'
                                                           'F' 'G' 'H'
                                                           'I' 'J' 'K'
CR8581                                                     'M' 'P' 'Q'.
CR8581             88  :FSSC:-FREQ-PROV-DR-CR-ROPN        VALUE
CR8581                                                     '7' '8' 'Q'.
                   88  :FSSC:-FREQ-MAC-INIT-ADJ           VALUE
                                                           'F' 'G' 'H'
                                                           'I' 'J' 'K'
                                                           'M'.
C10231             88  :FSSC:-FREQ-MAC-ADJ-EXCL-RAC       VALUE
C10231                                                     'F' 'G' 'I'
C10231                                                     'J' 'K' 'M'
C10231                                                     'P'.
                   88  :FSSC:-FREQ-F-MAC-BENE             VALUE 'F'.
                   88  :FSSC:-FREQ-G-MAC-CWF              VALUE 'G'.
                   88  :FSSC:-FREQ-H-MAC-CMS              VALUE 'H'.
                   88  :FSSC:-FREQ-I-MAC-INTERNAL         VALUE 'I'.
                   88  :FSSC:-FREQ-J-MAC-OTHER            VALUE 'J'.
                   88  :FSSC:-FREQ-K-MAC-OIG              VALUE 'K'.
                   88  :FSSC:-FREQ-M-MAC-MSP              VALUE 'M'.
                   88  :FSSC:-FREQ-P-PRO-DEBIT            VALUE 'P'.
CR8581             88  :FSSC:-FREQ-Q-PROV-REOPEN          VALUE 'Q'.
C10231             88  :FSSC:-FREQ-ADJ-EXCL-RAC           VALUE
C10231                                                     '7' 'F' 'G'
C10231                                                     'I' 'J' 'K'
C10231                                                     'M' 'P' 'Q'.
               20  :FSSC:-UB04-FILLER-F2                 PIC X(1).
               20  :FSSC:-RECD-DT-CYMD.
                 25  :FSSC:-RECD-DT-CC                   PIC 9(2).
                 25  :FSSC:-RECD-DT.
                   30  :FSSC:-RECD-YR                    PIC 9(2).
                   30  :FSSC:-RECD-MO                    PIC 9(2).
                   30  :FSSC:-RECD-DY                    PIC 9(2).
               20  :FSSC:-CURR-TRAN-DT-CYMD.
                 25  :FSSC:-CURR-TRAN-DT-CC              PIC 9(2).
                 25  :FSSC:-CURR-TRAN-DT.
                   30  :FSSC:-CURR-TRAN-YR               PIC 9(2).
                   30  :FSSC:-CURR-TRAN-MO               PIC 9(2).
                   30  :FSSC:-CURR-TRAN-DY               PIC 9(2).
               20  :FSSC:-PAID-DT-CYMD.
                 25  :FSSC:-PAID-DT-CC                   PIC 9(2).
                 25  :FSSC:-PAID-DT.
                   30  :FSSC:-PAID-YR                    PIC 9(2).
                   30  :FSSC:-PAID-MO                    PIC 9(2).
                   30  :FSSC:-PAID-DY                    PIC 9(2).
               20  :FSSC:-ADM-DATE-CYMD.
                 25  :FSSC:-ADM-DATE-CC                  PIC 9(2).
                 25  :FSSC:-ADM-DATE.
                   30  :FSSC:-ADM-DATE-YY                PIC 9(2).
                   30  :FSSC:-ADM-DATE-MO                PIC 9(2).
                   30  :FSSC:-ADM-DATE-DD                PIC 9(2).
               20  :FSSC:-STMT-COV-FROM-DT-CYMD.
                 25  :FSSC:-STMT-COV-FROM-DT-CC          PIC 9(2).
                 25  :FSSC:-STMT-COV-FROM-DT.
                   30  :FSSC:-STMT-COV-FROM-YR           PIC 9(2).
                   30  :FSSC:-STMT-COV-FROM-MO           PIC 9(2).
                   30  :FSSC:-STMT-COV-FROM-DY           PIC 9(2).
               20  :FSSC:-STMT-COV-TO-DT-CYMD.
                 25  :FSSC:-STMT-COV-TO-DT-CC            PIC 9(2).
                 25  :FSSC:-STMT-COV-TO-DT.
                   30  :FSSC:-STMT-COV-TO-YR             PIC 9(2).
                   30  :FSSC:-STMT-COV-TO-MO             PIC 9(2).
                   30  :FSSC:-STMT-COV-TO-DY             PIC 9(2).
               20  :FSSC:-TOTALS.
                 25  :FSSC:-TOTAL-CHARGE-AMOUNT   COMP-3 PIC S9(9)V99.
               20  :FSSC:-TAPE-TO-TAPE-IND               PIC X(1).
                 88  :FSSC:-TTT-VALID           VALUE ' ' 'A' 'J'
                                                      'O' 'Q' 'S'
                                                      'T' 'U' 'W'
                                                      'X' 'Z'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-A      VALUE 'A'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-J      VALUE 'J'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-O      VALUE 'O'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-Q      VALUE 'Q'.
FS0103           88  :FSSC:-TAPE-TO-TAPE-R      VALUE 'R'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-S      VALUE 'S'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-T      VALUE 'T'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-U      VALUE 'U'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-W      VALUE 'W'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-X      VALUE 'X'.
FS0103           88  :FSSC:-TAPE-TO-TAPE-Y      VALUE 'Y'.
FS7350           88  :FSSC:-TAPE-TO-TAPE-Z      VALUE 'Z'.
                 88  :FSSC:-TTT-BYPASS-CWF      VALUE 'J' 'O' 'Q'
                                                      'X' 'Z'.
                 88  :FSSC:-TTT-BYPASS-REMIT    VALUE 'O' 'T' 'U'
                                                      'Z'.
                 88  :FSSC:-TTT-BYPASS-PSR      VALUE 'Q' 'S' 'T'
                                                      'U' 'Z'.
                 88  :FSSC:-TTT-BYPASS-WORKLOAD VALUE 'A' 'U' 'Z'.
FS0174           88  :FSSC:-TTT-CWF             VALUE ' ' 'A' 'S'
FS0174                                                'T' 'U' 'V'
FS0174                                                'W'.
FS0174           88  :FSSC:-TTT-NON-CWF         VALUE 'J' 'O' 'Q'
FS0174                                            'R' 'X' 'Y' 'Z'.
                 88  :FSSC:-TTT-BYPASS-DUP-EDIT VALUE 'X'.
FS7330           88  :FSSC:-TTT-BYPASS-HIGLAS-A VALUE 'A'.              FS7330U1
               20  :FSSC:-POST-PAY-IND                   PIC X(1).
                 88  :FSSC:-POST-PAY-NOT-POSTPAY          VALUE ' '.
                 88  :FSSC:-POST-PAY-C-COMPLETE           VALUE 'C'.
                 88  :FSSC:-POST-PAY-Y-ACTIVE             VALUE 'Y'.
               20  :FSSC:-CANCEL-DATE-CYMD.
                 25  :FSSC:-CANCEL-DATE-CYMD-CC          PIC 9(2).
                 25  :FSSC:-CANCEL-DATE.
                   30  :FSSC:-CANCEL-DATE-YY             PIC 9(2).
                   30  :FSSC:-CANCEL-DATE-MM             PIC 9(2).
                   30  :FSSC:-CANCEL-DATE-DD             PIC 9(2).
               20  :FSSC:-AHHSM-PASS-IND                 PIC X(1).
                 88  :FSSC:-AHHSM-N-REAS-NOT-SET         VALUE 'N' ' '.
                 88  :FSSC:-AHHSM-Y-REAS-SET             VALUE 'Y'.
               20  :FSSC:-CLEAN-IND                      PIC X(1).
      ****           'NON' IN 88 LEVELS BELOW MEANS NON-PIP
                 88  :FSSC:-CLEAN-A-PIP-OTHER             VALUE 'A'.
                 88  :FSSC:-CLEAN-B-PIP-CLEAN             VALUE 'B'.
                 88  :FSSC:-CLEAN-C-NON-OTHER             VALUE 'C'.
                 88  :FSSC:-CLEAN-D-NON-CLEAN             VALUE 'D'.
                 88  :FSSC:-CLEAN-E-NON-INFO-REQ          VALUE 'E'.
                 88  :FSSC:-CLEAN-F-PIP-INFO-REQ          VALUE 'F'.
                 88  :FSSC:-CLEAN-G-NON-DOD-RECD          VALUE 'G'.
                 88  :FSSC:-CLEAN-H-PIP-DOD-RECD          VALUE 'H'.
                 88  :FSSC:-CLEAN-I-NON-NON-DEF           VALUE 'I'.
                 88  :FSSC:-CLEAN-J-PIP-NON-DEF           VALUE 'J'.
                 88  :FSSC:-CLEAN-K-NON-DELAYED           VALUE 'K'.
                 88  :FSSC:-CLEAN-L-PIP-DELAYED           VALUE 'L'.
                 88  :FSSC:-CLEAN-M-NON-MANUAL            VALUE 'M'.
                 88  :FSSC:-CLEAN-N-PIP-MANUAL            VALUE 'N'.
                 88  :FSSC:-CLEAN-O-NON-PRI-PEND          VALUE 'O'.
                 88  :FSSC:-CLEAN-P-PIP-PRI-PEND          VALUE 'P'.
FS0159           88  :FSSC:-CLEAN-Q                       VALUE 'Q'.
FS0159           88  :FSSC:-CLEAN-R                       VALUE 'R'.
FS0159           88  :FSSC:-CLEAN-Y-NON-OTHER             VALUE 'Y'.
FS0159           88  :FSSC:-CLEAN-SPACE-NON-CLEAN         VALUE ' '.
               20  :FSSC:-PP-REASON-CODE                 PIC X(5).
               20  :FSSC:-USER-ACTION-CODE               PIC X(1).
                 88  :FSSC:-USER-1-RNHCI-EXCEPTED         VALUE '1'.
                 88  :FSSC:-USER-2-RNHCI-NON-EXCP         VALUE '2'.
                 88  :FSSC:-USER-5-SPEC-PROCESS           VALUE '5'.
                 88  :FSSC:-USER-7-RE-EDIT-5XXXX          VALUE '7'.
                 88  :FSSC:-USER-8-OCE-MR-SUSP            VALUE '8'.
                 88  :FSSC:-USER-9-1ST-CLM-REVIEW         VALUE '9'.
                 88  :FSSC:-USER-A-PAY-PER-WV-TEC         VALUE 'A'.
                 88  :FSSC:-USER-B-PAY-PER-WV-MED         VALUE 'B'.
                 88  :FSSC:-USER-D-BENE-LIAB-SUB          VALUE 'D'.
                 88  :FSSC:-USER-K-BENE-LIAB-NOT          VALUE 'K'.
                 88  :FSSC:-USER-C-PRV-LIAB-MED-S         VALUE 'C'.
                 88  :FSSC:-USER-G-PRV-LIAB-TEC-S         VALUE 'G'.
                 88  :FSSC:-USER-I-PRV-LIAB-MED-N         VALUE 'I'.
                 88  :FSSC:-USER-J-PRV-LIAB-TEC-N         VALUE 'J'.
                 88  :FSSC:-USER-E-PAY-FULL               VALUE 'E'.
                 88  :FSSC:-USER-F-PAY-PARTIAL            VALUE 'F'.
                 88  :FSSC:-USER-H-MULTIPLE-LIABS         VALUE 'H'.
                 88  :FSSC:-USER-L-PRV-LIAB-CD-CH         VALUE 'L'.
                 88  :FSSC:-USER-M-PAY-PER-W-PART         VALUE 'M'.
                 88  :FSSC:-USER-N-PRV-LIAB-PART          VALUE 'N'.
                 88  :FSSC:-USER-O-BENE-LIAB-PART         VALUE 'O'.
                 88  :FSSC:-USER-P-OPEN-CLS-BIOPS         VALUE 'P'.
                 88  :FSSC:-USER-Q-RLS-WITH-NO-MR         VALUE 'Q'.
                 88  :FSSC:-USER-R-CWF-DEN-BUT-MR         VALUE 'R'.
                 88  :FSSC:-USER-Z-REEDIT-5-7XXXX         VALUE 'Z'.
               20  :FSSC:-UNIFORM-BILL-CD                PIC X(1).
                 88  :FSSC:-UBC-0-UNKNOWN         VALUE  '0'.
                 88  :FSSC:-UBC-3-PRO-AUTO-ADJ    VALUE  '3'.
                 88  :FSSC:-UBC-EMC               VALUES '1' '2'
                                                         '4' THRU '7'.
                 88  :FSSC:-UBC-HARDCOPY          VALUES '8' '9'.
               20  :FSSC:-REJECT-CD                      PIC X(5).
               20  :FSSC:-ROUTING-UBC                    PIC X(1).
                 88  :FSSC:-ROUTING-UBC-HARD-COPY         VALUE
                                                           '0' '4' '8'.
               20  :FSSC:-PRIMARY-REASON                 PIC X(5).
               20  :FSSC:-CWF-RECYCLE-JUL-DT             PIC X(5).
               20  :FSSC:-PVDR-FINAL-SETTLEMENT   COMP-3 PIC S9(9)V99.
               20  :FSSC:-PRIMARY-PAYER-CODE             PIC X(1).
                 88  :FSSC:-PRI-PAY-1-MEDICAID            VALUE '1'.
                 88  :FSSC:-PRI-PAY-2-BLUE-CROSS          VALUE '2'.
                 88  :FSSC:-PRI-PAY-3-OTHER               VALUE '3'.
                 88  :FSSC:-PRI-PAY-4-NONE                VALUE '4'.
                 88  :FSSC:-PRI-PAY-A-WORKING-AGE         VALUE 'A'.
                 88  :FSSC:-PRI-PAY-B-ESRD-W-EGHP         VALUE 'B'.
                 88  :FSSC:-PRI-PAY-C-CONDITIONAL         VALUE 'C'.
                 88  :FSSC:-PRI-PAY-D-AUTO                VALUE 'D'.
                 88  :FSSC:-PRI-PAY-E-WORKER-COMP         VALUE 'E'.
                 88  :FSSC:-PRI-PAY-F-FED-AGENCY          VALUE 'F'.
                 88  :FSSC:-PRI-PAY-G-DISABLED            VALUE 'G'.
                 88  :FSSC:-PRI-PAY-H-BLACK-LUNG          VALUE 'H'.
                 88  :FSSC:-PRI-PAY-I-VA                  VALUE 'I'.
                 88  :FSSC:-PRI-PAY-L-LIABILITY           VALUE 'L'.
                 88  :FSSC:-PRI-PAY-Z-MEDICARE            VALUE 'Z'.
               20  :FSSC:-UB04-FILLER-F3                 PIC X(1).
               20  :FSSC:-SUBMITTED-TOB.
                 25  :FSSC:-SUBMITTED-CAT                PIC X(2).
                 25  :FSSC:-SUBMITTED-FREQ               PIC X(1).
               20  :FSSC:-UB04-FILLER-F4                 PIC X(1).
               20  :FSSC:-UB-82-92                       PIC X(1).
                 88  :FSSC:-UB-A-HARD-COPY                VALUE 'A'.
                 88  :FSSC:-UB-8-UB82                     VALUE '8'.
                 88  :FSSC:-UB-9-UB92                     VALUE '9'.
               20  :FSSC:-RECON-IND                      PIC X(1).
                 88  :FSSC:-BSVS-UNDER-RECON              VALUE 'Y'.
                 88  :FSSC:-RECON-A-FINAL-AFFIRM          VALUE 'A'.
                 88  :FSSC:-RECON-B-FINAL-NO-ADJ          VALUE 'B'.
                 88  :FSSC:-RECON-R-FINAL-REVERSE         VALUE 'R'.
                 88  :FSSC:-RECON-U-RECONSIDER            VALUE 'U'.
               20  :FSSC:-CHOICES-CLAIM                  PIC X(1).
                 88  :FSSC:-CHOICES-D-HH-DAYCARE          VALUE 'D'.
                 88  :FSSC:-CHOICES-H-HH-HOME             VALUE 'H'.
                 88  :FSSC:-CHOICES-L-LOW-VISION          VALUE 'L'.
                 88  :FSSC:-CHOICES-T-TRIAL-49            VALUE 'T'.
                 88  :FSSC:-CHOICES-V-VA                  VALUE 'V'.
                 88  :FSSC:-CHOICES-Y-CHOICES             VALUE 'Y'.
C10250           88  :FSSC:-CHOICES-DEMO-CLAIM       VALUES 'V' 'Y'.
               20  :FSSC:-FULL-PART-DEN-IND              PIC X(1).
               20  :FSSC:-ADMIT-DIAG-CODE.
                 25  :FSSC:-ADM-DIAG-CD                  PIC X(7).
               20  :FSSC:-PRINCIPLE-DIAG-CODE.
                 25  :FSSC:-PRINCIPLE-DIAG               PIC X(7).
                 25  :FSSC:-ICD10-FILLER-F2              PIC X(1).
               20  :FSSC:-NON-PAY-IND                    PIC X(2).
                 88  :FSSC:-NON-PAY-B-BEN-EXHAUST         VALUE 'B '.
                 88  :FSSC:-NON-PAY-C-NON-COVERED         VALUE 'C '.
                 88  :FSSC:-NON-PAY-E-1ST-CLM-DEV         VALUE 'E '.
                 88  :FSSC:-NON-PAY-F-TRAUMA-DEV          VALUE 'F '.
                 88  :FSSC:-NON-PAY-G-SECONDARY           VALUE 'G '.
                 88  :FSSC:-NON-PAY-H-SELF-REPORT         VALUE 'H '.
                 88  :FSSC:-NON-PAY-J-411-25              VALUE 'J '.
                 88  :FSSC:-NON-PAY-K-VOLUNTARY           VALUE 'K '.
                 88  :FSSC:-NON-PAY-N-ALL-OTHER           VALUE 'N '.
                 88  :FSSC:-NON-PAY-P-PAY-REQUEST         VALUE 'P '.
                 88  :FSSC:-NON-PAY-Q-MSP-AGREE           VALUE 'Q '.
                 88  :FSSC:-NON-PAY-R-REFUSED             VALUE 'R '.
                 88  :FSSC:-NON-PAY-T-MSP-ENROLL          VALUE 'T '.
                 88  :FSSC:-NON-PAY-U-HMO-RATE            VALUE 'U '.
                 88  :FSSC:-NON-PAY-V-LITIGATION          VALUE 'V '.
                 88  :FSSC:-NON-PAY-W-WORKER-COMP         VALUE 'W '.
                 88  :FSSC:-NON-PAY-X-COST-AVOID          VALUE 'X '.
                 88  :FSSC:-NON-PAY-Y-DATA-MATCH          VALUE 'Y '.
                 88  :FSSC:-NON-PAY-Z-CWF-ACCEPT          VALUE 'Z '.
                 88  :FSSC:-NON-PAY-00-COBC               VALUE '00'.
                 88  :FSSC:-NON-PAY-12-BC-AGREE           VALUE '12'.
                 88  :FSSC:-NON-PAY-13-OPM-MATCH          VALUE '13'.
                 88  :FSSC:-NON-PAY-14-WC-MATCH           VALUE '14'.
                 88  :FSSC:-NON-PAY-15-WC-AGREE           VALUE '15'.
                 88  :FSSC:-NON-PAY-16-LIAB-VDSA          VALUE '16'.
                 88  :FSSC:-NON-PAY-17-NO-FAULT           VALUE '17'.
                 88  :FSSC:-NON-PAY-18-PHAR-AGREE         VALUE '18'.
                 88  :FSSC:-NON-PAY-25-RAC-MSP-CA         VALUE '25'.
                 88  :FSSC:-NON-PAY-26-RAC-MSP-FL         VALUE '26'.
      ****         'UAC' BELOW IS 'USER ACTION CODE'
               20  :FSSC:-ORIGINAL-UAC.
                 25  :FSSC:-ORIG-UAC                     PIC X(1).
                 25  :FSSC:-ORIG-RECON-UAC               PIC X(1).
                   88  :FSSC:-ORIG-RECON-R-RECON          VALUE 'R'.
               20  :FSSC:-SUMM-SUPPRESS-IND              PIC X(1).
                 88  :FSSC:-SUMM-SUPP-Y-MAIL-SUPP         VALUE 'Y'.
                 88  :FSSC:-SUMM-SUPP-N-MAIL-NOT          VALUE 'N' ' '.
               20  :FSSC:-HH-SPLIT-IND                   PIC X(1).
                 88  :FSSC:-HH-SPLIT-F-FINAL              VALUE 'F'.
                 88  :FSSC:-HH-SPLIT-R-RAP                VALUE 'R'.
               20  :FSSC:-PHYS-REV-RECS           COMP-3 PIC S9(3).
               20  :FSSC:-LINES-TOTAL             COMP-3 PIC S9(3).
               20  :FSSC:-LINES                   COMP-3 PIC S9(3).
               20  :FSSC:-PROCESS-DT-CYMD.
                 25  :FSSC:-PROCESS-DT-CC                PIC 9(2).
                 25  :FSSC:-PROCESS-DT.
                   30  :FSSC:-PROCESS-YR                 PIC 9(2).
                   30  :FSSC:-PROCESS-MO                 PIC 9(2).
                   30  :FSSC:-PROCESS-DY                 PIC 9(2).
               20  :FSSC:-NPI-NUMBER                     PIC 9(10).
               20  :FSSC:-TRANSACT-TYPE                  PIC X(1).
                 88  :FSSC:-TRANSACT-C-CREDIT             VALUE 'C'.
                 88  :FSSC:-TRANSACT-D-DEBIT              VALUE 'D'.
                 88  :FSSC:-TRANSACT-DEFAULT              VALUE ' '.
               20  :FSSC:-SUPPRESS-VIEW                  PIC X(1).
               20  :FSSC:-DRG-CD                         PIC X(3).
               20  :FSSC:-ENTERED-FROM-DT-CYMD.
                 25  :FSSC:-ENTERED-FROM-DT-CC           PIC 9(2).
                 25  :FSSC:-ENTERED-FROM-DT.
                   30  :FSSC:-ENTERED-FROM-YR            PIC 9(2).
                   30  :FSSC:-ENTERED-FROM-MO            PIC 9(2).
                   30  :FSSC:-ENTERED-FROM-DY            PIC 9(2).
               20  :FSSC:-CALC-FROM-DT-CYMD.
                 25  :FSSC:-CALC-FROM-DT-CC              PIC 9(2).
                 25  :FSSC:-CALC-FROM-DT.
                   30  :FSSC:-CALC-FROM-YR               PIC 9(2).
                   30  :FSSC:-CALC-FROM-MO               PIC 9(2).
                   30  :FSSC:-CALC-FROM-DY               PIC 9(2).
               20  :FSSC:-NOA-CLAIM-MATCH                PIC X(1).
                 88  :FSSC:-NOA-CLAIM-MATCH-Y-YES         VALUE 'Y'.
                 88  :FSSC:-NOA-CLAIM-MATCH-N-NO          VALUE 'N'.
                 88  :FSSC:-NOA-CLAIM-MATCH-NA            VALUE ' '.
CR9630         20  :FSSC:-MBI                            PIC X(11).
CR9630         20  :FSSC:-SUMMARY-FUTURE                 PIC X(03).