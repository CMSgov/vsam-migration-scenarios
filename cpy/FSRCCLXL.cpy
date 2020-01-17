      ********************** | *****************************************
      ***                                                              *
      *** MEDICARE PART A                                              *
      *** FISS SYSTEM DOCUMENTATION                                    *
      ***                                                              *
      *|* MEMBER: FSRCCLXL                                             *
      ***                                                              *
      *** DESCRIPTION: CLAIM EXTRACT REVENUE LINE                      *
      ***                                                              *
      *** FIXED LRECL: 1,500 BYTES                                     *
      ***                                                              *
      ******************************************************************
      ***                                                              *
      *** ANY CHANGES MAY REQUIRE UPDATES TO SOME OF THE FOLLOWING:    *
      ***   `CLMB, `CLML, `CLMP, `CLMQ, `CLMR, `CLMS, `CLMX,           *
      ***   'CLMY, `IDRC, `PDCL, `PDCP, `PDCR, `259R, SFSPDCL.         *
      ***                                                              *
      *** THIS COPYBOOK IS USED IN OTHER COPYBOOKS.                    *
      ***                                                              *
      ***$************************************************************$
             15  :FSSC:-NON-BILL-REV-CODE                PIC X(1).
               88  :FSSC:-NON-BILL-E-ESRD-ATT             VALUE 'E'.
               88  :FSSC:-NON-BILL-H-HCPC-EMC             VALUE 'H'.
               88  :FSSC:-NON-BILL-I-HCPC-HC              VALUE 'I'.
               88  :FSSC:-NON-BILL-N-INV-REV              VALUE 'N'.
               88  :FSSC:-NON-BILL-R-RC636                VALUE 'R'.
               88  :FSSC:-NON-BILL-S-OXYGEN-EMC           VALUE 'S'.
               88  :FSSC:-NON-BILL-T-OXYGEN-HC            VALUE 'T'.
               88  :FSSC:-NON-BILL-Y-VALID-REV            VALUE 'Y'.
             15  :FSSC:-HCPC-DATA.
               20  :FSSC:-HCPC-CD-X.
                 25  :FSSC:-HCPC-CD                      PIC X(5).
               20  :FSSC:-HCPC-IND                       PIC X(1).
                 88  :FSSC:-HCPC-IND-R-RHHI               VALUE 'R'.
CR9501           88  :FSSC:-HCPC-IND-H-RURAL-DME          VALUE 'H'.
               20  :FSSC:-HCPC-MODIFIERS.
                 25  :FSSC:-HCPC-MODIFIER                PIC X(2).
                 25  :FSSC:-HCPC-MODIFIER2               PIC X(2).
                 25  :FSSC:-HCPC-MODIFIER3               PIC X(2).
                 25  :FSSC:-HCPC-MODIFIER4               PIC X(2).
                 25  :FSSC:-HCPC-MODIFIER5               PIC X(2).
               20  FILLER REDEFINES :FSSC:-HCPC-MODIFIERS.
                 25  :FSSC:-HCPC-MOD      OCCURS 5 TIMES PIC X(2).
             15  :FSSC:-SPEC-PROCESS-IND                 PIC X(1).
               88  :FSSC:-SPEC-PROCESS-DEFAULT            VALUE ' '.
               88  :FSSC:-SPEC-PROCESS-X-MANUAL           VALUE 'X'.
               88  :FSSC:-SPEC-PROCESS-Z-MANUAL           VALUE 'Z'.
               88  :FSSC:-SPEC-PROCESS-T-MD-WAV           VALUE 'T'.
               88  :FSSC:-SPEC-PROCESS-B-BUNDLE           VALUE 'B'.
             15  :FSSC:-ACT-MEDA-REIMB-LINE       COMP-3 PIC S9(9)V99.
             15  :FSSC:-CWF-OVR.
               20  :FSSC:-CWF-OVR-FLAG                   PIC X(1).
                 88  :FSSC:-CWF-OVR-1-HH-CONSO            VALUE '1'.
                 88  :FSSC:-CWF-OVR-2-SNF-CONSO           VALUE '2'.
                 88  :FSSC:-CWF-OVR-T-THERAPY             VALUE 'T'.
             15  :FSSC:-PC-TC-IND                        PIC X(1).
               88  :FSSC:-PC-TC-0-HPSA-PAY                 VALUE '0'.
               88  :FSSC:-PC-TC-1-HPSA-RETURN              VALUE '1'.
               88  :FSSC:-PC-TC-2-PROF-COMP                VALUE '2'.
               88  :FSSC:-PC-TC-3-TECH-COMP                VALUE '3'.
               88  :FSSC:-PC-TC-4-GLOBAL-NOPAY             VALUE '4'.
               88  :FSSC:-PC-TC-5-INCID-NOPAY              VALUE '5'.
               88  :FSSC:-PC-TC-6-LAB-PHYS-PAY             VALUE '6'.
               88  :FSSC:-PC-TC-7-PT-NOPAY                 VALUE '7'.
               88  :FSSC:-PC-TC-8-PHYSICIAN-PAY            VALUE '8'.
               88  :FSSC:-PC-TC-9-NOAPPLY-NOPAY            VALUE '9'.
             15  :FSSC:-ORIG-RTC-CODE                    PIC X(2).
             15  :FSSC:-ORIG-OCE-FLAG                    PIC X(2).
             15  :FSSC:-ASC-DATA.
               20  :FSSC:-ASC-PERCENT                    PIC X(1).
               20  :FSSC:-ASC-GRP                        PIC X(3).
                 88  :FSSC:-ASC-GRP-DEFAULT                VALUE '   '.
                 88  :FSSC:-ASC-GRP-O                      VALUE 'O  '.
                 88  :FSSC:-ASC-GRP-R                      VALUE 'R  '.
               20  :FSSC:-ASC-ADJ-LABOR           COMP-3 PIC S9(9)V99.
               20  :FSSC:-ASC-UNADJ-NONLABOR      COMP-3 PIC S9(9)V99.
             15  :FSSC:-DME-GENERIC-CODE                 PIC X(2).
               88  :FSSC:-DME-GENERIC-VALID                VALUES
                                                         'A ' THRU 'Z '.
             15  :FSSC:-DME-CATEGORY-CODE                PIC X(1).
               88  :FSSC:-DME-CAT-1-INEXPENSIVE            VALUE '1'.
               88  :FSSC:-DME-CAT-2-FREQ-MAINT             VALUE '2'.
               88  :FSSC:-DME-CAT-3-CUSTOMIZED             VALUE '3'.
               88  :FSSC:-DME-CAT-4-PROS-ORTHO             VALUE '4'.
               88  :FSSC:-DME-CAT-5-CAPPED-RENT            VALUE '5'.
               88  :FSSC:-DME-CAT-6-OXYGEN                 VALUE '6'.
             15  :FSSC:-PRICER-IND                       PIC X(1).
               88  :FSSC:-PRICER1-A-FEE-SCHED-N            VALUE 'A'.
               88  :FSSC:-PRICER1-B-FEE-SCHED              VALUE 'B'.
               88  :FSSC:-PRICER1-C-UNLISTED-RE            VALUE 'C'.
               88  :FSSC:-PRICER1-D-RADIOLOGY              VALUE 'D'.
               88  :FSSC:-PRICER1-E-ASC                    VALUE 'E'.
               88  :FSSC:-PRICER1-F-ESRD                   VALUE 'F'.
               88  :FSSC:-PRICER1-G-NOT-ON-HCPC            VALUE 'G'.
               88  :FSSC:-PRICER1-H-DME                    VALUE 'H'.
               88  :FSSC:-PRICER1-I-DME-MATCH              VALUE 'I'.
               88  :FSSC:-PRICER1-J-DME-RX-AUTO            VALUE 'J'.
               88  :FSSC:-PRICER1-K-DME-RX-PRES            VALUE 'K'.
               88  :FSSC:-PRICER1-L-TENS-GE-6MO            VALUE 'L'.
               88  :FSSC:-PRICER1-M-TENS-MR-5MO            VALUE 'M'.
               88  :FSSC:-PRICER1-N-NON-ESRD               VALUE 'N'.
               88  :FSSC:-PRICER1-Q-MANUAL                 VALUE 'Q'.
               88  :FSSC:-PRICER1-R-RADIOLOGY              VALUE 'R'.
               88  :FSSC:-PRICER1-S-FLU-PPV                VALUE 'S'.
               88  :FSSC:-PRICER1-T-NO-DED-COIN            VALUE 'T'.
               88  :FSSC:-PRICER1-U-AMBULANCE              VALUE 'U'.
JH0010         88  :FSSC:-PRICER1-V-HOSPICE                VALUE 'V'.
               88  :FSSC:-PRICER1-X-DRUG-MANUAL            VALUE 'X'.
               88  :FSSC:-PRICER1-Y-MANUAL-FEE             VALUE 'Y'.
               88  :FSSC:-PRICER1-Z-SUSP-MANUAL            VALUE 'Z'.
             15  :FSSC:-PRICER-IND2                      PIC X(1).
               88  :FSSC:-PRICER2-A-FEE-SCHED-N            VALUE 'A'.
               88  :FSSC:-PRICER2-B-FEE-SCHED              VALUE 'B'.
               88  :FSSC:-PRICER2-C-UNLISTED-RE            VALUE 'C'.
               88  :FSSC:-PRICER2-D-RADIOLOGY              VALUE 'D'.
               88  :FSSC:-PRICER2-E-ASC                    VALUE 'E'.
               88  :FSSC:-PRICER2-F-ESRD                   VALUE 'F'.
               88  :FSSC:-PRICER2-G-NOT-ON-HCPC            VALUE 'G'.
               88  :FSSC:-PRICER2-H-DME                    VALUE 'H'.
               88  :FSSC:-PRICER2-I-DME-MATCH              VALUE 'I'.
               88  :FSSC:-PRICER2-J-DME-RX-AUTO            VALUE 'J'.
               88  :FSSC:-PRICER2-K-DME-RX-PRES            VALUE 'K'.
               88  :FSSC:-PRICER2-L-TENS-GE-6MO            VALUE 'L'.
               88  :FSSC:-PRICER2-M-TENS-MR-5MO            VALUE 'M'.
               88  :FSSC:-PRICER2-N-NON-ESRD               VALUE 'N'.
               88  :FSSC:-PRICER2-Q-MANUAL                 VALUE 'Q'.
               88  :FSSC:-PRICER2-R-RADIOLOGY              VALUE 'R'.
               88  :FSSC:-PRICER2-S-FLU-PPV                VALUE 'S'.
               88  :FSSC:-PRICER2-T-NO-DED-COIN            VALUE 'T'.
               88  :FSSC:-PRICER2-U-AMBULANCE              VALUE 'U'.
JH0010         88  :FSSC:-PRICER2-V-HOSPICE                VALUE 'V'.
               88  :FSSC:-PRICER2-X-DRUG-MANUAL            VALUE 'X'.
               88  :FSSC:-PRICER2-Z-SUSP-MANUAL            VALUE 'Z'.
             15  :FSSC:-LINE-ITEM-OVR-FLAG               PIC X(1).
               88  :FSSC:-LINE-OVR-0-OCE-DENIAL            VALUE '0'.
               88  :FSSC:-LINE-OVR-1-OCE-IGNORE            VALUE '1'.
               88  :FSSC:-LINE-OVR-2-EXT-DENY              VALUE '2'.
               88  :FSSC:-LINE-OVR-3-EXT-REJECT            VALUE '3'.
               88  :FSSC:-LINE-OVR-4-EXT-ADJ               VALUE '4'.
             15  :FSSC:-OPPS-PRICR-LINE-RTC              PIC X(2). 
             15  :FSSC:-NCD-OVR-FLAG                     PIC X(1).
               88  :FSSC:-NCD-OVR-DEFAULT                  VALUE ' '.
               88  :FSSC:-NCD-OVR-Y-REVIEWED               VALUE 'Y'.
               88  :FSSC:-NCD-OVR-D-NO-COV-CHRG            VALUE 'D'.
             15  :FSSC:-NCD-DOC-FLAG                     PIC X(1).
               88  :FSSC:-NCD-DOC-Y-YES                    VALUE 'Y'.
               88  :FSSC:-NCD-DOC-N-NO                     VALUE 'N'.
             15  :FSSC:-NCD-RESP-CODE                    PIC X(1).
               88  :FSSC:-NCD-RESP-DEFAULT                 VALUE ' '.
               88  :FSSC:-NCD-RESP-0-EDIT-PASS             VALUE '0'.
               88  :FSSC:-NCD-RESP-1-EDIT-CONT             VALUE '1'.
               88  :FSSC:-NCD-RESP-2-SUSPEND-MR            VALUE '2'.
               88  :FSSC:-NCD-RESP-3-NON-COV               VALUE '3'.
               88  :FSSC:-NCD-RESP-4-NOT-NECESS            VALUE '4'.
               88  :FSSC:-NCD-RESP-5-FI-RTP                VALUE '5'.
             15  :FSSC:-PAY-METH-IND-LINE                PIC X(2).
             15  :FSSC:-HCPC-DRUG-CD                     PIC X(1).
FS7202         88  :FSSC:-HCPC-DRUG-E-YES                  VALUE 'E'.   FS7202U1
               88  :FSSC:-HCPC-DRUG-NO                     VALUE ' '.
             15  :FSSC:-EOMB-IND                         PIC X(1).
               88  :FSSC:-EOMB-A-MAX-AMT                   VALUE 'A'.
               88  :FSSC:-EOMB-B-RENT-MAX                  VALUE 'B'.
               88  :FSSC:-EOMB-C-PAID-MAX                  VALUE 'C'.
FS7202         88  :FSSC:-EOMB-D-REDUCED-RENT              VALUE 'D'.   FS7202U1
               88  :FSSC:-EOMB-E-RENT-TO-BUY               VALUE 'E'.
               88  :FSSC:-EOMB-F-LIABLE-AFTER              VALUE 'F'.
               88  :FSSC:-EOMB-G-NOT-NEEDED                VALUE 'G'.
               88  :FSSC:-EOMB-H-RENT-15MO                 VALUE 'H'.
               88  :FSSC:-EOMB-I-NEXT-TO-LAST              VALUE 'I'.
               88  :FSSC:-EOMB-J-LAST-RENT-15MO            VALUE 'J'.
               88  :FSSC:-EOMB-K-RENTED-TO-15MO            VALUE 'K'.
               88  :FSSC:-EOMB-L-NO-PAY-FOR-6MO            VALUE 'L'.
               88  :FSSC:-EOMB-R-PAY-HIGHER                VALUE 'R'.
               88  :FSSC:-EOMB-X-NO-PAY-ITEM               VALUE 'X'.
               88  :FSSC:-EOMB-Y-DENY-INCLUDED             VALUE 'Y'.
               88  :FSSC:-EOMB-Z-DENY-PORTABLE             VALUE 'Z'.
             15  :FSSC:-REV-FLAG                         PIC X(4).
             15  :FSSC:-LINE-ERROR                       PIC X(1).
             15  :FSSC:-HCPC-REGION                      PIC X(2).
             15  :FSSC:-OXYGEN-DATA.
               20  :FSSC:-OXYGEN-SYSTEM                  PIC X(1).
                 88  :FSSC:-OXYGEN-A-STATIONARY            VALUE 'A'.
                 88  :FSSC:-OXYGEN-B-STAT-CONT             VALUE 'B'.
                 88  :FSSC:-OXYGEN-C-PORTABLE              VALUE 'C'.
                 88  :FSSC:-OXYGEN-D-PORT-CONT             VALUE 'D'.
                 88  :FSSC:-OXYGEN-E-ACCESSORY             VALUE 'E'.
               20  :FSSC:-OXYGEN-TYPE                    PIC X(1).
             15  :FSSC:-MESSAGE-CODE                     PIC X(1).
               88  :FSSC:-MESSAGE-A-MAX-APPROVE              VALUE 'A'.
               88  :FSSC:-MESSAGE-B-MAX-PAYMENT              VALUE 'B'.
             15  :FSSC:-IDE-TYPE-CD                      PIC X(2).
             15  :FSSC:-IDE-NUMBER                       PIC X(15).
             15  :FSSC:-NCD-NUM                          PIC X(8).
             15  :FSSC:-REV-CD-DATA.
               20  :FSSC:-REV-CD                         PIC 9(4).
               20  :FSSC:-REV-CD-X              REDEFINES
                   :FSSC:-REV-CD.
                 25  :FSSC:-REV-CENTER                   PIC 9(3).
                 25  :FSSC:-REV-UNIT                     PIC 9(1).
             15  :FSSC:-REV-UNITS-BILLED          COMP-3 PIC S9(9).
             15  :FSSC:-REV-UNITS-BILLED-F        REDEFINES
                 :FSSC:-REV-UNITS-BILLED          COMP-3 PIC S9(08)V9.
             15  :FSSC:-REV-SERV-UNIT-CNT         COMP-3 PIC S9(9).
             15  :FSSC:-REV-SERV-UNIT-CNT-F       REDEFINES
                 :FSSC:-REV-SERV-UNIT-CNT         COMP-3 PIC S9(08)V9.
             15  :FSSC:-SERV-DT-CYMD.
               20  :FSSC:-SERV-DT-CC                     PIC 9(2).
               20  :FSSC:-SERV-DT.
                 25  :FSSC:-SERV-FROM-YR                 PIC 9(2).
                 25  :FSSC:-SERV-FROM-MO                 PIC 9(2).
                 25  :FSSC:-SERV-FROM-DY                 PIC 9(2).
             15  :FSSC:-OVERIDE-CD-R.
                 88  :FSSC:-OVERIDE-M-MSP-EGHP             VALUE 'M'.
                 88  :FSSC:-OVERIDE-N-MSP-NONEGHP          VALUE 'N'.
                 88  :FSSC:-OVERIDE-Y-MSP-COST-AV          VALUE 'Y'.
               20  :FSSC:-OVERIDE-CD                     PIC 9(1).
                 88  :FSSC:-OVERIDE-0-DEDUCT-COIN          VALUE 0.
                 88  :FSSC:-OVERIDE-1-COIN-NO-DED          VALUE 1.
                 88  :FSSC:-OVERIDE-2-DED-NO-COIN          VALUE 2.
                 88  :FSSC:-OVERIDE-3-NEITHER              VALUE 3.
                 88  :FSSC:-OVERIDE-4-NO-TOT-CHRG          VALUE 4.
                 88  :FSSC:-OVERIDE-5-PSYC                 VALUE 5.
             15  :FSSC:-MULTI-CHAN-TEST-QTY-X.
               20  :FSSC:-MULTI-CHAN-TEST-QTY            PIC 9(2).
             15  :FSSC:-UB04-FILLER-44            COMP-3 PIC 9(2).
             15  :FSSC:-REV-SERV-RATE-X.
               20  :FSSC:-REV-SERV-RATE          COMP-3 PIC S9(9)V999.
             15  :FSSC:-RAD-PRICER-AMT-X.
               20  :FSSC:-RAD-PRICER-AMT          COMP-3 PIC S9(9)V99.
             15  :FSSC:-REV-TOT-CHRG-AMT          COMP-3 PIC S9(9)V99.
             15  :FSSC:-REV-COV-CHRG-AMT          COMP-3 PIC S9(9)V99.
             15  :FSSC:-REV-NCOV-CHRG-AMT         COMP-3 PIC S9(9)V99.
             15  :FSSC:-TOT-FEE-SCHEDULE-AMT      COMP-3 PIC S9(9)V99.
             15  :FSSC:-WAGE-ADJ-COIN-LINE        COMP-3 PIC S9(8)V99.
             15  :FSSC:-REDUCED-COIN-LINE         COMP-3 PIC S9(8)V99.
             15  :FSSC:-PROV-REIMB-LINE           COMP-3 PIC S9(9)V99.
             15  :FSSC:-PAT-CASH-DED-LINE         COMP-3 PIC S9(8)V99.
             15  :FSSC:-PAT-REIMB-LINE            COMP-3 PIC S9(9)V99.
             15  :FSSC:-TOT-CONTR-ADJ             COMP-3 PIC S9(9)V99.
             15  :FSSC:-PSY-ESRD-BLD-HEMO         COMP-3 PIC S9(9)V99.
             15  :FSSC:-OTHER1-AMT                COMP-3 PIC S9(9)V99.
             15  :FSSC:-ANSI-LINE-INFO.
               20  :FSSC:-REDUC-COIN-ANSI.
                 25 :FSSC:-REDUC-COIN-ANSI-GRP           PIC X(2).
                 25 :FSSC:-REDUC-COIN-ANSI-RSN           PIC X(3).
               20  :FSSC:-CONTR-ANSI-INFO.
                 25  :FSSC:-CONTR-ANSI-GROUPS.
                   30  :FSSC:-CONTR-ANSI-GRP             PIC X(2).
                   30  :FSSC:-CONTR-ANSI-RSN             PIC X(3).
               20  :FSSC:-PSY-ESRD-BLD-HEMO-CDS.
                 25  :FSSC:-PSY-ESRD-BLD-HEMO-GRP        PIC X(2).
                 25  :FSSC:-PSY-ESRD-BLD-HEMO-RSN        PIC X(3).
               20  :FSSC:-OTHER1-ANSI.
                 25  :FSSC:-OTHER1-GRP                   PIC X(2).
                 25  :FSSC:-OTHER1-RSN                   PIC X(3).
               20  :FSSC:-BENE-REIMB-ANSI.
                 25  :FSSC:-BENE-REIMB-GRP               PIC X(2).
                 25  :FSSC:-BENE-REIMB-RSN               PIC X(3).
             15  :FSSC:-MSP-FIELDS.
               20  :FSSC:-MSP-CASH-DED-LINE       COMP-3 PIC S9(7)V99.
               20  :FSSC:-MSP-CASH-DED-ANSI.
                 25  :FSSC:-MSP-CASH-DED-GRP             PIC X(2).
                 25  :FSSC:-MSP-CASH-DED-RSN             PIC X(3).
               20  :FSSC:-MSP-COINS-LINE          COMP-3 PIC S9(7)V99.
               20  :FSSC:-MSP-COIN-ANSI.
                 25  :FSSC:-MSP-COIN-GRP                 PIC X(2).
                 25  :FSSC:-MSP-COIN-RSN                 PIC X(3).
               20  :FSSC:-MSP-BLD-DED-LINE        COMP-3 PIC S9(7)V99.
               20  :FSSC:-MSP-BLD-DED-ANSI.
                 25  :FSSC:-MSP-BLD-DED-GRP              PIC X(2).
                 25  :FSSC:-MSP-BLD-DED-RSN              PIC X(3).
               20  :FSSC:-MSP-AMT1-LINE           COMP-3 PIC S9(9)V99.
               20  :FSSC:-MSP-AMT2-LINE           COMP-3 PIC S9(9)V99.
               20  :FSSC:-MSP-OTAF-AMT            COMP-3 PIC S9(9)V99.
               20  :FSSC:-MSP-DENIAL-IND                 PIC X(1).
                 88  :FSSC:-MSP-DENIAL-NO                  VALUE ' '.
                 88  :FSSC:-MSP-DENIAL-D-DENIED            VALUE 'D'.
               20  :FSSC:-OUTLIER-AMT             COMP-3 PIC S9(9)V99.
             15  :FSSC:-MSP-FUTURE                       PIC X(11).
             15  :FSSC:-FMR-REASON-CODES            OCCURS 4 TIMES.
                 20  :FSSC:-FMR-REASON                   PIC X(5).
             15  :FSSC:-ADR-REASONS                 OCCURS 4 TIMES.
                 20  :FSSC:-ADR-REASON                   PIC X(5).
             15  :FSSC:-LINE-REASON-CODES           OCCURS 4 TIMES.
                 20  :FSSC:-LINE-REASON                  PIC X(5).
             15  :FSSC:-HCPC-ROLLUP-PANEL-CD             PIC X(5).
             15  :FSSC:-HCPC-ROLLUP-PMT-IND              PIC X(1).
               88  :FSSC:-HCPC-ROLLUP-DEFAULT              VALUE ' '.
               88  :FSSC:-HCPC-ROLLUP-D-DUP                VALUE 'D'.
               88  :FSSC:-HCPC-ROLLUP-I-INDIV              VALUE 'I'.
               88  :FSSC:-HCPC-ROLLUP-P-PANEL              VALUE 'P'.
               88  :FSSC:-HCPC-ROLLUP-R-REDUCED            VALUE 'R'.
             15  :FSSC:-FORM-LOC49                       PIC X(4).
             15  :FSSC:-HCPC-DUPE-IND                    PIC X(1).
               88  :FSSC:-HCPC-DUPE-DEFAULT                VALUE ' '.
               88  :FSSC:-HCPC-DUPE-F-FREE-ESRD            VALUE 'F'.
               88  :FSSC:-HCPC-DUPE-H-HOSP-ESRD            VALUE 'H'.
               88  :FSSC:-HCPC-DUPE-Y-HCPC-REQ             VALUE 'Y'.
             15  :FSSC:-OCE-APC-BUFFER.
               20  :FSSC:-APC-HCPCS-PROC                 PIC X(5).
               20  :FSSC:-APC-PAYMENT-APC                PIC 9(5).
               20  :FSSC:-APC-HCPCS-APC                  PIC 9(5).
               20  :FSSC:-HIPPS-APC-LINE        REDEFINES
                   :FSSC:-APC-HCPCS-APC                  PIC X(5).
               20  :FSSC:-APC-SERV-IND                   PIC X(2).
               20  :FSSC:-SITE-OF-SERV-INC-FLAG REDEFINES
                   :FSSC:-APC-SERV-IND                   PIC X(2).
FS8591           88  :FSSC:-SERV-A-NOT-PAID-OPPS           VALUE ' A'.
FS8591           88  :FSSC:-SERV-B-OPPS-NON-ALLOW          VALUE ' B'.
FS8591           88  :FSSC:-SERV-C-INPAT-PROCEDUR          VALUE ' C'.
CR9736           88  :FSSC:-SERV-D-DISCONT-CODES           VALUE ' D'.
FS8591           88  :FSSC:-SERV-E-NON-ALLOWED             VALUE ' E'.
FS8591           88  :FSSC:-SERV-F-CORN-CRNA-HEPB          VALUE ' F'.
FS8591           88  :FSSC:-SERV-G-DRUG-PASS-THRU          VALUE ' G'.
FS8591           88  :FSSC:-SERV-H-DEVICE-PASS-TH          VALUE ' H'.
FS8591           88  :FSSC:-SERV-I-IRF-DIFFERENT           VALUE ' I'.
FS8591           88  :FSSC:-SERV-J-NEW-DRUG                VALUE ' J'.
FS0374           88  :FSSC:-SERV-J1-PCKG-PAY-PRIM          VALUE 'J1'.
FS0374           88  :FSSC:-SERV-J2-COMBINE-ADJNC          VALUE 'J2'.
FS8591           88  :FSSC:-SERV-K-NON-PASS-THRU           VALUE ' K'.
FS8591           88  :FSSC:-SERV-L-FLU-PPV-VAC             VALUE ' L'.
FS8591           88  :FSSC:-SERV-M-SRV-NOT-BILABL          VALUE ' M'.
FS8591           88  :FSSC:-SERV-M-HH-MR-HIPPS             VALUE ' M'.
FS8591           88  :FSSC:-SERV-N-PACKAGD-IN-APC          VALUE ' N'.
FS8591           88  :FSSC:-SERV-P-PARTIAL-HOSPTL          VALUE ' P'.
FS8591           88  :FSSC:-SERV-P-HH-LT-10-THER           VALUE ' P'.
FS8591           88  :FSSC:-SERV-Q-PKGD-SEPR-PAY           VALUE ' Q'.
                 88  :FSSC:-SERV-Q1-STVX-PKG-CDS           VALUE 'Q1'.
                 88  :FSSC:-SERV-Q2-T-PKG-CDS              VALUE 'Q2'.
                 88  :FSSC:-SERV-Q3-PD-COMP-APC            VALUE 'Q3'.
FS8591           88  :FSSC:-SERV-R-BLOOD-PRODCTS           VALUE ' R'.
FS8591           88  :FSSC:-SERV-S-NO-MULTI-PROC           VALUE ' S'.
FS8591           88  :FSSC:-SERV-T-MULTI-PROC              VALUE ' T'.
FS8591           88  :FSSC:-SERV-U-BRACHYTHERAPY           VALUE ' U'.
FS8591           88  :FSSC:-SERV-V-CLINIC-EMERG            VALUE ' V'.
FS8591           88  :FSSC:-SERV-W-INV-HCPC-REV            VALUE ' W'.
FS8591           88  :FSSC:-SERV-X-ANCILLARY               VALUE ' X'.
FS8591           88  :FSSC:-SERV-Y-NO-IMPANT-DME           VALUE ' Y'.
FS8591           88  :FSSC:-SERV-Z-NO-HCPC-OR-SI           VALUE ' Z'.
CR9601           88  :FSSC:-SERV-STVPJ12
CR9601                           VALUES ' S' ' T' ' V' ' P' 'J1' 'J2'.
               20  :FSSC:-APC-PAYMENT-IND                PIC X(2).
FS0236           88  :FSSC:-APC-PAY-1-PAID-OPPS            VALUE ' 1'.
FS0236           88  :FSSC:-APC-PAY-2-NOT-OPPS             VALUE ' 2'.
FS0236           88  :FSSC:-APC-PAY-3-NOT-PAID             VALUE ' 3'.
FS0236           88  :FSSC:-APC-PAY-4-REAS-COST            VALUE ' 4'.
FS0236           88  :FSSC:-APC-PAY-5-ADD-DRUG-BI          VALUE ' 5'.
FS0236           88  :FSSC:-APC-PAY-6-ADD-DEVICE           VALUE ' 6'.
FS0236           88  :FSSC:-APC-PAY-7-NEW-DRUG             VALUE ' 7'.
FS0236           88  :FSSC:-APC-PAY-8-PER-DIEM             VALUE ' 8'.
FS0236           88  :FSSC:-APC-PAY-9-NO-ADD-PAY           VALUE ' 9'.
CR8743           88  :FSSC:-APC-PAY-10-FQHC-PAID           VALUE '10'.
CR8743           88  :FSSC:-APC-PAY-11-FQHC-NOPAY          VALUE '11'.
CR8743           88  :FSSC:-APC-PAY-12-FQHC-NOADD          VALUE '12'.
CR8743           88  :FSSC:-APC-PAY-13-FQHC-NEWPT          VALUE '13'.
               20  :FSSC:-OCE-APC-FILLER-F1              PIC X(1).
               20  :FSSC:-APC-DISC-FCTR                  PIC X(1).
                 88  :FSSC:-APC-DISC-FORMULA-1             VALUE '1'.
                 88  :FSSC:-APC-DISC-FORMULA-2             VALUE '2'.
                 88  :FSSC:-APC-DISC-FORMULA-3             VALUE '3'.
                 88  :FSSC:-APC-DISC-FORMULA-4             VALUE '4'.
                 88  :FSSC:-APC-DISC-FORMULA-5             VALUE '5'.
                 88  :FSSC:-APC-DISC-FORMULA-6             VALUE '6'.
                 88  :FSSC:-APC-DISC-FORMULA-7             VALUE '7'.
                 88  :FSSC:-APC-DISC-FORMULA-8             VALUE '8'.
                 88  :FSSC:-APC-DISC-FORMULA-9             VALUE '9'.
               20  :FSSC:-DISCOUNT-FLAG-LINE    REDEFINES
                   :FSSC:-APC-DISC-FCTR                  PIC X(1).
               20  :FSSC:-OCE-APC-FILLER-F2              PIC X(1).
               20  :FSSC:-APC-DEN-REJ                    PIC X(1).
                 88  :FSSC:-APC-DEN-0-NOT-REJ              VALUE '0'.
                 88  :FSSC:-APC-DEN-1-REJECT               VALUE '1'.
                 88  :FSSC:-APC-DEN-2-DAY-REJ              VALUE '2'.
CR9880           88  :FSSC:-APC-DEN-3-DAY-REJ              VALUE '3'.
                 88  :FSSC:-APC-DEN-DEFAULT                VALUE ' '.
               20  :FSSC:-OCE-APC-FILLER-F3              PIC X(1).
               20  :FSSC:-APC-PKG-FLAG                   PIC X(1).
                 88  :FSSC:-APC-PKG-0-NOT-PACKAGD          VALUE '0'.
                 88  :FSSC:-APC-PKG-1-SERVICE              VALUE '1'.
                 88  :FSSC:-APC-PKG-2-PER-DIEM             VALUE '2'.
                 88  :FSSC:-APC-PKG-3-ARTF-SURGRY          VALUE '3'.
                 88  :FSSC:-APC-PKG-4-DRUG-ADMIN           VALUE '4'.
CR8743           88  :FSSC:-APC-PKG-5-FQHC-DIEM            VALUE '5'.
CR8743           88  :FSSC:-APC-PKG-6-FQHC-NOCOIN          VALUE '6'.
               20  :FSSC:-PACKAGE-FLAG-LINE     REDEFINES
                   :FSSC:-APC-PKG-FLAG                   PIC X(1).
               20  :FSSC:-APC-PAY-ADJ-FLAG               PIC X(2).
      ****         FLAG 6 - PAYMENT ADJUSTMENT
                 88  :FSSC:-OCE6-0-NO-ADJ                  VALUE '0 '.
                 88  :FSSC:-OCE6-1-ADD-DRUG-BIO            VALUE '1 '.
                 88  :FSSC:-OCE6-2-ADD-DEVICE              VALUE '2 '.
                 88  :FSSC:-OCE6-3-ADD-NEW-DRUG            VALUE '3 '.
                 88  :FSSC:-OCE6-4-DEDUC-NOT-APLC          VALUE '4 '.
                 88  :FSSC:-OCE6-5-BLOOD-SUB-DED           VALUE '5 '.
                 88  :FSSC:-OCE6-6-BLOOD-NOT-SUB           VALUE '6 '.
                 88  :FSSC:-OCE6-7-NO-COST-PVDR            VALUE '7 '.
                 88  :FSSC:-OCE6-8-PART-CRED-PVDR          VALUE '8 '.
                 88  :FSSC:-OCE6-9-DED-CO-NOT-APL          VALUE '9 '.
                 88  :FSSC:-OCE6-10-COINS-NOT-APL          VALUE '10'.
                 88  :FSSC:-OCE6-91-99-COMPOSITE           VALUE '91'
                                                            THRU '99'.
               20  :FSSC:-OCE-APC-FILLER-F4              PIC X(1).
               20  :FSSC:-APC-TOB-INCL                   PIC X(1).
      ****         FLAG 7 - PAYMENT METHOD
                 88  :FSSC:-OCE7-0-OPPS-PRICER             VALUE '0'.
                 88  :FSSC:-OCE7-1-OPPS-NOPAY              VALUE '1'.
                 88  :FSSC:-OCE7-2-SERV-NOT-OPPS           VALUE '2'.
                 88  :FSSC:-OCE7-3-OCE-LINE-REJ            VALUE '3'.
                 88  :FSSC:-OCE7-4-MAC-LINE-REJ            VALUE '4'.
CR8743           88  :FSSC:-OCE7-5-FQHC-PAYMENT            VALUE '5'.
                 88  :FSSC:-APC-TOB-INCL-DEFAULT           VALUE ' '.
               20  :FSSC:-APC-SERV-UNIT                  PIC X(7).
               20  :FSSC:-APC-CHARGES             COMP-3 PIC S9(9)V99.
               20  :FSSC:-OCE-APC-FILLER-F5              PIC X(1).
               20  :FSSC:-APC-ACTION-FLAG                PIC X(1).
      ****       FLAG 8 - LINE ITEM ACTION
                 88  :FSSC:-OCE8-0-OCE-REJECT              VALUE '0'.
                 88  :FSSC:-OCE8-1-OCE-REJ-IGNORE          VALUE '1'.
                 88  :FSSC:-OCE8-2-EXTRNL-LIN-DEN          VALUE '2'.
                 88  :FSSC:-OCE8-3-EXTRNL-LIN-REJ          VALUE '3'.
                 88  :FSSC:-OCE8-4-EXTRNL-LIN-ADJ          VALUE '4'.
CR8743           88  :FSSC:-OCE8-5-FQHC-NON-COV            VALUE '5'.
               20  :FSSC:-COMPOSITE-ADJ-FLAG             PIC X(2).
                 88  :FSSC:-COMPOSITE-ADJ-00-NONE          VALUE '00'.
                 88  :FSSC:-COMPOSITE-ADJ-01-1ST           VALUE '01'.
                 88  :FSSC:-COMPOSITE-ADJ-02-2ND           VALUE '02'.
                 88  :FSSC:-COMPOSITE-ADJ-03-3RD           VALUE '03'.
                 88  :FSSC:-COMPOSITE-ADJ-04-4TH           VALUE '04'.
                 88  :FSSC:-COMPOSITE-ADJ-05-5TH           VALUE '05'.
CR8743           88  :FSSC:-COMP-ADJ-FQHC-MEDICAL          VALUE '01'.
CR8743           88  :FSSC:-COMP-ADJ-FQHC-MENTAL           VALUE '02'.
CR8743           88  :FSSC:-COMP-ADJ-FQHC-SUBSQNT          VALUE '03'.
             15  :FSSC:-OCE-APC-FILLER-F6                PIC X(16).
             15  :FSSC:-ORIG-HCPC-DATA.
               20  :FSSC:-ORIG-HCPC-CD                   PIC X(5).
               20  :FSSC:-ORIG-HCPC-IND                  PIC X(1).
                 88  :FSSC:-ORIG-HCPC-R-RHHI               VALUE 'R'.
                 88  :FSSC:-ORIG-HCPC-DEFAULT              VALUE ' '.
               20  :FSSC:-ORIG-HCPC-MODS.
                 25  :FSSC:-ORIG-HCPC-MOD1               PIC X(2).
                 25  :FSSC:-ORIG-HCPC-MOD2               PIC X(2).
                 25  :FSSC:-ORIG-HCPC-MOD3               PIC X(2).
                 25  :FSSC:-ORIG-HCPC-MOD4               PIC X(2).
                 25  :FSSC:-ORIG-HCPC-MOD5               PIC X(2).
               20  FILLER REDEFINES :FSSC:-ORIG-HCPC-MODS.
                 25  :FSSC:-ORIG-HCPC-MOD OCCURS 5 TIMES PIC X(2).
             15  :FSSC:-HCPC-MOD-IND                     PIC X(1).
               88  :FSSC:-HCPC-MOD-DEFAULT               VALUE ' '.
               88  :FSSC:-HCPC-MOD-U-UPCODING            VALUE 'U'.
               88  :FSSC:-HCPC-MOD-D-DOWNCODING          VALUE 'D'.
             15  :FSSC:-ORIG-REV-CD                      PIC 9(4).
               88  :FSSC:-ORIG-REV-001                   VALUE 001.
               88  :FSSC:-ORIG-REV-100                   VALUE 100.
               88  :FSSC:-ORIG-REV-101                   VALUE 101.
             15  :FSSC:-REASON-CD-BYPASS                 PIC X(1).
               88  :FSSC:-REASON-CD-BYP-Y-YES             VALUE 'Y'.
               88  :FSSC:-REASON-CD-BYP-N-NO              VALUE 'N'.
             15  :FSSC:-ANES-CONV-FACTOR                 PIC 99V99.
             15  :FSSC:-ANES-BASE-UNITS                  PIC 999.
               88  :FSSC:-ANES-BASE-VALID                VALUES 001
                                                           THRU 199.
             15  :FSSC:-ORIGINAL-LUAC             OCCURS 4 TIMES.
               20  :FSSC:-ORIG-LUAC                      PIC X(1).
             15  :FSSC:-BSVS-LINES                OCCURS 4 TIMES.
               20  :FSSC:-BSVS-DEN-OVERRIDE              PIC X.
                 88  :FSSC:-BSVS-DEN-D-DENIED              VALUE 'D'.
                 88  :FSSC:-BSVS-DEN-R-REJECT              VALUE 'R'.
                 88  :FSSC:-BSVS-DEN-DEFAULT             VALUE ' '.
               20  :FSSC:-BSVS-NCOV-CHRGS         COMP-3 PIC S9(9)V99.
               20  :FSSC:-BSVS-NCOV-DYS-VSTS      COMP-3 PIC S9(9).
               20  :FSSC:-BSVS-NCOV-DYS-VSTS-F    REDEFINES
                   :FSSC:-BSVS-NCOV-DYS-VSTS      COMP-3 PIC S9(8)V9.
               20  :FSSC:-BSVS-USER-ACT                  PIC X.
                 88  :FSSC:-BSVS-USER-1-RNHCI-EXC        VALUE '1'.
                 88  :FSSC:-BSVS-USER-2-RNHCI-NEX        VALUE '2'.
                 88  :FSSC:-BSVS-USER-A-TECH-WAV         VALUE 'A'.
                 88  :FSSC:-BSVS-USER-B-MED-WAV          VALUE 'B'.
                 88  :FSSC:-BSVS-USER-C-PROV-LIAB        VALUE 'C'.
                 88  :FSSC:-BSVS-USER-D-BENE-LIAB        VALUE 'D'.
                 88  :FSSC:-BSVS-USER-E-PAY-FULL         VALUE 'E'.
                 88  :FSSC:-BSVS-USER-F-PAY-PART         VALUE 'F'.
                 88  :FSSC:-BSVS-USER-G-PROV-TECH        VALUE 'G'.
                 88  :FSSC:-BSVS-USER-H-MULT-LIAB        VALUE 'H'.
                 88  :FSSC:-BSVS-USER-I-FULL-MED         VALUE 'I'.
                 88  :FSSC:-BSVS-USER-J-FULL-TECH        VALUE 'J'.
                 88  :FSSC:-BSVS-USER-K-FULL-BENE        VALUE 'K'.
                 88  :FSSC:-BSVS-USER-L-FULL-ACT         VALUE 'L'.
                 88  :FSSC:-BSVS-USER-M-PAY-WAV          VALUE 'M'.
                 88  :FSSC:-BSVS-USER-N-PROV-LIAB        VALUE 'N'.
                 88  :FSSC:-BSVS-USER-O-BENE-LIAB        VALUE 'O'.
                 88  :FSSC:-BSVS-USER-P-BIOPSY-CL        VALUE 'P'.
                 88  :FSSC:-BSVS-USER-Q-RELEASE          VALUE 'Q'.
                 88  :FSSC:-BSVS-USER-R-CWF-DENY         VALUE 'R'.
                 88  :FSSC:-BSVS-USER-Z-FORCE-MR         VALUE 'Z'.
                 88  :FSSC:-BSVS-USER-5-SPEC-PROC        VALUE '5'.
                 88  :FSSC:-BSVS-USER-7-MR-EDIT-5        VALUE '7'.
                 88  :FSSC:-BSVS-USER-8-OCE-MR           VALUE '8'.
                 88  :FSSC:-BSVS-USER-9-1ST-REV          VALUE '9'.
               20  :FSSC:-BSVS-MED-TECH-IND              PIC X.
                 88  :FSSC:-BSVS-MT-A-HH-NO-INTER        VALUE 'A'.
                 88  :FSSC:-BSVS-MT-B-HH-NO-HOME         VALUE 'B'.
                 88  :FSSC:-BSVS-MT-C-HH-NO-PHYS         VALUE 'C'.
                 88  :FSSC:-BSVS-MT-D-HH-NO-REC          VALUE 'D'.
                 88  :FSSC:-BSVS-MT-E-TECH-ERROR         VALUE 'E'.
                 88  :FSSC:-BSVS-MT-M-MED-DENIAL         VALUE 'M'.
                 88  :FSSC:-BSVS-MT-S-MED-DEN-NO         VALUE 'S'.
                 88  :FSSC:-BSVS-MT-T-TECH-DENIAL        VALUE 'T'.
                 88  :FSSC:-BSVS-MT-U-TECH-DEN-NO        VALUE 'U'.
               20  :FSSC:-BSVS-RECON                     PIC X.
                 88  :FSSC:-BSVS-RECON-DEFAULT           VALUE ' '.
                 88  :FSSC:-BSVS-RECON-R                 VALUE 'R'.
                 88  :FSSC:-BSVS-RECON-P                 VALUE 'P'.
               20  :FSSC:-BSVS-ANSI-INFO.
                 25  :FSSC:-BSVS-ANSI-GROUPS.
                   30  :FSSC:-BSVS-ANSI-GRP              PIC X(2).
                   30  :FSSC:-BSVS-ANSI-RSN              PIC X(3).
                 25  :FSSC:-BSVS-ANSI-REMARKS     OCCURS 4 TIMES.
                   30  :FSSC:-BSVS-ANSI-RMKS             PIC X(5).
               20  :FSSC:-BSVS-REASON                    PIC X(5).
               20  :FSSC:-BSVS-CHRGS-OVR-CD              PIC X(1).
FS7202           88  :FSSC:-BSVS-CHRGS-OVR-DEFLT         VALUE ' '.     FS7202U1
                 88  :FSSC:-BSVS-CHRGS-OVR-A-YES         VALUE 'A'.
             15  :FSSC:-HH-HRG-WGTS             COMP-3 PIC S9(2)V9(4).
             15  :FSSC:-CERT-MR-IND                      PIC X(1).
               88  :FSSC:-CERT-MR-NO-MR                  VALUE ' '.
               88  :FSSC:-CERT-MR-N-ROUTINE-MR           VALUE 'N'.
               88  :FSSC:-CERT-MR-Y-COMPLEX-MR           VALUE 'Y'.
             15  :FSSC:-ORIGINAL-DENIAL-CODE          OCCURS 4 TIMES.
               20  :FSSC:-ORIG-DENIAL                    PIC X(5).
             15  :FSSC:-C7274-FLAG                       PIC X(1).
               88  :FSSC:-C7274-FLAG-Y-YES               VALUE 'Y'.
               88  :FSSC:-C7274-FLAG-N-NO                VALUE 'N'.
             15  :FSSC:-SUSP-DUP-REV-IND                 PIC X(1).
               88  :FSSC:-SUSP-DUP-NO-REVIEW             VALUE ' '.
               88  :FSSC:-SUSP-DUP-1-NO-DUP              VALUE '1'.
             15  :FSSC:-LINE-TABLE                    OCCURS 5 TIMES.
               20 :FSSC:-CWF-LNEOVR-ERROR                PIC  X(5).
             15  :FSSC:-ORIG-HCPC-RATE          COMP-3 PIC S9(9)V999.
             15  :FSSC:-HCPC-TYPE                        PIC X(01).
               88  :FSSC:-HCPC-TYPE-M-MPFS               VALUE 'M'.
               88  :FSSC:-HCPC-TYPE-NOT-MPFS             VALUE ' '.
               88  :FSSC:-HCPC-TYPE-HEMO-CLOT-F          VALUE '1'.
             15  :FSSC:-HIPA-5010-SL-NBR                 PIC X(3).
             15  :FSSC:-HIPA-5010-SLD-IND                PIC X(1).
             15  :FSSC:-NDC-DATA.
               20 :FSSC:-NDC                             PIC  X(11).
               20 :FSSC:-NDC-QTY                COMP-3   PIC  9(7)V999.
               20 :FSSC:-NDC-QTY-QUAL                    PIC  X(2).
                 88  :FSSC:-NDC-QTY-5010-QUAL            VALUES
                                               'F2' 'GR' 'ME' 'ML' 'UN'.
                 88  :FSSC:-NDC-QTY-4010A1-QUAL          VALUES
                                               'F2' 'GR'      'ML' 'UN'.
                 88  :FSSC:-NDC-QTY-QUAL-DEFAULT         VALUE ' '.
             15  :FSSC:-FQHC-DUP-VISIT-IND               PIC X(1).
FS7202         88  :FSSC:-FQHC-DUP-VISIT-DEFLT           VALUE ' '.     FS7202U1
               88  :FSSC:-FQHC-DUP-VIS-Y-BYPASS          VALUE 'Y'.
               88  :FSSC:-FQHC-DUP-VIS-N-NO-BYP          VALUE 'N'.
             15  :FSSC:-FRAC-UNIT                        PIC X(1).
               88  :FSSC:-FRAC-UNIT-DEFAULT              VALUE ' '.
               88  :FSSC:-FRAC-UNIT-Y-YES-RC540          VALUE 'Y'.
             15  :FSSC:-NFACPE                  COMP-3   PIC S9(5)V99.
             15  :FSSC:-THERREDUC               COMP-3   PIC S9(5)V99.
             15  :FSSC:-ORIG-FEE                COMP-3   PIC S9(9)V99.
             15  :FSSC:-REDUCT-IND                       PIC X(01).
               88  :FSSC:-REDUCT-IND-DEFAULT             VALUE ' '.
CR9134         88  :FSSC:-REDUCT-B-MLT-GLB-SURG          VALUE 'B'.     CR9134R1
               88  :FSSC:-REDUCT-E-NON-EMRG-AMB          VALUE 'E'.
               88  :FSSC:-REDUCT-F-FUL-MLT-SURG          VALUE 'F'.
CR9134         88  :FSSC:-REDUCT-G-GLOBAL-SURG           VALUE 'G'.     CR9134R1
               88  :FSSC:-REDUCT-M-PRT-MLT-SURG          VALUE 'M'.
               88  :FSSC:-REDUCT-P-NORM-REDUCED          VALUE 'P'.
               88  :FSSC:-REDUCT-R-ALL-REDUCED           VALUE 'R'.
CR9647         88  :FSSC:-REDUCT-S-MPPR                  VALUE 'S'.
             15  :FSSC:-ESRD-PPS-DATA.
               20 :FSSC:-FULL-COMP-RATE         COMP-3   PIC S9(9)V99.
               20 :FSSC:-FULL-PPS-RATE          COMP-3   PIC S9(9)V99.
               20 :FSSC:-FULL-OUTLIER           COMP-3   PIC S9(9)V99.
               20 :FSSC:-BLEND-COMP-RATE        COMP-3   PIC S9(9)V99.
               20 :FSSC:-BLEND-PPS-RATE         COMP-3   PIC S9(9)V99.
               20 :FSSC:-BLEND-OUTLIER          COMP-3   PIC S9(9)V99.
             15  :FSSC:-RESP-DATA REDEFINES :FSSC:-ESRD-PPS-DATA.
               20 :FSSC:-RETURN-HIPPS1                   PIC X(05).
               20 :FSSC:-RETURN-HIPPS2                   PIC X(05).
               20 :FSSC:-SUB-DATE-DT            COMP-3   PIC 9(08).
               20 :FSSC:-ASSESS-CONV-DT         COMP-3   PIC 9(08).
C10257         20 :FSSC:-ASSESS-ID                       PIC X(15).
C10257         20 :FSSC:-FILLER                          PIC X(01).
             15  :FSSC:-GLOBAL-SW                        PIC X(1).
               88  :FSSC:-GLOBAL-SW-DEFAULT              VALUE ' '.
               88  :FSSC:-GLOBAL-SW-N-NO                 VALUE 'N'.
               88  :FSSC:-GLOBAL-SW-Y-YES                VALUE 'Y'.
             15  :FSSC:-LIN-SUS-CARC-TBL        OCCURS 06 TIMES.
               20 :FSSC:-LIN-SUS-CARC-TBL-IND            PIC X(01).
                 88  :FSSC:-LIN-SUS-CARC-DEFAULT         VALUE ' '.
                 88  :FSSC:-LIN-SUS-1-ADJ-LT-1996        VALUE '1'.
                 88  :FSSC:-LIN-SUS-2-STD-ADJ            VALUE '2'.
                 88  :FSSC:-LIN-SUS-3-SPECIAL-ADJ        VALUE '3'.
                 88  :FSSC:-LIN-SUS-4-MPPR-REDUCT        VALUE '4'.
                 88  :FSSC:-LIN-SUS-9-NOT-APPLY          VALUE '9'.
             15  :FSSC:-PWK-LINE-IND                     PIC X(02).
             15  :FSSC:-LOW-VOLUME              COMP-3   PIC S9(9)V99.
             15  :FSSC:-MULT-PROC-51                     PIC X(01).
             15  :FSSC:-ENDO-SET                         PIC X(03).
               88  :FSSC:-ENDO-SET-VALID                 VALUE '   '
                                                    '001' THRU '450'.
             15  :FSSC:-ENDO-BASE-CODE                   PIC X(05).
             15  :FSSC:-ENDO-BASE-RATE          COMP-3   PIC S9(9)V99.
             15  :FSSC:-REDUCED-RATE            COMP-3   PIC S9(9)V99.
             15  :FSSC:-SET-TOTAL-REIMB         COMP-3   PIC S9(9)V99.
             15  :FSSC:-LLR-REN-PHYS-ID-NPI              PIC 9(10).
             15  :FSSC:-LLR-REN-FILLER-F30               PIC X(2).
             15  :FSSC:-LLR-REN-FILLER-F31               PIC X(9).
             15  :FSSC:-LLR-REN-PHYS-NAME.
                 20  :FSSC:-LLR-REN-PHYS-LNAME           PIC X(17).
                 20  :FSSC:-LLR-REN-PHYS-FNAME           PIC X(8).
                 20  :FSSC:-LLR-REN-FILLER-F32           PIC X(4).
                 20  :FSSC:-LLR-REN-PHYS-MINT            PIC X(1).
             15  :FSSC:-LLR-REN-PHYS-FLAG                PIC X(1).
             15  :FSSC:-LLR-REN-PHYS-SC                  PIC X(02).
             15  :FSSC:-RB-DEMO-RED-AMT         COMP-3   PIC S9(9)V99.
             15  :FSSC:-RB-DEMO-GRP-CD                   PIC X(02).
               88  :FSSC:-RB-DEMO-CO-CONT-OBLIG          VALUE 'CO'.
             15  :FSSC:-RB-DEMO-CARC                     PIC X(03).
               88  :FSSC:-RB-DEMO-CARC-132-PRE           VALUE '132'.
             15  :FSSC:-FEE-IND                          PIC X(01).
               88  :FSSC:-FEE-B-BUNDLED-HCPC             VALUE 'B'.
FS7202         88  :FSSC:-FEE-R-REHAB-HCPC               VALUE 'R'.     FS7202U1
               88  :FSSC:-FEE-Q-NON-PAY-HCPC             VALUE 'Q'.
               88  :FSSC:-FEE-NA                         VALUE ' '.
             15  :FSSC:-FPS-D-MODEL                      PIC X(02).
             15  :FSSC:-FPS-D-MSN1                       PIC X(05).
             15  :FSSC:-FPS-D-MSN2                       PIC X(05).
             15  :FSSC:-LEG-CAP-INDS.
                 20  :FSSC:-LEG-CAP-IND-A                PIC X(1).
                   88  :FSSC:-LEG-CAP-A-OUTPAT             VALUE 'A'.
                   88  :FSSC:-LEG-CAP-A-NA                 VALUE ' '.
                 20  :FSSC:-LEG-CAP-IND-B                PIC X(1).
                   88  :FSSC:-LEG-CAP-B-ACCESS           VALUE 'B'.
                   88  :FSSC:-LEG-CAP-B-NA               VALUE ' '.
                 20  :FSSC:-LEG-CAP-IND-C                PIC X(1).
                   88  :FSSC:-LEG-CAP-C-DOS-NA            VALUE 'C'.
                   88  :FSSC:-LEG-CAP-C-NA                VALUE ' '.
                 20  :FSSC:-LEG-CAP-IND-D                PIC X(1).
                   88  :FSSC:-LEG-CAP-D-THR-NA           VALUE 'D'.
                   88  :FSSC:-LEG-CAP-D-NA               VALUE ' '.
                 20  :FSSC:-LEG-CAP-IND-E                PIC X(1).
             15  :FSSC:-PAY-RED-AMOUNT        COMP-3     PIC S9(9)V99.
             15  :FSSC:-PAY-RED-CARC                     PIC X(03).
FS7202         88  :FSSC:-PAY-RED-CO-CONT-OBLIG          VALUE 'CO '.   FS7202U1
               88  :FSSC:-PAY-RED-DEFAULT                VALUE '   '.
             15  :FSSC:-PAY-RED-GRP-CD                   PIC X(02).
FS7202       15  :FSSC:-RED-OVERRIDE-IND                 PIC X(01).     FS7202U1
CR8140       15  :FSSC:-ACO-RED-AMT           COMP-3     PIC S9(9)V99.
CR8140       15  :FSSC:-ACO-RED-CAGC                     PIC X(02).
CR8140       15  :FSSC:-ACO-RED-CARC                     PIC X(03).
CR8140       15  :FSSC:-ACO-RED-RARC                     PIC X(05).
CR8743       15  :FSSC:-ADD-ON-PYMT-AMT       COMP-3     PIC S9(9)V99.
CR8562       15  :FSSC:-UTN-LINE                         PIC X(14).
CR8562       15  :FSSC:-PROGRAM-IND                      PIC X(04).
CR8840       15  :FSSC:-PFSP-STAT-CD                     PIC X(01).
CR8840         88  :FSSC:-PFSP-STAT-CD-M                 VALUE 'M'.
CR8538       15  :FSSC:-EHR-RED-AMT           COMP-3     PIC S9(9)V99.  CR8538B
CR8538       15  :FSSC:-EHR-RED-CAGC                     PIC X(02).     CR8538B
CR8538           88  :FSSC:-CACG-CO-REDC                   VALUE 'CO'.  CR8538B
CR8538       15  :FSSC:-EHR-RED-CARC                     PIC X(03).     CR8538B
CR8538           88  :FSSC:-CARC-237-REDC                  VALUE '237'. CR8538B
CR8538       15  :FSSC:-EHR-RED-RARC                     PIC X(05).     CR8538B
CR8538           88  :FSSC:-RARC-700-REDC                  VALUE 'N700'.CR8538B
FS8518       15  :FSSC:-CAH-INCEN-IND                    PIC X(01).
FS8518           88  :FSSC:-CAH-INCEN-NONE               VALUE ' '.
FS8518           88  :FSSC:-CAH-INCEN-HPSA               VALUE '1'.
FS8518           88  :FSSC:-CAH-INCEN-PSA                VALUE '2'.
FS8518           88  :FSSC:-CAH-INCEN-HPSA-PSA           VALUE '3'.
FS8518           88  :FSSC:-CAH-INCEN-HSIP               VALUE '4'.
FS8518           88  :FSSC:-CAH-INCEN-HPSA-HSIP          VALUE '5'.
FS8518           88  :FSSC:-CAH-INCEN-PCIP               VALUE '6'.
FS8518           88  :FSSC:-CAH-INCEN-HPSA-PCIP          VALUE '7'.
FS7483       15  :FSSC:-REV-NCOV-CHRG-IND                PIC X(01).
FS7483           88  :FSSC:-REV-NCOV-T-TIMELINESS          VALUE 'T'.
CR9134       15  :FSSC:-PQRS-RED-AMT          COMP-3     PIC S9(9)V99.
CR9134       15  :FSSC:-PQRS-RED-CAGC                    PIC X(02).
CR9134           88  :FSSC:-CACG-CO-PQRS                   VALUE 'CO'.
CR9134       15  :FSSC:-PQRS-RED-CARC                    PIC X(03).
CR9134           88  :FSSC:-CARC-237-PQRS                  VALUE '237'. CR9134A
CR9134       15  :FSSC:-PQRS-RED-RARC                    PIC X(05).
CR9134           88  :FSSC:-RARC-699-PQRS                  VALUE 'N699'.
CR9202       15  :FSSC:-REP-PAYEE-LINE                   PIC X(01).
CR9202           88  :FSSC:-REP-PAYEE-LINE-VALID           VALUES ' '
CR9202                                                            'R'.
CR9202           88  :FSSC:-REP-PAYEE-LINE-SPACE           VALUE  ' '.
CR9202           88  :FSSC:-REP-PAYEE-LINE-R               VALUE  'R'.
CR9134       15  :FSSC:-PQRS-RED-PCT          COMP-3     PIC 999V99.    CR9134U3
FS0089       15  :FSSC:-EHRA-RED-PCT          COMP-3     PIC 999V99.    FS0089
CR9736       15  :FSSC:-NPWT-REPRICE-IND                 PIC X(01).
CR9736           88  :FSSC:-NPWT-REPRICE-SPACE             VALUE ' '.
CR9736           88  :FSSC:-NPWT-REPRICE-YES               VALUE 'Y'.
CR9656       15  :FSSC:-PMT-PT-AIPBP          COMP-3     PIC S9(9)V99.
CR9830       15  :FSSC:-COINS-PERCENT         COMP-3     PIC 9V99.
CR9915       15  :FSSC:-LUPA-RED-AMT          COMP-3     PIC S9(9)V99.
CR9915       15  :FSSC:-LUPA-RED-CAGC                    PIC X(02).
CR9915       15  :FSSC:-LUPA-RED-CARC                    PIC X(03).
CR9915       15  :FSSC:-LUPA-RED-RARC                    PIC X(05).
CR9911       15  :FSSC:-QMB-DED                          PIC X(05).
CR9911           88  :FSSC:-QMB-DED-N781                 VALUE 'N781 '.
CR9911       15  :FSSC:-QMB-COIN                         PIC X(05).
CR9911           88  :FSSC:-QMB-COIN-N782                VALUE 'N782 '.
C10065       15  :FSSC:-TDAPA-AMT             COMP-3     PIC 9(07)V9(4).
C10065       15  :FSSC:-VAR-FUTURE                       PIC X(455).