      ********************** | *****************************************
      ***                                                              *
      *** MEDICARE PART A                                              *
      *** FISS SYSTEM DOCUMENTATION                                    *
      ***                                                              *
      *|* MEMBER: FSRCCLXB                                             *
      ***                                                              *
      *** DESCRIPTION: CLAIM EXTRACT BASE RECORD (MINUS QUENCE)        *
      ***                                                              *
      *** FIXED LRECL: 12,000 BYTES                                    *
      ***                                                              *
      ******************************************************************
      ***                                                              *
      *** ANY CHANGES MAY REQUIRE UPDATES TO SOME OF THE FOLLOWING:    *
      ***   `CLMB, `CLML, `CLMP, `CLMQ, `CLMR, `CLMS, `CLMX,           *
      ***   'CLMY, `IDRC, `PDCL, `PDCP, `PDCR, `259R, SFSPDCL.         *
      ***                                                              *
      *** THIS COPYBOOK IS USED IN ONTER COPYBOOKS.                    *
      ***                                                              *
      ***$************************************************************$
           10  :FSSC:-PAYERS-ID-TABLE.
             15  :FSSC:-PAYERS-ID-DATA            OCCURS 3 TIMES.
               20  :FSSC:-PAYERS-ID                      PIC X(1).
                 88  :FSSC:-PAYERS-1-MEDICAID             VALUE '1'.
                 88  :FSSC:-PAYERS-2-BLUE-CROSS           VALUE '2'.
                 88  :FSSC:-PAYERS-3-OTHER                VALUE '3'.
                 88  :FSSC:-PAYERS-4-NONE                 VALUE '4'.
                 88  :FSSC:-PAYERS-A-WORKING-AGE          VALUE 'A'.
                 88  :FSSC:-PAYERS-B-ESRD                 VALUE 'B'.
                 88  :FSSC:-PAYERS-C-CONDITIONAL          VALUE 'C'.
                 88  :FSSC:-PAYERS-D-AUTO                 VALUE 'D'.
                 88  :FSSC:-PAYERS-E-WORKER-COMP          VALUE 'E'.
                 88  :FSSC:-PAYERS-F-FED-AGENCY           VALUE 'F'.
                 88  :FSSC:-PAYERS-G-DISABLED             VALUE 'G'.
                 88  :FSSC:-PAYERS-H-BLACK-LUNG           VALUE 'H'.
                 88  :FSSC:-PAYERS-I-VA                   VALUE 'I'.
                 88  :FSSC:-PAYERS-L-LIABILITY            VALUE 'L'.
                 88  :FSSC:-PAYERS-Z-MEDICARE             VALUE 'Z'.
               20  :FSSC:-PAYERS-NAME                    PIC X(32).
                 88  :FSSC:-MEDICARE                      VALUE
                                                          'MEDICARE  '.
                 88  :FSSC:-PAYERS-NAME-MEDICARE          VALUE
                                                          'MEDICARE  '.
               20  :FSSC:-REL-IND                        PIC X(1).
                 88  :FSSC:-REL-Y-RELEASE-OK              VALUE 'Y'.
                 88  :FSSC:-REL-N-NO-RELEASE              VALUE 'N'.
                 88  :FSSC:-REL-R-RESTRICTED              VALUE 'R'.
               20  :FSSC:-UB04-FILLER-F5                 PIC X(1).
               20  :FSSC:-ASSIGN-IND                     PIC X(1).
                 88  :FSSC:-ASSIGN-Y-ASSIGNED             VALUE 'Y'.
                 88  :FSSC:-ASSIGN-N-NOT-ASSIGNED         VALUE 'N'.
               20  :FSSC:-PROVIDER-NUMBER                PIC X(13).
               20  :FSSC:-UB04-FILLER-F6                 PIC X(2).
               20  :FSSC:-ADJ-DCN-ICN                    PIC X(23).
               20  :FSSC:-PRIOR-PMT               COMP-3 PIC S9(9)V99.
               20  :FSSC:-EST-AMT-DUE             COMP-3 PIC S9(9)V99.
           10  :FSSC:-INSURED-INFO-TABLE.
             15  :FSSC:-INSURED-INFORMATION       OCCURS 3 TIMES.
               20  :FSSC:-INSURED-DATA.
                 25  :FSSC:-INSURED-REL                  PIC 9(2).
                   88  :FSSC:-INS-REL-VALID               VALUE
                                                           00 THRU 20.
                   88  :FSSC:-INS-REL-00-DEFAULT          VALUE 00.
                   88  :FSSC:-INS-REL-01-PATIENT          VALUE 01.
                   88  :FSSC:-INS-REL-02-SPOUSE           VALUE 02.
                   88  :FSSC:-INS-REL-03-CHILD            VALUE 03.
                   88  :FSSC:-INS-REL-04-NOT-RESP         VALUE 04.
                   88  :FSSC:-INS-REL-05-STEP-CHILD       VALUE 05.
                   88  :FSSC:-INS-REL-06-FOSTER           VALUE 06.
                   88  :FSSC:-INS-REL-07-WARD-COURT       VALUE 07.
                   88  :FSSC:-INS-REL-08-EMPLOYEE         VALUE 08.
                   88  :FSSC:-INS-REL-09-UNKNOWN          VALUE 09.
                   88  :FSSC:-INS-REL-10-HANDICAP         VALUE 10.
                   88  :FSSC:-INS-REL-11-ORGAN-DON        VALUE 11.
                   88  :FSSC:-INS-REL-12-CADAV-DON        VALUE 12.
                   88  :FSSC:-INS-REL-13-GRANDCHILD       VALUE 13.
                   88  :FSSC:-INS-REL-14-NIECE-NEPH       VALUE 14.
                   88  :FSSC:-INS-REL-15-INJURED          VALUE 15.
                   88  :FSSC:-INS-REL-16-SPONSORED        VALUE 16.
                   88  :FSSC:-INS-REL-17-MINOR            VALUE 17.
                   88  :FSSC:-INS-REL-18-PARENT           VALUE 18.
                   88  :FSSC:-INS-REL-19-GRANDPAREN       VALUE 19.
                   88  :FSSC:-INS-REL-20-LIFE-PART        VALUE 20.
                 25  :FSSC:-UB04-FILLER-F12              PIC X(1).
                 25  :FSSC:-INSURED-NAME                 PIC X(25).
                 25  :FSSC:-UB04-FILLER-F9               PIC X(1).
                 25  :FSSC:-INSURED-SSN-HIC              PIC X(19).
                 25  :FSSC:-UB04-FILLER-F10              PIC X(1).
                 25  :FSSC:-INSURED-GROUP-NAME           PIC X(17).
                 25  :FSSC:-INSURED-GROUP-NBR            PIC X(20).
                 25  :FSSC:-TREAT-AUTH-CD                PIC X(18).
                 25  :FSSC:-UB04-FILLER-F11              PIC X(13).
               20  :FSSC:-BENE-Z-DATA         REDEFINES
                   :FSSC:-INSURED-DATA.
                 25  :FSSC:-BENE-REL                     PIC 9(2).
                 25  :FSSC:-UB04-FILLER-F12A             PIC X(1).
                 25  :FSSC:-BENE-NAME.
                   30  :FSSC:-BENE-LAST-NAME             PIC X(15).
                   30  :FSSC:-BENE-FIRST-NAME            PIC X(10).
                 25  :FSSC:-UB04-FILLER-F13              PIC X(1).
                 25  :FSSC:-BENE-SSN-HIC                 PIC X(19).
                 25  :FSSC:-UB04-FILLER-F14              PIC X(1).
                 25  :FSSC:-BENE-MID-INIT                PIC X(1).
                 25  :FSSC:-BENE-DOB                     PIC X(8).
                 25  :FSSC:-BENE-SEX                     PIC X(1).
                   88  :FSSC:-BENE-SEX-F-FEMALE           VALUE 'F'.
                   88  :FSSC:-BENE-SEX-M-MALE             VALUE 'M'.
                   88  :FSSC:-BENE-SEX-U-UNKNOWN          VALUE 'U'.
FS8132           25  :FSSC:-UB04-FILLER-F14A             PIC X(27).
FS8132           25  :FSSC:-BENE-TREAT-AUTH-CD           PIC X(18).
FS8132           25  :FSSC:-UB04-FILLER-F14B             PIC X(13).
           10  :FSSC:-EMPLOYMENT-DATA             OCCURS 3 TIMES
                                            INDEXED BY :FSSC:-EMPDX.
             15  :FSSC:-EMP-STATUS-CD                    PIC X(1).
               88  :FSSC:-EMP-STAT-1-FULL-TIME            VALUE '1'.
               88  :FSSC:-EMP-STAT-2-PART-TIME            VALUE '2'.
               88  :FSSC:-EMP-STAT-3-UNEMPLOYED           VALUE '3'.
               88  :FSSC:-EMP-STAT-4-SELF-EMP             VALUE '4'.
               88  :FSSC:-EMP-STAT-5-RETIRED              VALUE '5'.
               88  :FSSC:-EMP-STAT-6-MILITARY             VALUE '6'.
               88  :FSSC:-EMP-STAT-9-UNKNOWN              VALUE '9'.
             15  :FSSC:-EMP-NAME                         PIC X(24).
             15  :FSSC:-UB04-FILLER-F15                  PIC X(1).
             15  :FSSC:-EMP-LOCATION.
               20  :FSSC:-EMP-STREET                     PIC X(18).
               20  :FSSC:-EMP-CITY                       PIC X(15).
               20  :FSSC:-EMP-STATE                      PIC X(2).
               20  :FSSC:-EMP-ZIP.
                 25  :FSSC:-EMP-ZIP-5                    PIC 9(5).
                 25  :FSSC:-EMP-ZIP-4                    PIC 9(4).
           10  :FSSC:-CLAIM-PATH.
             15  :FSSC:-CLAIM-PATH-ENTRY          OCCURS 20 TIMES
                                            INDEXED BY :FSSC:-CPNDX.
               20 :FSSC:-CLAIM-PATH-MODE                 PIC X(1).
                 88  :FSSC:-CLAIM-PATH-B-BATCH            VALUE 'B'.
                 88  :FSSC:-CLAIM-PATH-O-ONLINE           VALUE 'O'.
               20 :FSSC:-CLAIM-PATH-LOC                  PIC X(2).
               20 :FSSC:-CLAIM-PATH-REPS                 PIC X(1).
               20 :FSSC:-CLAIM-PATH-FLAG                 PIC X(1).
                 88  :FSSC:-CLAIM-PATH-F-S-SUSP           VALUE 'S'.
                 88  :FSSC:-CLAIM-PATH-F-Y-COMP           VALUE 'Y'.
           10  :FSSC:-AUDIT-TRAIL.
             15  :FSSC:-BADT-ENTRY                OCCURS 25 TIMES
                                            INDEXED BY :FSSC:-BANDX.
               20  :FSSC:-BADT-STAT-LOC.
                 25  :FSSC:-BADT-STATUS                  PIC X(1).
                 25  :FSSC:-BADT-LOC                     PIC X(5).
               20  :FSSC:-BADT-OPER-ID                   PIC X(9).
               20  :FSSC:-BADT-REAS-TABLE.
                 25  :FSSC:-BADT-REAS                    PIC X(5).
               20  :FSSC:-BADT-CURR-DATE-CYMD.
                 25  :FSSC:-BADT-CURR-DATE-CC            PIC 9(2).
                 25  :FSSC:-BADT-CURR-DATE.
                   30  :FSSC:-BADT-YY                    PIC 9(2).
                   30  :FSSC:-BADT-MM                    PIC 9(2).
                   30  :FSSC:-BADT-DD                    PIC 9(2).
           10  :FSSC:-MSA-PRICER-CD                      PIC X(4).
           10  :FSSC:-BC-BS-PROV-ID                      PIC X(13).
           10  :FSSC:-INTERMEDIARY-NB                    PIC X(5).
           10  :FSSC:-MSPPAY-IND                         PIC X(1).
             88  :FSSC:-MSPPAY-Y-SAVINGS-CALC             VALUE 'Y'.
           10  :FSSC:-DIAG-CODE-DATA              OCCURS 25 TIMES
                                            INDEXED BY :FSSC:-DCNDX.
             15  :FSSC:-DIAGNOSIS-CODES.
               20  :FSSC:-DIAG-CD-2                      PIC X(7).
               20 :FSSC:-DIAG-POA-IND                    PIC X(1).
                 88  :FSSC:-DIAG-POA-VALID
                     VALUES 'Y' 'N' 'U' 'W' '1'.
                 88  :FSSC:-DIAG-POA-Y-PRESENT            VALUE 'Y'.
                 88  :FSSC:-DIAG-POA-N-NOT-PRESEN         VALUE 'N'.
                 88  :FSSC:-DIAG-POA-U-UNKNOWN            VALUE 'U'.
                 88  :FSSC:-DIAG-POA-W-UNDETERMIN         VALUE 'W'.
                 88  :FSSC:-DIAG-POA-1-UNREPORTED         VALUE '1'.
                 88  :FSSC:-DIAG-POA-NA                   VALUE ' '.
             15  :FSSC:-BIT-FLAGS                        PIC X(4).
FS7350     10  :FSSC:-DIAG-END-OF-POA.
FS7350       15  :FSSC:-END-OF-POA-FUTURE                PIC X(1).
FS7350       15  :FSSC:-END-OF-POA-IND                   PIC X(1).
FS7350         88  :FSSC:-END-OF-POA-VALID               VALUES 'X' 'Z'.
FS7350         88  :FSSC:-END-OF-POA-X-END                VALUE 'X'.
FS7350         88  :FSSC:-END-OF-POA-Z-END-SPEC           VALUE 'Z'.
FS7350         88  :FSSC:-END-OF-POA-NA                   VALUE ' '.
           10  :FSSC:-HOLD-STATUS-LOC.
             15  :FSSC:-HOLD-STATUS                      PIC X(1).
             15  :FSSC:-HOLD-LOCATION                    PIC X(5).
           10  :FSSC:-PSR-PPS-BLEND-YR                   PIC X(1).
           10  :FSSC:-PAT-MED-REC-NO                     PIC X(20).
           10  :FSSC:-UB04-FILLER-F16                    PIC X(4).
           10  :FSSC:-OPER-ID-DATA.
             15  :FSSC:-OPER-ID.
               20  :FSSC:-DDE-PROV                       PIC X(6).
               20  :FSSC:-OPER-ID-DEPT                   PIC X(3).
             15  :FSSC:-OPER-ID-FILL                     PIC X(4).
           10  :FSSC:-PPS-IND                            PIC X(1).
             88  :FSSC:-PPS-Y-PPS-PROV                    VALUE 'Y'.
             88  :FSSC:-PPS-N-NOT-PPS-PROV                VALUE 'N'.
           10  :FSSC:-CURR-REAS-TABLE.
             15  :FSSC:-CURRENT-REASON            OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-CRNDX.
               20  :FSSC:-CURR-REAS-CD                   PIC X(5).
           10  :FSSC:-PRO-ID                             PIC X(5).
           10  :FSSC:-REIMB-DIST.
             15  :FSSC:-REIMB-METH-CD                    PIC X(1).
             88  :FSSC:-REIMB-C-PAT-FILED                 VALUE 'C'.
             88  :FSSC:-REIMB-D-PIP                       VALUE 'D'.
             88  :FSSC:-REIMB-G-PPS                       VALUE 'G'.
             88  :FSSC:-REIMB-I-EMERGENCY                 VALUE 'I'.
FS8636       88  :FSSC:-REIMB-J-METHOD-II                 VALUE 'J'.
             88  :FSSC:-REIMB-P-PERCENTAGE                VALUE 'P'.
             88  :FSSC:-REIMB-R-PER-DIEM                  VALUE 'R'.
           10  :FSSC:-CWF-RECORD-ID                      PIC X(4).
             88  :FSSC:-INPAT-CWF-REC-ID                  VALUE 'HUIP'.
             88  :FSSC:-OUTPAT-CWF-REC-ID                 VALUE 'HUOP'.
             88  :FSSC:-CWF-HUHH-HOME-HEALTH              VALUE 'HUHH'.
             88  :FSSC:-CWF-HUIP-INPAT-SNF                VALUE 'HUIP'.
             88  :FSSC:-CWF-HUOP-OUTPATIENT               VALUE 'HUOP'.
             88  :FSSC:-CWF-HUHC-HOSPICE                  VALUE 'HUHC'.
           10  :FSSC:-CWF-ACTION-CD                      PIC X(1).
             88  :FSSC:-CWF-ACT-1-ORIGINAL                VALUE '1'.
             88  :FSSC:-CWF-ACT-2-CREDIT-ADJ              VALUE '2'.
             88  :FSSC:-CWF-ACT-3-DEBIT-ADJ               VALUE '3'.
             88  :FSSC:-CWF-ACT-4-CANCEL-ONLY             VALUE '4'.
             88  :FSSC:-CWF-ACT-7-HISTORY                 VALUE '7'.
             88  :FSSC:-CWF-ACT-8-REFUSED                 VALUE '8'.
             88  :FSSC:-CWF-ACT-9-PAY-REQUEST             VALUE '9'.
           10  :FSSC:-ADM-SOURCE                         PIC X(1).
             88  :FSSC:-ADM-1-PHYSICIAN-REFER             VALUE '1'.
             88  :FSSC:-ADM-2-CLINICAL-REFER              VALUE '2'.
             88  :FSSC:-ADM-3-HMO-REFERRAL                VALUE '3'.
             88  :FSSC:-ADM-4-HOSP-TRANSFER               VALUE '4'.
             88  :FSSC:-ADM-5-SNF-TRANSFER                VALUE '5'.
             88  :FSSC:-ADM-6-OTHER-TRANSFER              VALUE '6'.
             88  :FSSC:-ADM-7-EMERGENCY-ROOM              VALUE '7'.
             88  :FSSC:-ADM-8-COURT-OR-LAW                VALUE '8'.
             88  :FSSC:-ADM-9-NOT-AVAILABLE               VALUE '9'.
             88  :FSSC:-ADM-A-CAH-TRANSFER                VALUE 'A'.
             88  :FSSC:-ADM-B-HH-TRANFER                  VALUE 'B'.
             88  :FSSC:-ADM-C-HH-READMISSION              VALUE 'C'.
             88  :FSSC:-ADM-D-SAME-FACILITY               VALUE 'D'.
           10  :FSSC:-UB04-FILLER-F17                    PIC X(2).
           10  :FSSC:-CWF-DISP-CODE                      PIC X(2).
FS0231       88  :FSSC:-CWF-DISP-ACCEPTED
FS0231           VALUES '01' '02' '03' '04' '05'.
             88  :FSSC:-CWF-DISP-01-DEBIT                 VALUE '01'.
             88  :FSSC:-CWF-DISP-02-DEBIT-ADJ             VALUE '02'.
             88  :FSSC:-CWF-DISP-03-CANCEL                VALUE '03'.
             88  :FSSC:-CWF-DISP-04-HIST-ONLY             VALUE '04'.
FS0231       88  :FSSC:-CWF-DISP-05-CAN-RETRN             VALUE '05'.
             88  :FSSC:-CWF-DISP-50-NIF                   VALUE '50'.
             88  :FSSC:-CWF-DISP-51-TRUE-NIF              VALUE '51'.
             88  :FSSC:-CWF-DISP-52-OTH-SITE              VALUE '52'.
             88  :FSSC:-CWF-DISP-53-ALPHA-MAT             VALUE '53'.
             88  :FSSC:-CWF-DISP-55-MISMATCH              VALUE '55'.
C10092       88  :FSSC:-CWF-DISP-56-HICMBI-NF             VALUE '56'.
             88  :FSSC:-CWF-DISP-57-ARCHIVED              VALUE '57'.
             88  :FSSC:-CWF-DISP-58-BLOCKED               VALUE '58'.
             88  :FSSC:-CWF-DISP-59-FROZEN                VALUE '59'.
             88  :FSSC:-CWF-DISP-60-I-O-ERROR             VALUE '60'.
             88  :FSSC:-CWF-DISP-61-PROBLEM               VALUE '61'.
             88  :FSSC:-CWF-DISP-AA-DEBIT-ADJ             VALUE 'AA'.
             88  :FSSC:-CWF-DISP-AB-ABEND                 VALUE 'AB'.
             88  :FSSC:-CWF-DISP-BT-NO-HIST               VALUE 'BT'.
             88  :FSSC:-CWF-DISP-CI-CICS-ERR              VALUE 'CI'.
             88  :FSSC:-CWF-DISP-CR-XOVER-REJ             VALUE 'CR'.
             88  :FSSC:-CWF-DISP-ER-EDIT-REJ              VALUE 'ER'.
             88  :FSSC:-CWF-DISP-RD-TRANS-ERR             VALUE 'RD'.
             88  :FSSC:-CWF-DISP-RT-RETRIEVE              VALUE 'RT'.
             88  :FSSC:-CWF-DISP-UR-UTIL-REJ              VALUE 'UR'.
           10  :FSSC:-PPS-EFFECT-DATE-CYMD.
             15  :FSSC:-PPS-EFFECT-DATE-CC               PIC X(2).
             15  :FSSC:-PPS-EFFECT-DATE.
               20  :FSSC:-PPS-EFFECT-DATE-YY             PIC X(2).
               20  :FSSC:-PPS-EFFECT-DATE-MM             PIC X(2).
               20  :FSSC:-PPS-EFFECT-DATE-DD             PIC X(2).
           10  :FSSC:-COND-CODE-TABLE.
             15  :FSSC:-CONDITION-CODES           OCCURS 30 TIMES
                                            INDEXED BY :FSSC:-CCNDX.
               20  :FSSC:-COND-CD                        PIC X(2).
               20  :FSSC:-UB04-FILLER-F18                PIC X(1).
           10  :FSSC:-ADM-TYP-CD                         PIC X(1).
             88  :FSSC:-ADM-TYP-1-EMERGENCY               VALUE '1'.
             88  :FSSC:-ADM-TYP-2-URGENT                  VALUE '2'.
             88  :FSSC:-ADM-TYP-3-ELECTIVE                VALUE '3'.
             88  :FSSC:-ADM-TYP-4-NEWBORN                 VALUE '4'.
             88  :FSSC:-ADM-TYP-5-TRAUMA                  VALUE '5'.
           10  :FSSC:-UB04-FILLER-F19                    PIC X(2).
           10  :FSSC:-MED-POLICY-TABLE.
             15  :FSSC:-MED-POLICY-REASONS        OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-MEDPX.
               20 :FSSC:-MED-POL-REASON                  PIC X(5).
           10  :FSSC:-UB04-FILLER-F20                    PIC X(1).
           10  :FSSC:-ORIGINAL-TOB.
             15  :FSSC:-ORIGINAL-CAT                     PIC X(2).
             15  :FSSC:-ORIGINAL-FREQ                    PIC X(1).
           10  :FSSC:-UB04-FILLER-F21                    PIC X(1).
FS8716     10  FILLER                                    PIC X(1).
           10  :FSSC:-MSP-OVR-B                          PIC X(1).
             88  :FSSC:-MSP-OVR-B-OVERRIDE                VALUE 'Y'.
           10  :FSSC:-MSP-OVR-C                          PIC X(1).
             88  :FSSC:-MSP-OVR-C-OVERRIDE                VALUE 'Y'.
FS8716     10  FILLER                                    PIC X(6).
           10  :FSSC:-MSP-OVR-J                          PIC X(1).
             88  :FSSC:-MSP-OVR-J-OVERRIDE                VALUE 'Y'.
FS8716     10  FILLER                                    PIC X(1).
           10  :FSSC:-MSP-OVR-FUTURE                     PIC X(3).
           10  :FSSC:-BDL-LETTER-TABLE.
             15  :FSSC:-BDL-LETTER-CODES          OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-BDLLX.
               20 :FSSC:-BDL-LETTER-CD                   PIC X(5).
           10  :FSSC:-CAP2-PAY-CODE                      PIC X(1).
             88  :FSSC:-CAP2-PAY-A-HOLD-HARM              VALUE 'A'.
             88  :FSSC:-CAP2-PAY-B-HOLD-100               VALUE 'B'.
             88  :FSSC:-CAP2-PAY-C-FULL-RATE              VALUE 'C'.
             88  :FSSC:-CAP2-PAY-NEW-HOSP                 VALUE ' '.
           10  :FSSC:-REJ-ADJ-SW                         PIC X(1).
             88  :FSSC:-REJ-ADJ-P-RECALCULATE             VALUE 'P'.
             88  :FSSC:-REJ-ADJ-DONT-RECALC               VALUE ' '.
           10  :FSSC:-XREF-DCN-NBR                       PIC X(23).
           10  :FSSC:-SEC-PAYOR-RPT-TYPE                 PIC X(1).
      ****        FULL OR PARTIAL
             88  :FSSC:-SEC-PAY-1-WC-BL-FED               VALUE '1'.
             88  :FSSC:-SEC-PAY-2-WORKING-AGE             VALUE '2'.
             88  :FSSC:-SEC-PAY-3-ESRD                    VALUE '3'.
             88  :FSSC:-SEC-PAY-4-AUTO                    VALUE '4'.
             88  :FSSC:-SEC-PAY-5-DISABLED                VALUE '5'.
             88  :FSSC:-SEC-PAY-6-LIABILITY               VALUE '6'.
      ****        COST AVOIDED
             88  :FSSC:-SEC-PAY-1-WORKING-AGE             VALUE '1'.
             88  :FSSC:-SEC-PAY-2-ESRD                    VALUE '2'.
             88  :FSSC:-SEC-PAY-3-AUTO                    VALUE '3'.
             88  :FSSC:-SEC-PAY-4-WORKER-COMP             VALUE '4'.
             88  :FSSC:-SEC-PAY-5-FED-AGENCY              VALUE '5'.
             88  :FSSC:-SEC-PAY-6-DISABLED                VALUE '6'.
             88  :FSSC:-SEC-PAY-7-BLACK-LUNG              VALUE '7'.
             88  :FSSC:-SEC-PAY-8-VA                      VALUE '8'.
             88  :FSSC:-SEC-PAY-9-LIABILITY               VALUE '9'.
           10  :FSSC:-ADJ-REQ-CD                         PIC X(1).
             88  :FSSC:-ADJ-REQ-F-FI                      VALUE 'F'.
             88  :FSSC:-ADJ-REQ-H-PROVIDER                VALUE 'H'.
             88  :FSSC:-ADJ-REQ-P-PRO                     VALUE 'P'.
             88  :FSSC:-ADJ-REQ-S-SYSTEM                  VALUE 'S'.
           10  :FSSC:-CANCEL-XREF-DCN                    PIC X(23).
           10  :FSSC:-ADM-DIAG-FLAG                      PIC X(4).
           10  :FSSC:-ADJ-REAS-CD                        PIC X(2).
             88  :FSSC:-ADJ-REAS-AA-AUTO-ADJ              VALUE 'AA'.
             88  :FSSC:-ADJ-REAS-AD-TECH-DEN              VALUE 'AD'.
             88  :FSSC:-ADJ-REAS-AM-MED-DEN               VALUE 'AM'.
             88  :FSSC:-ADJ-REAS-AR-ADMIT-REV             VALUE 'AR'.
             88  :FSSC:-ADJ-REAS-AU-AUTO                  VALUE 'AU'.
             88  :FSSC:-ADJ-REAS-AW-WAIVER                VALUE 'AW'.
             88  :FSSC:-ADJ-REAS-BL-BLACK-L               VALUE 'BL'.
             88  :FSSC:-ADJ-REAS-CA-COST-OUTA             VALUE 'CA'.
             88  :FSSC:-ADJ-REAS-CB-CR-BAL                VALUE 'CB'.
             88  :FSSC:-ADJ-REAS-CD-COV-DAYS              VALUE 'CD'.
             88  :FSSC:-ADJ-REAS-CO-COST-OUTN             VALUE 'CO'.
             88  :FSSC:-ADJ-REAS-CP-COST-OUTP             VALUE 'CP'.
             88  :FSSC:-ADJ-REAS-CR-RECON                 VALUE 'CR'.
             88  :FSSC:-ADJ-REAS-CW-COST-OUTW             VALUE 'CW'.
             88  :FSSC:-ADJ-REAS-DA-DAY-OUT               VALUE 'DA'.
             88  :FSSC:-ADJ-REAS-DB-DISABLED              VALUE 'DB'.
             88  :FSSC:-ADJ-REAS-DC-DIAGNOSIS             VALUE 'DC'.
             88  :FSSC:-ADJ-REAS-DD-DISC-DEST             VALUE 'DD'.
             88  :FSSC:-ADJ-REAS-DG-DRG-DEN-G             VALUE 'DG'.
             88  :FSSC:-ADJ-REAS-DH-DRG-DEN-H             VALUE 'DH'.
             88  :FSSC:-ADJ-REAS-DI-BENE-LIAB             VALUE 'DI'.
             88  :FSSC:-ADJ-REAS-DO-DAY-OUT-D             VALUE 'DO'.
             88  :FSSC:-ADJ-REAS-DP-DIAG-PROC             VALUE 'DP'.
             88  :FSSC:-ADJ-REAS-DS-DISC-STAT             VALUE 'DS'.
             88  :FSSC:-ADJ-REAS-DV-DRG-VAL-C             VALUE 'DV'.
             88  :FSSC:-ADJ-REAS-DW-DAY-OUT-W             VALUE 'DW'.
             88  :FSSC:-ADJ-REAS-EF-ESRD-ADJ              VALUE 'EF'.
             88  :FSSC:-ADJ-REAS-ES-ESRD                  VALUE 'ES'.
             88  :FSSC:-ADJ-REAS-FB-BENE-LIAB             VALUE 'FB'.
             88  :FSSC:-ADJ-REAS-FC-HHPPS-FIN             VALUE 'FC'.
             88  :FSSC:-ADJ-REAS-FD-FULL-DEN              VALUE 'FD'.
             88  :FSSC:-ADJ-REAS-FR-FULL-REV              VALUE 'FR'.
             88  :FSSC:-ADJ-REAS-FT-FULL-TECH             VALUE 'FT'.
             88  :FSSC:-ADJ-REAS-HA-HH-AUDITS             VALUE 'HA'.
             88  :FSSC:-ADJ-REAS-HC-HH-REVIEW             VALUE 'HC'.
             88  :FSSC:-ADJ-REAS-HD-HMO-DISEN             VALUE 'HD'.
             88  :FSSC:-ADJ-REAS-HP-HMO-PAY               VALUE 'HP'.
             88  :FSSC:-ADJ-REAS-IB-INTERIM               VALUE 'IB'.
             88  :FSSC:-ADJ-REAS-IC-REV-CODES             VALUE 'IC'.
             88  :FSSC:-ADJ-REAS-ID-DEDUCT                VALUE 'ID'.
             88  :FSSC:-ADJ-REAS-JP-DEEM-DAYS             VALUE 'JP'.
             88  :FSSC:-ADJ-REAS-KB-DEEM-DAYS             VALUE 'KB'.
             88  :FSSC:-ADJ-REAS-KD-DEEM-DIAG             VALUE 'KD'.
             88  :FSSC:-ADJ-REAS-KP-DEEM-PROC             VALUE 'KP'.
             88  :FSSC:-ADJ-REAS-LD-DEEM-OUTL             VALUE 'LD'.
             88  :FSSC:-ADJ-REAS-LI-LIABILITY             VALUE 'LI'.
             88  :FSSC:-ADJ-REAS-LS-LOS-NOPAY             VALUE 'LS'.
             88  :FSSC:-ADJ-REAS-LW-LOS-WAIV              VALUE 'LW'.
             88  :FSSC:-ADJ-REAS-MC-DEEM-COST             VALUE 'MC'.
             88  :FSSC:-ADJ-REAS-NF-HHPPS-NO              VALUE 'NF'.
             88  :FSSC:-ADJ-REAS-OC-PROC-CODE             VALUE 'OC'.
             88  :FSSC:-ADJ-REAS-OP-DAY-OUT-A             VALUE 'OP'.
             88  :FSSC:-ADJ-REAS-OT-OTHER                 VALUE 'OT'.
             88  :FSSC:-ADJ-REAS-PC-PROC                  VALUE 'PC'.
             88  :FSSC:-ADJ-REAS-PD-PROC-DEN              VALUE 'PD'.
             88  :FSSC:-ADJ-REAS-PH-PHS-MSP16             VALUE 'PH'.
             88  :FSSC:-ADJ-REAS-PI-INTEGRITY             VALUE 'PI'.
             88  :FSSC:-ADJ-REAS-PN-PROVIDER              VALUE 'PN'.
             88  :FSSC:-ADJ-REAS-PP-DISC-STAT             VALUE 'PP'.
             88  :FSSC:-ADJ-REAS-PR-PREV-ADJ              VALUE 'PR'.
             88  :FSSC:-ADJ-REAS-PT-ADMIT-DEN             VALUE 'PT'.
             88  :FSSC:-ADJ-REAS-PW-PROC-WAV              VALUE 'PW'.
             88  :FSSC:-ADJ-REAS-QC-PROC-CODE             VALUE 'QC'.
             88  :FSSC:-ADJ-REAS-QD-ANCILLARY             VALUE 'QD'.
             88  :FSSC:-ADJ-REAS-QR-HCPC                  VALUE 'QR'.
             88  :FSSC:-ADJ-REAS-RI-RAC-OVERP             VALUE 'RI'.
CR8581       88  :FSSC:-ADJ-REAS-R1-LE-1YEAR              VALUE 'R1'.
CR8581       88  :FSSC:-ADJ-REAS-R2-GT-1YEAR              VALUE 'R2'.
CR8581       88  :FSSC:-ADJ-REAS-R3-GT-4YEARS             VALUE 'R3'.
             88  :FSSC:-ADJ-REAS-RC-REVERSAL              VALUE 'RC'.
             88  :FSSC:-ADJ-REAS-RP-PART-REV              VALUE 'RP'.
             88  :FSSC:-ADJ-REAS-SB-SAME-PER              VALUE 'SB'.
             88  :FSSC:-ADJ-REAS-SD-7-DAY-DEN             VALUE 'SD'.
             88  :FSSC:-ADJ-REAS-SW-7-DAY-WAV             VALUE 'SW'.
             88  :FSSC:-ADJ-REAS-TD-TRANS-NO              VALUE 'TD'.
FS0174       88  :FSSC:-ADJ-REAS-TL-TELEPHONE             VALUE 'TL'.
             88  :FSSC:-ADJ-REAS-TW-TRANS-WAV             VALUE 'TW'.
             88  :FSSC:-ADJ-REAS-VA-VA                    VALUE 'VA'.
             88  :FSSC:-ADJ-REAS-WC-WORK-COMP             VALUE 'WC'.
             88  :FSSC:-ADJ-REAS-WE-WORK-AGED             VALUE 'WE'.
             88  :FSSC:-ADJ-REAS-YA-PACE-DEN              VALUE 'YA'.
             88  :FSSC:-ADJ-REAS-YB-PACE-ERR              VALUE 'YB'.
             88  :FSSC:-ADJ-REAS-YC-PACE-REV              VALUE 'YC'.
             88  :FSSC:-ADJ-REAS-YD-PACE-NO               VALUE 'YD'.
             88  :FSSC:-ADJ-REAS-ZW-DEBIT-ADJ             VALUE 'ZW'.
           10  :FSSC:-HCPC-MULTI-LAB                     PIC X(1).
             88  :FSSC:-HCPC-MULTI-M-PAID                 VALUE 'M'.
FS7202       88  :FSSC:-HCPC-MULTI-S-MULTI-CH             VALUE 'S'.    FS7202U1
             88  :FSSC:-HCPC-MULTI-Y-PAID                 VALUE 'Y'.
           10  :FSSC:-CANC-ADJ-CD                        PIC X(1).
             88  :FSSC:-CANC-ADJ-B-HH                     VALUE 'B'.
             88  :FSSC:-CANC-ADJ-C-COVERAGE               VALUE 'C'.
             88  :FSSC:-CANC-ADJ-D-DUPLICATE              VALUE 'D'.
             88  :FSSC:-CANC-ADJ-E-HH                     VALUE 'E'.
             88  :FSSC:-CANC-ADJ-F-HH                     VALUE 'F'.
             88  :FSSC:-CANC-ADJ-H-OTHER                  VALUE 'H'.
             88  :FSSC:-CANC-ADJ-P-PLAN-TRANS             VALUE 'P'.
             88  :FSSC:-CANC-ADJ-S-SCRAMBLE               VALUE 'S'.
             88  :FSSC:-CANC-ADJ-F-FI                     VALUE 'F'.
             88  :FSSC:-CANC-ADJ-H-PROVIDER               VALUE 'H'.
             88  :FSSC:-CANC-ADJ-P-PRO                    VALUE 'P'.
             88  :FSSC:-CANC-ADJ-S-SYSTEM                 VALUE 'S'.
           10  :FSSC:-LUGAR-RECLAS-CD                    PIC X(1).
             88  :FSSC:-LUGAR-RECLAS-L-URBAN              VALUE 'L'.
             88  :FSSC:-LUGAR-RECLAS-NON-URB              VALUE ' '.
           10  :FSSC:-XREF-HIC-NBR                       PIC X(12).
           10  :FSSC:-TYP-OF-SAVINGS-CD                  PIC X(1).
             88  :FSSC:-TYP-OF-SAVINGS-1-MSP              VALUE '1'.
           10  :FSSC:-ORIGINAL-XREF-DCN                  PIC X(23).
           10  :FSSC:-DCN-DUPED-AGAINST                  PIC X(23).
           10  :FSSC:-SEC-PAYOR-TYP-SVG                  PIC X(2).
             88  :FSSC:-SEC-PAY-FR-FULL-REC               VALUE 'FR'.
             88  :FSSC:-SEC-PAY-IA-COST-AVOID             VALUE 'IA'.
             88  :FSSC:-SEC-PAY-ID-INIT-DEN               VALUE 'ID'.
             88  :FSSC:-SEC-PAY-PR-PARTIAL                VALUE 'PR'.
             88  :FSSC:-SEC-PAY-DEFAULT                   VALUE '  '.
           10  :FSSC:-CONDITION-ID                       PIC X(5).
           10  :FSSC:-HOLD-SEC-PAY-TYP-SVG               PIC X(2).
           10  :FSSC:-BYPASS-72X-OVERLAP                 PIC X(1).
             88  :FSSC:-BYPASS-72X-Y-YES                  VALUE 'Y'.
             88  :FSSC:-BYPASS-72X-N-NO                   VALUE 'N'.
           10  :FSSC:-ORIG-MSP-CD                        PIC X(2).
             88  :FSSC:-ORIG-MSP-12-WORKING               VALUE '12'.
             88  :FSSC:-ORIG-MSP-13-ESRD                  VALUE '13'.
             88  :FSSC:-ORIG-MSP-14-AUTO                  VALUE '14'.
             88  :FSSC:-ORIG-MSP-15-WORKER                VALUE '15'.
             88  :FSSC:-ORIG-MSP-16-FED                   VALUE '16'.
             88  :FSSC:-ORIG-MSP-41-BLACK-L               VALUE '41'.
             88  :FSSC:-ORIG-MSP-42-VA                    VALUE '42'.
             88  :FSSC:-ORIG-MSP-43-DISABLED              VALUE '43'.
             88  :FSSC:-ORIG-MSP-47-LIABILITY             VALUE '47'.
             88  :FSSC:-ORIG-MSP-DEFAULT                  VALUE '  '.
           10  :FSSC:-CWF-ADJ-DCN                        PIC X(23).
           10  :FSSC:-MED-REVIEW-ANALYST-ID              PIC X(9).
           10  :FSSC:-OCC-23-SET                         PIC X(1).
             88  :FSSC:-OCC-23-SET-Y-YES                  VALUE 'Y'.
             88  :FSSC:-OCC-23-SET-DEFAULT                VALUE ' '.
           10  :FSSC:-PRO-ERROR-REASON                   PIC X(7).
           10  :FSSC:-ESRD-DATA.
             15  :FSSC:-ESRD-DIAL-SITE-CD                PIC X(1).
             15  :FSSC:-ESRD-DIAG-CD                     PIC X(6).
             15  :FSSC:-ESRD-PROC-CD                     PIC X(7).
           10  :FSSC:-MED-REVIEW-TABLE.
             15  :FSSC:-MED-REVIEW-REASON         OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-MEDRX.
               20  :FSSC:-MED-REVIEW-RSN                 PIC X(5).
           10  :FSSC:-OSC-70-SET                         PIC X(1).
             88  :FSSC:-OSC-70-SET-Y-YES                  VALUE 'Y'.
             88  :FSSC:-OSC-70-SET-DEFAULT                VALUE ' '.
           10  :FSSC:-NEW-HIC                            PIC X(12).
           10  :FSSC:-PHY-SAN                            PIC X(1).
             88  :FSSC:-PHY-SAN-Y-SANCTIONED              VALUE 'Y'.
             88  :FSSC:-PHY-SAN-N-NOT-SANC                VALUE 'N'.
             88  :FSSC:-PHY-SAN-DEFAULT                   VALUE ' '.
           10  :FSSC:-MEDICAL-RECORD-NO                  PIC X(17).
           10  :FSSC:-AUTH-TABLE                  OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-AUTHX.
             15  :FSSC:-RELEASE-REASON                   PIC X(5).
             15  :FSSC:-REL-OPER-ID                      PIC X(9).
           10  :FSSC:-MP-RC-TABLE.
             15  :FSSC:-MP-RC-STATUS              OCCURS 10 TIMES
                                              INDEXED BY :FSSC:-MPRCX
                                                         PIC X(1).
           10  :FSSC:-LTR-RATE-BYPASS                    PIC X.
             88  :FSSC:-LTR-RATE-BYPASS-Y-YES             VALUE 'Y'.
             88  :FSSC:-LTR-RATE-BYPASS-DEFAU             VALUE ' '.
           10  :FSSC:-ZERO-REIMB-RATE-USED               PIC X(1).
             88  :FSSC:-ZERO-REIMB-RATE-Y-YES             VALUE 'Y'.
             88  :FSSC:-ZERO-REIMB-RATE-N-NO              VALUE 'N'.
           10  :FSSC:-BDL-CHK-OVR                        PIC X(1).
             88  :FSSC:-BDL-CHK-OVR-Y-YES                 VALUE 'Y'.
             88  :FSSC:-BDL-CHK-OVR-DEFAULT               VALUE ' '.
           10  :FSSC:-NOE-ACTION-CD                      PIC X(1).
             88  :FSSC:-NOE-ACT-2-DATE-CHANGE             VALUE '2'.
             88  :FSSC:-NOE-ACT-DEFAULT                   VALUE ' '.
           10  :FSSC:-HOLD-PRO-ID                        PIC X(5).
           10  :FSSC:-CODES-SWITCHES.
             15  :FSSC:-CASH-DED-OVRD-CD                 PIC X(1).
               88  :FSSC:-CASH-DED-A-MET                  VALUE 'A'.
               88  :FSSC:-CASH-DED-B-OVERRIDE             VALUE 'B'.
               88  :FSSC:-CASH-DED-NO-OVERRIDE            VALUE ' '.
             15  :FSSC:-HMO-OPTION-CD                    PIC X(1).
               88  :FSSC:-HMO-OPT-1-FI-PROCESS            VALUE '1'.
               88  :FSSC:-HMO-OPT-2-HMO-DIRECT            VALUE '2'.
               88  :FSSC:-HMO-OPT-A-FI-PROCESS            VALUE 'A'.
               88  :FSSC:-HMO-OPT-B-HMO-DIRECT            VALUE 'B'.
               88  :FSSC:-HMO-OPT-C-HMO-PROC              VALUE 'C'.
             15  :FSSC:-EMER-CARE-CD                     PIC X(1).
               88  :FSSC:-EMER-Y-EMERGENCY                VALUE 'Y'.
               88  :FSSC:-EMER-N-NON-EMERGENCY            VALUE 'N' ' '.
             15  :FSSC:-ESRD-REDUCT-SW                   PIC X(1).
               88  :FSSC:-ESRD-RED-P-PROC-CODE            VALUE 'P'.
               88  :FSSC:-ESRD-RED-R-REV-LINE             VALUE 'R'.
             15  :FSSC:-KRON-OVRD                        PIC X(1).
               88  :FSSC:-KRON-0-NO-EXTENSION             VALUE '0'.
               88  :FSSC:-KRON-1-BENEFIT                  VALUE '1'.
             15  :FSSC:-PRIOR-DRG-CD                     PIC X(3).
             15  :FSSC:-PRO-DRG-CD                       PIC X(3).
             15  :FSSC:-PRO-REVIEW-CD                    PIC X(2).
      ***         PRO REVIEW CODES ARE CONDITION CODES C1 THRU C7.
             15  :FSSC:-READMIT-7-DY                     PIC X(1).
               88  :FSSC:-READMIT-7-DAY-Y-YES             VALUE 'Y'.
               88  :FSSC:-READMIT-7-DAY-N-NO              VALUE 'N'.
               88  :FSSC:-READMIT-7-DAY-DEFAULT           VALUE ' '.
             15  :FSSC:-SNF-TRANSFER-CD                  PIC X(1).
               88  :FSSC:-SNF-TRANS-1-NO-BED              VALUE '1'.
               88  :FSSC:-SNF-TRANS-2-MED-NEC             VALUE '2'.
               88  :FSSC:-SNF-TRANS-3-READMIT             VALUE '3'.
               88  :FSSC:-SNF-TRANS-REQUIRE-MET           VALUE ' '.
           10  :FSSC:-INDICATORS.
             15  :FSSC:-AIR-THERAPY-IND                  PIC X(1).
FS7202         88  :FSSC:-AIR-THER-Y-ATTACH               VALUE 'Y'.    FS7202U1
FS7202         88  :FSSC:-AIR-THER-N-NO-ATTACH            VALUE 'N'.    FS7202U1
FS7202         88  :FSSC:-AIR-THER-NA                     VALUE ' '.    FS7202U1
             15  :FSSC:-CLAIM-REMARK-IND                 PIC X(1).
               88  :FSSC:-CLAIM-REM-Y-ATTACH              VALUE 'Y'.
               88  :FSSC:-CLAIM-REM-N-NO-ATTACH           VALUE 'N'.
               88  :FSSC:-CLAIM-REM-NA                    VALUE ' '.
             15  :FSSC:-CLM-TYP-IND                      PIC X(1).
FS7204         88  :FSSC:-CLM-TYP-A                       VALUE 'A'.
FS7204         88  :FSSC:-CLM-TYP-B                       VALUE 'B'.
FS7204         88  :FSSC:-CLM-TYP-C                       VALUE 'C'.
FS7204         88  :FSSC:-CLM-TYP-DEFAULT                 VALUE ' '.
             15  :FSSC:-GENER-HARDCOPY-IND               PIC X(1).
               88  :FSSC:-GENER-HC-2-MED-ADR              VALUE '2'.
               88  :FSSC:-GENER-HC-3-NON-M-ADR            VALUE '3'.
               88  :FSSC:-GENER-HC-4-MSP-ADR              VALUE '4'.
               88  :FSSC:-GENER-HC-5-COST-AVOID           VALUE '5'.
               88  :FSSC:-GENER-HC-7-BENE-ADR             VALUE '7'.
               88  :FSSC:-GENER-HC-8-MSN-LINE             VALUE '8'.
               88  :FSSC:-GENER-HC-9-MSN-CLAIM            VALUE '9'.
             15  :FSSC:-HMO-RLSE-CD                      PIC X(1).
               88  :FSSC:-HMO-RLSE-Y-YES                  VALUE 'Y'.
               88  :FSSC:-HMO-RLSE-NA                     VALUE ' '.
             15  :FSSC:-DME-ESRD-IND                     PIC X(1).
               88  :FSSC:-DME-ESRD-D-DME                  VALUE 'D'.
               88  :FSSC:-DME-ESRD-H-ESRD                 VALUE 'H'.
               88  :FSSC:-DME-ESRD-P-PNEUMONIA            VALUE 'P'.
             15  :FSSC:-BENE-SAV-ATTCH-IND               PIC X(1).
               88  :FSSC:-BENE-SAV-Y-PRESENT              VALUE 'Y'.
               88  :FSSC:-BENE-SAV-N-NOT-PRES             VALUE 'N' ' '.
             15  :FSSC:-ESRD-ATTCH-IND                   PIC X(1).
               88  :FSSC:-ESRD-ATT-R-NEEDED               VALUE 'R'.
               88  :FSSC:-ESRD-ATT-X-PRESENT              VALUE 'X'.
               88  :FSSC:-ESRD-ATT-Z-RTP-PRES             VALUE 'Z'.
               88  :FSSC:-ESRD-ATT-Y-NOT-NEEDED           VALUE 'Y'.
               88  :FSSC:-ESRD-ATT-NA                     VALUE ' '.
             15  :FSSC:-ERD-PAY-TYP-IND                  PIC X(1).
             15  :FSSC:-HME-HTLH-ATTCH-IND               PIC X(1).
               88  :FSSC:-HME-HTLH-ATTCH-Y-YES            VALUE 'Y'.
               88  :FSSC:-HME-HTLH-ATTCH-N-NO             VALUE 'N'.
               88  :FSSC:-HME-HTLH-ATTCH-NA               VALUE ' '.
             15  :FSSC:-MCE-BYP-IND                      PIC X(1).
               88  :FSSC:-MCE-BYP-Y-YES                   VALUE 'Y'.
             15  :FSSC:-MED-REC-ATTACH-IND               PIC X(1).
               88  :FSSC:-MED-REC-ATTACH-Y-YES            VALUE 'Y'.
               88  :FSSC:-MED-REC-ATTACH-N-NO             VALUE 'N'.
               88  :FSSC:-MED-REC-ATTACH-NA               VALUE ' '.
             15  :FSSC:-OP-REHAB-ATTCH-IND               PIC X(1).
               88  :FSSC:-OP-REHAB-ATTCH-Y-YES            VALUE 'Y'.
               88  :FSSC:-OP-REHAB-ATTCH-N-NO             VALUE 'N' ' '.
             15  :FSSC:-PACEMAKER-ATTCH-IND              PIC X(1).
               88  :FSSC:-PACEMAKER-ATTCH-Y-YES           VALUE 'Y'.
               88  :FSSC:-PACEMAKER-ATTCH-N-NO            VALUE 'N' ' '.
             15  :FSSC:-TIMELINESS-IND                   PIC X(1).
               88  :FSSC:-TIMELINESS-DONT-PAY             VALUE ' '.
               88  :FSSC:-TIMELINESS-N-PAY                VALUE 'N'.
               88  :FSSC:-TIMELINESS-R-OVERRIDE           VALUE 'R'.
               88  :FSSC:-TIMELINESS-Y-PAY                VALUE 'Y'.
             15  :FSSC:-SOLE-COMM-HOSP-IND               PIC X(1).
               88  :FSSC:-SOLE-COMM-HOSP-Y-YES            VALUE 'Y'.
               88  :FSSC:-SOLE-COMM-HOSP-N-NO             VALUE 'N'.
             15  :FSSC:-THERAPY-ATTACH-IND               PIC X(1).
               88  :FSSC:-THERAPY-ATTACH-Y-YES            VALUE 'Y'.
               88  :FSSC:-THERAPY-ATTACH-N-NO             VALUE 'N' ' '.
             15  :FSSC:-OUTLIER-RLSE-IND                 PIC X(1).
               88  :FSSC:-OUTLIER-RLSE-VALID              VALUES
                                        '0' '1' '2' '3' '4' '5' '6' '9'.
               88  :FSSC:-OUTLIER-RLSE-1-PPS-1            VALUE '1'.
               88  :FSSC:-OUTLIER-RLSE-2-PPS-2            VALUE '2'.
               88  :FSSC:-OUTLIER-RLSE-9-CC-66            VALUE '9'.
             15  :FSSC:-BTCH-AUD-UPDT-IND                PIC X(1).
             15  :FSSC:-PIP-IND                          PIC X(1).
               88  :FSSC:-PIP-N-NO                        VALUE 'N'.
               88  :FSSC:-PIP-Y-YES                       VALUE 'Y'.
             15  :FSSC:-UNIBIL-RIC                       PIC X(1).
               88  :FSSC:-UNIBIL-RIC-U-BOTH               VALUE 'U'.
               88  :FSSC:-UNIBIL-RIC-V-PARTA              VALUE 'V'.
               88  :FSSC:-UNIBIL-RIC-W-PARTB              VALUE 'W'.
               88  :FSSC:-UNIBIL-RIC-DEFAULT              VALUE ' '.
             15  :FSSC:-PROC-NEW-HIC-IND                 PIC X(1).
               88  :FSSC:-PROC-NEW-HIC-E-LOOP             VALUE 'E'.
               88  :FSSC:-PROC-NEW-HIC-Y-INCORR           VALUE 'Y'.
               88  :FSSC:-PROC-NEW-HIC-S-SAME             VALUE 'S'.
             15  :FSSC:-COMM-IND                         PIC X(1).
FS8161         88  :FSSC:-COMM-IND-ON-CLMB                VALUE 'Y'.
FS8161         88  :FSSC:-COMM-IND-CANCELS                VALUE 'X'.
FS8161         88  :FSSC:-COMM-IND-IDR-PHASE1             VALUE '*'.
             15  :FSSC:-THPY-INDICATORS.
               20  :FSSC:-THPY-PT-IND                    PIC X(1).
                 88  :FSSC:-THPY-PT-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-PT-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-PT-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-PT-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-PT-NA                    VALUE ' '.
               20  :FSSC:-THPY-OT-IND                    PIC X(1).
                 88  :FSSC:-THPY-OT-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-OT-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-OT-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-OT-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-OT-NA                    VALUE ' '.
               20  :FSSC:-THPY-ST-IND                    PIC X(1).
                 88  :FSSC:-THPY-ST-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-ST-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-ST-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-ST-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-ST-NA                    VALUE ' '.
               20  :FSSC:-THPY-RT-IND                    PIC X(1).
                 88  :FSSC:-THPY-RT-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-RT-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-RT-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-RT-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-RT-NA                    VALUE ' '.
               20  :FSSC:-THPY-CR-IND                    PIC X(1).
                 88  :FSSC:-THPY-CR-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-CR-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-CR-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-CR-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-CR-NA                    VALUE ' '.
               20  :FSSC:-THPY-PR-IND                    PIC X(1).
                 88  :FSSC:-THPY-PR-R-NEEDED              VALUE 'R'.
                 88  :FSSC:-THPY-PR-X-PRESENT             VALUE 'X'.
                 88  :FSSC:-THPY-PR-Z-RTP-PRES            VALUE 'Z'.
                 88  :FSSC:-THPY-PR-Y-NOT-NEEDED          VALUE 'Y'.
                 88  :FSSC:-THPY-PR-NA                    VALUE ' '.
             15  :FSSC:-UTIL-OVERRIDE-IND                PIC X(1).
               88  :FSSC:-UTIL-OVERRIDE-Y-YES             VALUE 'Y'.
               88  :FSSC:-UTIL-OVERRIDE-NO                VALUE ' '.
CR9788         88  :FSSC:-UTIL-OVERRIDE-N-NO              VALUE 'N'.
CR9788         88  :FSSC:-UTIL-OVERRIDE-B                 VALUE 'B'.
CR9788         88  :FSSC:-UTIL-OVERRIDE-VALID  VALUE 'Y' ' ' 'N' 'B'.
             15  :FSSC:-TERM-ILL-IND                     PIC X(1).
               88  :FSSC:-TERM-ILL-N-NO                   VALUE 'N'.
               88  :FSSC:-TERM-ILL-Y-YES                  VALUE 'Y'.
             15  :FSSC:-MED-POLICY-INDICATORS.
               20  :FSSC:-MR-HOSPICE-REDUCED             PIC X(1).
                 88  :FSSC:-MR-HOSPICE-RED-Y-YES          VALUE 'Y'.
                 88  :FSSC:-MR-HOSPICE-RED-N-NO           VALUE 'N'.
               20  :FSSC:-MR-HOSPICE-RO-REFRD            PIC X(1).
                 88  :FSSC:-MR-HOSPICE-RO-Y-YES           VALUE 'Y'.
                 88  :FSSC:-MR-HOSPICE-RO-NO              VALUE ' '.
               20  :FSSC:-MR-INCLD-IN-COMPOSIT           PIC X(1).
                 88  :FSSC:-MR-INCLD-IN-COM-Y-YES         VALUE 'Y'.
               20  :FSSC:-MR-URC-REVERSAL                PIC X(1).
                 88  :FSSC:-MR-URC-REVERS-P-PART          VALUE 'P'.
                 88  :FSSC:-MR-URC-REVERS-F-FULL          VALUE 'F'.
               20  :FSSC:-MR-DEMAND-REVERSAL             PIC X(1).
                 88  :FSSC:-MR-DEMAND-REV-P-PART          VALUE 'P'.
                 88  :FSSC:-MR-DEMAND-REV-F-FULL          VALUE 'F'.
             15  :FSSC:-REJ-RSN-OVERRIDE                 PIC X(1).
           10  :FSSC:-VALUE-CODE-TABLE.
             15  :FSSC:-VALUE-CODE-AMT            OCCURS 36 TIMES
                                            INDEXED BY :FSSC:-VCNDX.
               20  :FSSC:-VAL-CD                         PIC X(2).
               20  :FSSC:-UB04-FILLER-F22                PIC X(1).
               20  :FSSC:-UB04-FILLER-F23                PIC X(2).
               20  :FSSC:-VAL-AMT                 COMP-3 PIC S9(7)V99.
               20  :FSSC:-VAL-ANSI-GROUPS.
                 25  :FSSC:-VAL-ANSI-GRP                 PIC X(2).
                 25  :FSSC:-VAL-ANSI-RSN                 PIC X(3).
           10  :FSSC:-OCCUR-CD-TABLE.
             15  :FSSC:-OCCUR-CD-DT               OCCURS 30 TIMES
                                            INDEXED BY :FSSC:-OCNDX.
               20  :FSSC:-OCCUR-CD                       PIC X(2).
               20  :FSSC:-UB04-FILLER-F24                PIC X(1).
               20  :FSSC:-OCCUR-DT-CYMD.
                 25  :FSSC:-OCCUR-DT-CC                  PIC 9(2).
                 25  :FSSC:-OCCUR-DT.
                   30  :FSSC:-OCCUR-YR                   PIC 9(2).
                   30  :FSSC:-OCCUR-MO                   PIC 9(2).
                   30  :FSSC:-OCCUR-DY                   PIC 9(2).
           10  :FSSC:-PROC-CODE-TABLE.
             15  :FSSC:-PROC-CODES-DATES          OCCURS 25 TIMES
                                            INDEXED BY :FSSC:-PCNDX.
               20  :FSSC:-PROC-CD-DATA.
                 25  :FSSC:-PROC-CD                      PIC X(7).
                 25  :FSSC:-UB04-FILLER-F25              PIC X(1).
               20  :FSSC:-PROC-FLAG                      PIC X(4).
               20  :FSSC:-PROC-DT-CYMD.
                 25  :FSSC:-PROC-DT-CC                   PIC 9(2).
                 25  :FSSC:-PROC-DT.
                   30  :FSSC:-PROC-YR                    PIC 9(2).
                   30  :FSSC:-PROC-MO                    PIC 9(2).
                   30  :FSSC:-PROC-DY                    PIC 9(2).
           10  :FSSC:-OCCUR-SPAN-CD-TABLE.
             15  :FSSC:-OCCUR-SPAN-CD-DT          OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-OSNDX.
               20 :FSSC:-OCCUR-SPAN-CD                   PIC X(2).
               20 :FSSC:-UB04-FILLER-F26                 PIC X(1).
               20  :FSSC:-OCUR-SPAN-FRM-DT-CYMD.
                 25  :FSSC:-OCCUR-SPAN-FROM-DT-CC        PIC 9(2).
                 25  :FSSC:-OCCUR-SPAN-FROM-DT.
                   30  :FSSC:-OCCUR-SPAN-FROM-YR         PIC 9(2).
                   30  :FSSC:-OCCUR-SPAN-FROM-MO         PIC 9(2).
                   30  :FSSC:-OCCUR-SPAN-FROM-DY         PIC 9(2).
               20  :FSSC:-OCCUR-SPAN-TO-DT-CYMD.
                 25  :FSSC:-OCCUR-SPAN-TO-DT-CC          PIC 9(2).
                 25  :FSSC:-OCCUR-SPAN-TO-DT.
                   30  :FSSC:-OCCUR-SPAN-TO-YR           PIC 9(2).
                   30  :FSSC:-OCCUR-SPAN-TO-MO           PIC 9(2).
                   30  :FSSC:-OCCUR-SPAN-TO-DY           PIC 9(2).
           10  :FSSC:-ANSI-DATA.
             15  :FSSC:-ADJ-CODE                         PIC X(3).
             15  :FSSC:-GROUP-CODE                       PIC X(2).
FS0145         88  :FSSC:-GROUP-CO-CNTRCT-OBLIG           VALUE 'CO'.
FS0145         88  :FSSC:-GROUP-CR-CORRECTION             VALUE 'CR'.
FS0145         88  :FSSC:-GROUP-OA-OTHER-ADJ              VALUE 'OA'.
FS0145         88  :FSSC:-GROUP-PR-PATIENT-RESP           VALUE 'PR'.
             15  :FSSC:-ANSI-APPEAL-CODES         OCCURS 5 TIMES.
               20  :FSSC:-APPEAL-CODE                    PIC X(5).
           10  :FSSC:-INFLUENZA-HCPC-IND                 PIC X(1).
               88  :FSSC:-INFLUENZA-HCPC-H-HEPB           VALUE 'H'.
               88  :FSSC:-INFLUENZA-HCPC-P-HEPO           VALUE 'P'.
               88  :FSSC:-INFLUENZA-HCPC-Y-FLU            VALUE 'Y'.
           10  :FSSC:-OTHER-THERAPIES.
             15  :FSSC:-THPY-PS-IND                      PIC X(1).
               88  :FSSC:-THPY-PS-R-NEEDED                VALUE 'R'.
               88  :FSSC:-THPY-PS-X-PRESENT               VALUE 'X'.
               88  :FSSC:-THPY-PS-Z-RTP-PRES              VALUE 'Z'.
               88  :FSSC:-THPY-PS-Y-NOT-NEEDED            VALUE 'Y'.
               88  :FSSC:-THPY-PS-NA                      VALUE ' '.
             15  :FSSC:-THPY-SN-IND                      PIC X(1).
               88  :FSSC:-THPY-SN-R-NEEDED                VALUE 'R'.
               88  :FSSC:-THPY-SN-X-PRESENT               VALUE 'X'.
               88  :FSSC:-THPY-SN-Z-RTP-PRES              VALUE 'Z'.
               88  :FSSC:-THPY-SN-Y-NOT-NEEDED            VALUE 'Y'.
               88  :FSSC:-THPY-SN-NA                      VALUE ' '.
             15  :FSSC:-THPY-MS-IND                      PIC X(1).
               88  :FSSC:-THPY-MS-R-NEEDED                VALUE 'R'.
               88  :FSSC:-THPY-MS-X-PRESENT               VALUE 'X'.
               88  :FSSC:-THPY-MS-Z-RTP-PRES              VALUE 'Z'.
               88  :FSSC:-THPY-MS-Y-NOT-NEEDED            VALUE 'Y'.
               88  :FSSC:-THPY-MS-NA                      VALUE ' '.
           10  :FSSC:-CWF-MSP-CD                         PIC X(2).
CR8677         88  :FSSC:-CWF-MSP-12-WORKING              VALUE '12'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-13-ESRD                 VALUE '13'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-14-AUTO                 VALUE '14'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-15-WORKER               VALUE '15'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-16-FED                  VALUE '16'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-41-BLACK-L              VALUE '41'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-42-VA                   VALUE '42'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-43-DISABLED             VALUE '43'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-47-LIABILITY            VALUE '47'.   CR8677U2
CR8677         88  :FSSC:-CWF-MSP-DEFAULT                 VALUE '  '.   CR8677U2
C10126     10  FILLER                                    PIC X(2).
           10  :FSSC:-LCC-MSP-IND                        PIC X(1).
               88  :FSSC:-LCC-MSP-NA                      VALUE ' '.
               88  :FSSC:-LCC-MSP-M-PSR-REVIEW            VALUE 'M'.
           10  :FSSC:-PPS-DEMO-IND                       PIC X(1).
             88  :FSSC:-PPS-DEMO-D-DAYCARE                VALUE 'D'.
             88  :FSSC:-PPS-DEMO-L-LOW-VISION             VALUE 'L'.
             88  :FSSC:-PPS-DEMO-P-LAB                    VALUE 'P'.
             88  :FSSC:-PPS-DEMO-T-FREQ-HEMO              VALUE 'T'.
             88  :FSSC:-PPS-DEMO-NA                       VALUE ' '.
           10  :FSSC:-LEWIN-IND                          PIC X(1).
           10  :FSSC:-ASC-RTC                            PIC X(4).
           10  :FSSC:-EMC-RT30-PAYER-ID           OCCURS 3 TIMES.
             15  :FSSC:-RT30-PAYER-ID                    PIC X(9).
           10  :FSSC:-DEMO-CD.
             15  :FSSC:-DEMO-NUM                         PIC X(2).
             15  :FSSC:-DEMO-FLAG                        PIC X(1).
CR9415         88  :FSSC:-DEMO-PRESENT                    VALUE 'Y'.
CR9415         88  :FSSC:-DEMO-NOT-PRESENT                VALUE 'N'.
           10  :FSSC:-MAF-DEMO-IND                       PIC X(1).
           10  :FSSC:-BENE-SAVINGS-DATA.
             15  :FSSC:-BSVS-FORCE-CODE                  PIC X.
               88  :FSSC:-BSVS-FORCE-ON                   VALUE 'F'.
             15  :FSSC:-BSVS-PROV-WAIVER-IND             PIC X.
               88  :FSSC:-BSVS-ON-WAIVER                  VALUE 'Y'.
               88  :FSSC:-BSVS-OFF-WAIVER                 VALUE 'N'.
           10  :FSSC:-ERROR-CLAIM-IND                    PIC X(1).
           10  :FSSC:-REV-WAIVER-IND                     PIC X(1).
             88  :FSSC:-REV-WAIVER-M-INPAT                VALUE 'M'.
             88  :FSSC:-REV-WAIVER-Y-OUTPAT               VALUE 'Y'.
           10  :FSSC:-CROSSOVER-PLANCODE                 PIC X(3).
           10  :FSSC:-CROSSOVER-IND                      PIC X(1).
             88  :FSSC:-CROSSOVER-1-PRIMARY               VALUE '1'.
             88  :FSSC:-CROSSOVER-2-SECONDARY             VALUE '2'.
             88  :FSSC:-CROSSOVER-3-TERTIARY              VALUE '3'.
           10  :FSSC:-ALT-PAY-SCHED-IND                  PIC X(1).
             88  :FSSC:-ALT-PAY-SCHED-Y-YES               VALUE 'Y'.
             88  :FSSC:-ALT-PAY-SCHED-N-NO                VALUE 'N' ' '.
           10  :FSSC:-ATTEND-PHYS-ID                     PIC X(16).
           10  :FSSC:-ATTEND-PHYS-UPIN                 REDEFINES
               :FSSC:-ATTEND-PHYS-ID.
             15  :FSSC:-ATTEND-PHYS-UPIN-NUM             PIC X(6).
             15  :FSSC:-ATTEND-PHYS-NPI-NUM              PIC 9(10).
           10  :FSSC:-UB04-FILLER-F27                    PIC X(2).
           10  :FSSC:-UB04-FILLER-F28                    PIC X(9).
           10  :FSSC:-ATTEND-PHYS-NAME.
             15  :FSSC:-ATTEND-PHYS-LNAME                PIC X(17).
             15  :FSSC:-ATTEND-PHYS-FNAME                PIC X(8).
             15  :FSSC:-UB04-FILLER-F29                  PIC X(4).
             15  :FSSC:-ATTEND-PHYS-MINT                 PIC X(1).
           10  :FSSC:-ATTEND-PHYS-FLAG                   PIC X(1).
             88  :FSSC:-ATTEND-PHYS-N-NO                  VALUE 'N'.
           10  :FSSC:-OPERATING-PHYS-ID                  PIC X(16).
           10  :FSSC:-OPERATING-PHYS-UPIN              REDEFINES
               :FSSC:-OPERATING-PHYS-ID.
             15  :FSSC:-OPER-PHYS-UPIN-NUM               PIC X(6).
             15  :FSSC:-OPER-PHYS-NPI-NUM                PIC 9(10).
           10  :FSSC:-UB04-FILLER-F30                    PIC X(2).
           10  :FSSC:-UB04-FILLER-F31                    PIC X(9).
           10  :FSSC:-OPER-PHYS-NAME.
             15  :FSSC:-OPER-PHYS-LNAME                  PIC X(17).
             15  :FSSC:-OPER-PHYS-FNAME                  PIC X(8).
             15  :FSSC:-UB04-FILLER-F32                  PIC X(4).
             15  :FSSC:-OPER-PHYS-MINT                   PIC X(1).
           10  :FSSC:-OPER-PHYS-FLAG                     PIC X(1).
             88  :FSSC:-OPER-PHYS-N-NO                    VALUE 'N'.
           10  :FSSC:-OTH-PHYS-ID                        PIC X(16).
           10  :FSSC:-OTH-PHYS-UPIN                    REDEFINES
               :FSSC:-OTH-PHYS-ID.
             15  :FSSC:-OTH-PHYS-UPIN-NUM                PIC X(6).
             15  :FSSC:-OTH-PHYS-NPI-NUM                 PIC 9(10).
           10  :FSSC:-UB04-FILLER-F33                    PIC X(2).
           10  :FSSC:-UB04-FILLER-F34                    PIC X(9).
           10  :FSSC:-OTH-PHYS-NAME.
             15  :FSSC:-OTH-PHYS-LNAME                   PIC X(17).
             15  :FSSC:-OTH-PHYS-FNAME                   PIC X(8).
             15  :FSSC:-UB04-FILLER-F35                  PIC X(4).
             15  :FSSC:-OTH-PHYS-MINT                    PIC X(1).
           10  :FSSC:-OTH-PHYS-FLAG                      PIC X(1).
           10  :FSSC:-OT2-PHYS-ID                        PIC X(16).
           10  :FSSC:-OT2-PHYS-UPIN                    REDEFINES
                  :FSSC:-OT2-PHYS-ID.
             15  :FSSC:-OT2-PHYS-NO                      PIC X(6).
             15  :FSSC:-OT2-NPI-NUM                      PIC 9(10).
           10  :FSSC:-UB04-FILLER-F36                    PIC X(2).
           10  :FSSC:-UB04-FILLER-F37                    PIC X(9).
           10  :FSSC:-OT2-PHYS-NAME.
             15  :FSSC:-OT2-PHYS-LNAME                   PIC X(17).
             15  :FSSC:-OT2-PHYS-FNAME                   PIC X(8).
             15  :FSSC:-UB04-FILLER-F38                  PIC X(4).
             15  :FSSC:-OT2-PHYS-MINIT                   PIC X(1).
           10  :FSSC:-OT2-PHYS-FLAG                      PIC X(1).
           10  :FSSC:-PAT-FILED-BILL-CODE                PIC X(1).
             88  :FSSC:-PAT-FILED-BILL-E-EMER             VALUE 'E'.
           10  :FSSC:-EXT-INJURY-FLAG                    PIC X(4).
           10  :FSSC:-EXT-INJURY-DIAGNOSIS.
             15  :FSSC:-EXT-INJURY-DIAG-CODE             PIC X(7).
             15  :FSSC:-ICD10-FILLER-F4                  PIC X(1).
           10  :FSSC:-EXT-INJURY-DIAG-TAB     OCCURS 11 TIMES.
             15  :FSSC:-EXT-INJURY-DIAG-CD-T             PIC X(7).
             15  :FSSC:-ICD10-FILLER-F4-T                PIC X(1).
           10  :FSSC:-OLD-BSVS-IND                       PIC X(1).
           10  :FSSC:-CONTR-CLAIM-FIELDS.
             15  :FSSC:-CONTR-CLM-REASON                 PIC X(5).
             15  :FSSC:-CONTR-CLM-ANSI-INFO.
               20  :FSSC:-CONTR-CLM-ANSI-GROUPS.
                 25  :FSSC:-CONTR-CLM-ANSI-GRP           PIC X(2).
                 25  :FSSC:-CONTR-CLM-ANSI-RSN           PIC X(3).
           10  :FSSC:-REIMB-PAT-ANSI-INFO.
             15  :FSSC:-REIMB-PAT-ANSI-GROUPS.
               20  :FSSC:-REIMB-PAT-ANSI-GRP             PIC X(2).
               20  :FSSC:-REIMB-PAT-ANSI-RSN             PIC X(3).
           10  :FSSC:-FIXED-FUTURE2                      PIC X(62).
           10  :FSSC:-EMC-REC21-EMPLYR-INFX       OCCURS 4 TIMES.
             15  :FSSC:-EMC-R21-EMPLYR-NAME              PIC X(24).
             15  :FSSC:-EMC-R21-EMPLYR-ADDR              PIC X(18).
             15  :FSSC:-EMC-R21-EMPLYR-CITY              PIC X(15).
             15  :FSSC:-EMC-R21-EMPLYR-STATE             PIC X(2).
             15  :FSSC:-EMC-R21-EMPLYR-ZIP               PIC X(9).
           10  :FSSC:-EMC-RECEIVED-REC22          OCCURS 3 TIMES.
             15  :FSSC:-EMC-R22-STATE-CODE               PIC X(2).
             15  :FSSC:-EMC-R22-FORM-LOC2U               PIC X(29).
             15  :FSSC:-EMC-R22-FORM-LOC2L               PIC X(30).
             15  :FSSC:-EMC-R22-FORM-LOC11U              PIC X(12).
             15  :FSSC:-EMC-R22-FORM-LOC11L              PIC X(13).
             15  :FSSC:-EMC-R22-FORM-LOC56U              PIC X(13).
             15  :FSSC:-EMC-R22-FORM-LOC56L2             PIC X(14).
             15  :FSSC:-EMC-R22-FORM-LOC56L3             PIC X(14).
             15  :FSSC:-EMC-R22-FORM-LOC56L4             PIC X(14).
             15  :FSSC:-EMC-R22-FORM-LOC56PL             PIC X(14).
             15  :FSSC:-EMC-R22-FORM-LOC78U              PIC X(2).
             15  :FSSC:-EMC-R22-FORM-LOC78L              PIC X(3).
             15  :FSSC:-EMC-R22-LOCAL-USE                PIC X(8).
           10  :FSSC:-EMC-REC31-INSURED           OCCURS 3 TIMES.
             15  :FSSC:-EMC-R31-INSURED-ADDR1            PIC X(18).
             15  :FSSC:-EMC-R31-INSURED-ADDR2            PIC X(18).
             15  :FSSC:-EMC-R31-INSURED-CITY             PIC X(15).
             15  :FSSC:-EMC-R31-INSURED-STATE            PIC X(2).
             15  :FSSC:-EMC-R31-INSURED-ZIP              PIC X(9).
           10  :FSSC:-MSP-ADDITIONAL-INFO         OCCURS 2 TIMES.
             15  :FSSC:-INSURERS-ADDR1                   PIC X(32).
             15  :FSSC:-INSURERS-ADDR2                   PIC X(32).
             15  :FSSC:-INSURERS-CITY                    PIC X(15).
             15  :FSSC:-INSURERS-ST                      PIC X(2).
             15  :FSSC:-INSURERS-ZIP.
               20  :FSSC:-INSURERS-ZIP-5                 PIC 9(5).
               20  :FSSC:-INSURERS-ZIP-4                 PIC 9(4).
           10  :FSSC:-EMC-REC32-PAYER-INFO        OCCURS 3 TIMES.
             15  :FSSC:-EMC-R32-PAYER-NAME               PIC X(25).
             15  :FSSC:-EMC-R32-PAYER-ADDR1              PIC X(18).
             15  :FSSC:-EMC-R32-PAYER-ADDR2              PIC X(18).
             15  :FSSC:-EMC-R32-PAYER-CITY               PIC X(15).
             15  :FSSC:-EMC-R32-PAYER-STATE              PIC X(2).
             15  :FSSC:-EMC-R32-PAYER-ZIP                PIC X(9).
           10  :FSSC:-REC41-DATA                  OCCURS 3 TIMES.
             15  :FSSC:-EMC-REC41-FORM-LOC31U            PIC X(5).
             15  :FSSC:-EMC-REC41-FORM-LOC31L            PIC X(6).
           10  :FSSC:-EMC-REC70-FORM-LOC57               PIC X(27).
           10  :FSSC:-EMC-REC90-REMARKS.
             15  :FSSC:-EMC-R90-REMARKS                  PIC X(110).
             15  :FSSC:-EMC-R91-REMARKS                  PIC X(82).
           10  :FSSC:-REM-60                    REDEFINES
               :FSSC:-EMC-REC90-REMARKS.
             15  :FSSC:-EMC-R90-60-REMARKS               PIC X(105).
             15  :FSSC:-EMC-R91-60-REMARKS               PIC X(87).
           10  :FSSC:-FED-TAX-NB                         PIC X(10).
           10  :FSSC:-HMO-ID-CD                          PIC X(5).
           10  :FSSC:-CHOICES-CONTRCTR          REDEFINES
                   :FSSC:-HMO-ID-CD                      PIC X(5).
           10  :FSSC:-CONTRCTR-DATA               OCCURS 3 TIMES.
             15  :FSSC:-CONTRCTR-ID                      PIC X(5).
           10  :FSSC:-HMO-ID-FUTURE                      PIC X(10).
           10  :FSSC:-HMO-AUTH-NBR                       PIC X(16).
           10  :FSSC:-PRO-CTL-NBR                        PIC X(12).
           10  :FSSC:-FED-TAX-NB-SUB                     PIC X(4).
           10  :FSSC:-EMC-VERSION-CODE                   PIC X(3).
           10  :FSSC:-MEDICAID-NB                        PIC X(13).
           10  :FSSC:-PAYER-ID-IND-FIELD          OCCURS 3 TIMES.
             15  :FSSC:-PAYER-ID-IND                     PIC X(2).
           10  :FSSC:-RTP-IND                            PIC X(1).
           10  :FSSC:-ADJ-SHELL-DCN                      PIC X(23).
           10  :FSSC:-TRANS-HOSPICE-PROV                 PIC X(13).
           10  :FSSC:-PAYER-CODE-FIELD            OCCURS 3 TIMES.
             15  :FSSC:-PAYER-CODE                       PIC X(1).
               88  :FSSC:-PAYER-1-MEDICAID                VALUE '1'.
               88  :FSSC:-PAYER-2-BLUE-CROSS              VALUE '2'.
               88  :FSSC:-PAYER-3-OTHER                   VALUE '3'.
               88  :FSSC:-PAYER-4-NONE                    VALUE '4'.
               88  :FSSC:-PAYER-A-WORKING-AGE             VALUE 'A'.
               88  :FSSC:-PAYER-B-ESRD-W-EGHP             VALUE 'B'.
               88  :FSSC:-PAYER-C-CONDITIONAL             VALUE 'C'.
               88  :FSSC:-PAYER-D-AUTO                    VALUE 'D'.
               88  :FSSC:-PAYER-E-WORKER-COMP             VALUE 'E'.
               88  :FSSC:-PAYER-F-FED-AGENCY              VALUE 'F'.
               88  :FSSC:-PAYER-G-DISABLED                VALUE 'G'.
               88  :FSSC:-PAYER-H-BLACK-LUNG              VALUE 'H'.
               88  :FSSC:-PAYER-I-VA                      VALUE 'I'.
               88  :FSSC:-PAYER-L-LIABILITY               VALUE 'L'.
               88  :FSSC:-PAYER-Z-MEDICARE                VALUE 'Z'.
           10  :FSSC:-SUBMITTER-EIN                      PIC X(10).
           10  :FSSC:-NEW-CLAIM-IND                      PIC X(1).
             88  :FSSC:-NEW-CLAIM-Y-YES-PROC              VALUE 'Y'.
             88  :FSSC:-NEW-CLAIM-DONT-PROC               VALUE ' '.
           10  :FSSC:-RECON-USER-ACT                     PIC X(1).
CR9017       88  :FSSC:-RECON-UA-P-PARTIAL                VALUE 'P'.    CR9017A
CR9017       88  :FSSC:-RECON-UA-R-REVIEWED               VALUE 'R'.    CR9017A
           10  :FSSC:-MSP-APPORTION-SW                   PIC X(1).
             88  :FSSC:-MSP-APPOR-YES                     VALUE ' '.
             88  :FSSC:-MSP-APPOR-A-OTAF-ONLY             VALUE 'A'.
             88  :FSSC:-MSP-APPOR-N-NO                    VALUE 'N'.
             88  :FSSC:-MSP-APPOR-O-NO-OTAF               VALUE 'O'.
           10  :FSSC:-OCE-MR-REAS-TAB.
             15  :FSSC:-OCE-MR-REAS      OCCURS 15 TIMES
                                         INDEXED BY :FSSC:-OCEMRX
                                                         PIC X(3).
           10  :FSSC:-OCE-MED-REV-IND                    PIC X(1).
             88  :FSSC:-OCE-MED-REV-N-NO                  VALUE 'N'.
             88  :FSSC:-OCE-MED-REV-Y-YES                 VALUE 'Y'.
           10  :FSSC:-ERROR-TRAP.
             15  :FSSC:-IO-STATUS                        PIC X(2).
             15  :FSSC:-LINE-REAS-ERROR-NUM              PIC X(3).
           10  :FSSC:-MR-FLAG                            PIC X(1).
             88  :FSSC:-MR-DONT-SUSPEND                   VALUE ' '.
             88  :FSSC:-MR-Y-YES-SUSPEND                  VALUE 'Y'.
           10  :FSSC:-REPOS-HIC                          PIC X(12).
           10  :FSSC:-REPOS-IND                          PIC X(1).
           10  :FSSC:-CERT-CL-MR-IND                     PIC X(1).
             88  :FSSC:-CERT-CL-MR-DEFAULT                VALUE ' '.
             88  :FSSC:-CERT-CL-MR-N-NOT-RECD             VALUE 'N'.
             88  :FSSC:-CERT-CL-MR-Y-REVIEWED             VALUE 'Y'.
           10  :FSSC:-INSURED-ALPHA-TABLE         OCCURS  3 TIMES.
             15  :FSSC:-INSURED-SEX                      PIC X(1).
               88  :FSSC:-INSURED-SEX-F-FEMALE            VALUE 'F'.
               88  :FSSC:-INSURED-SEX-M-MALE              VALUE 'M'.
               88  :FSSC:-INSURED-SEX-U-UNKNOWN           VALUE 'U'.
           10  :FSSC:-PVDR-BENE-PAID-IND                 PIC X(1).
             88  :FSSC:-PVDR-BENE-PAID-B-BENE             VALUE 'B'.
             88  :FSSC:-PVDR-BENE-PAID-P-PROV             VALUE 'P'.
             88  :FSSC:-PVDR-BENE-PAID-X-BOTH             VALUE 'X'.
           10  FILLER                                    PIC X(4).
           10  :FSSC:-SFSCINFO-DATA.
             15  :FSSC:-SFSCINFO-CK-REMIT-NBR            PIC X(10).
             15  :FSSC:-SFSCINFO-CK-REMIT-DTE            PIC X(8).
           10  :FSSC:-MSPPAYPS-IND                       PIC X(1).
           10  :FSSC:-COB-TRD-PRTNR-TABLE.
             15  :FSSC:-COB-TRD-PRTNR-ITEM        OCCURS 10
                                            INDEXED BY :FSSC:-COBTP.
               20  :FSSC:-COB-TRD-PRTNR-TID-IND          PIC X(1).
                 88  :FSSC:-COB-TRD-PRTNR-CROSSED           VALUE ' '.
                 88  :FSSC:-COB-TRD-PRTNR-ERROR             VALUE 'N'.
FS7372           88  :FSSC:-COB-TRD-PRTNR-RECR-R            VALUE 'R'.
FS7372           88  :FSSC:-COB-TRD-PRTNR-RECR              VALUE 'S'
FS7372                                                            'X'.
FS7372         20  :FSSC:-COB-TRD-PRTNR-TID.
FS7372             25  FILLER                            PIC X(04).
FS7372             25  :FSSC:-COB-TRD-PRTNR-TID-5        PIC X(05).
           10  :FSSC:-ALIEN-BENE                         PIC X(1).
             88  :FSSC:-ALIEN-BENE-NO-OVERRID             VALUE ' '.
             88  :FSSC:-ALIEN-BENE-Y-OVERRIDE             VALUE 'Y'.
           10  :FSSC:-PSR-INFO.
             15  :FSSC:-PSR-TQ-PRESENT                   PIC X(1).
             15  :FSSC:-PSR-PASS-IND                     PIC X(1).
             15  :FSSC:-PSR-FILLER                       PIC X(6).
           10  :FSSC:-INSURED-REL-X12-TABLE       OCCURS 3 TIMES.
             15  :FSSC:-INSURED-REL-X12-DATA.
               20  :FSSC:-INSURED-REL-X12                PIC X(2).
FS7350             88  :FSSC:-INS-X12-01-SPOUSE           VALUE '01'.
FS7350             88  :FSSC:-INS-X12-04-GRANDPAREN       VALUE '04'.
FS7350             88  :FSSC:-INS-X12-05-GRANDCHILD       VALUE '05'.
FS7350             88  :FSSC:-INS-X12-07-NIECE-NEPH       VALUE '07'.
FS7350             88  :FSSC:-INS-X12-10-FOSTER           VALUE '10'.
FS7350             88  :FSSC:-INS-X12-15-WARD-COURT       VALUE '15'.
FS7350             88  :FSSC:-INS-X12-17-STEP-CHILD       VALUE '17'.
FS7350             88  :FSSC:-INS-X12-18-SELF             VALUE '18'.
FS7350             88  :FSSC:-INS-X12-19-CHILD            VALUE '19'.
FS7350             88  :FSSC:-INS-X12-20-EMPLOYEE         VALUE '20'.
FS7350             88  :FSSC:-INS-X12-21-UNKNOWN          VALUE '21'.
FS7350             88  :FSSC:-INS-X12-22-HANDICAP         VALUE '22'.
FS7350             88  :FSSC:-INS-X12-23-SPONSORED        VALUE '23'.
FS7350             88  :FSSC:-INS-X12-24-MINOR            VALUE '24'.
FS7350             88  :FSSC:-INS-X12-29-SIG-OTHER        VALUE '29'.
FS7350             88  :FSSC:-INS-X12-32-MOTHER           VALUE '32'.
FS7350             88  :FSSC:-INS-X12-33-FATHER           VALUE '33'.
FS7350             88  :FSSC:-INS-X12-36-EMANCIPATE       VALUE '36'.
FS7350             88  :FSSC:-INS-X12-39-ORGAN-DON        VALUE '39'.
FS7350             88  :FSSC:-INS-X12-40-CADAV-DON        VALUE '40'.
FS7350             88  :FSSC:-INS-X12-41-INJURED          VALUE '41'.
FS7350             88  :FSSC:-INS-X12-43-NOT-RESP         VALUE '43'.
FS7350             88  :FSSC:-INS-X12-53-LIFE-PARTN       VALUE '53'.
FS7350             88  :FSSC:-INS-X12-G8-OTHER            VALUE 'G8'.
               20  :FSSC:-UB04-FILLER-F40                PIC X(1).
             15  :FSSC:-BENE-REL-X12-DATA       REDEFINES
                       :FSSC:-INSURED-REL-X12-DATA.
               20  :FSSC:-BENE-REL-X12                   PIC X(2).
               20  FILLER                                PIC X(1).
           10  :FSSC:-MPP-CONTROL-NUMBERS.
             15  :FSSC:-MPP-CONTROL-NO            OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-MPPCX
                                                         PIC X(4).
           10  :FSSC:-BUSINESS-SEGMENT                   PIC X(4).
           10  :FSSC:-CBSA-LOC                           PIC X(5).
           10  :FSSC:-MPP-REASON-CODES.
             15  :FSSC:-MPP-REASON-CODE           OCCURS 10 TIMES
                                            INDEXED BY :FSSC:-MPPRX
                                                         PIC X(5).
           10  :FSSC:-CWF-CLAIM-OVERRIDES.
             15  :FSSC:-CWF-CLMOVRIDE           OCCURS 5 PIC X(5).
           10  :FSSC:-OS-CLINIC-ZIP-CD                   PIC X(5).
           10  :FSSC:-NPI-NUMBER-SUBMIT                  PIC 9(10).
           10  :FSSC:-OSCAR-IDENTIFIER                   PIC X(1).
             88  :FSSC:-OSCAR-N-NO-PREFER-NPI             VALUE 'N' ' '.
             88  :FSSC:-OSCAR-Y-PREFERRED-NPI             VALUE 'Y'.
           10  :FSSC:-TAXO-CODE                          PIC X(10).
           10  :FSSC:-FACILITY-ZIP.
             15  :FSSC:-FAC-ZIP-5                        PIC 9(05).
             15  :FSSC:-FAC-ZIP-4                        PIC 9(04).
           10  :FSSC:-MASS-ADJ-IND                       PIC X(1).
             88  :FSSC:-MASS-ADJ-M-FEE-SCHED              VALUE 'M'.
             88  :FSSC:-MASS-ADJ-O-OTHER                  VALUE 'O'.
             88  :FSSC:-MASS-ADJ-NA                       VALUE ' '.
           10  :FSSC:-ADJ-CLMS-IND                       PIC X(1).
             88  :FSSC:-ADJ-CLMS-A-CWF-ACT-1              VALUE 'A'.
FS0231       88  :FSSC:-ADJ-CLMS-N-CWF                    VALUE 'N'.
             88  :FSSC:-ADJ-CLMS-NA                       VALUE ' '.
           10  :FSSC:-BENE-LIAB-IND                      PIC X(1).
             88  :FSSC:-BENE-LIAB-L-GROUP-PR              VALUE 'L'.
             88  :FSSC:-BENE-LIAB-N-NO-GRP-PR             VALUE 'N'.
             88  :FSSC:-BENE-LIAB-NO-FULL-DEN             VALUE ' '.
           10  :FSSC:-ESRD-WADJ-RATE                     PIC 9(4)V9(2).
           10  :FSSC:-INTEREST-DAYS-BENE          COMP-3 PIC S9(9).
           10  :FSSC:-CBSA-SPCL-WAGE-INDEX               PIC 9(2)V9(4).
           10  :FSSC:-PREV-XACT-DT-CYMD.
             15  :FSSC:-PREV-XACT-DT-CC                  PIC 9(2).
             15  :FSSC:-PREV-XACT-DT.
               20  :FSSC:-PREV-XACT-DT-YR                PIC 9(2).
               20  :FSSC:-PREV-XACT-DT-MO                PIC 9(2).
               20  :FSSC:-PREV-XACT-DT-DY                PIC 9(2).
           10  :FSSC:-INTEREST-AMT                COMP-3 PIC S9(9)V99.
           10  :FSSC:-INTEREST-DAYS               COMP-3 PIC S9(9).
           10  :FSSC:-INTEREST-RATE               COMP-3 PIC S9(4)V9(5).
           10  :FSSC:-DISC-MIN                           PIC 9(02).
             88  :FSSC:-DISC-MIN-00-60-VALID              VALUES
                                                             00 THRU 60.
           10  :FSSC:-INSURED-NUMERIC-TABLE       OCCURS 3 TIMES.
             15  :FSSC:-INSURED-DOB                      PIC 9(08).
           10  :FSSC:-CWF-TRLR3-OCC-SELECT               PIC 9(2).
           10  :FSSC:-UB04-FILLER-F45                    PIC 9(2).
           10  :FSSC:-HHREV-SUM-1-3-QTY-THR              PIC 9(5).
           10  :FSSC:-HHREV-SUM-1-6-QTY-ALL              PIC 9(5).
           10  :FSSC:-REDUCED-COIN-TOT            COMP-3 PIC S9(7)V99.
           10  :FSSC:-CONTR-CLM-NCOV-CHRGS        COMP-3 PIC S9(9)V99.
           10  :FSSC:-NOE-ORIG-DT-CYMD.
             15  :FSSC:-NOE-ORIG-DT-CC                   PIC 9(2).
             15  :FSSC:-NOE-ORIG-DT.
               20  :FSSC:-NOE-ORIG-YY                    PIC 9(2).
               20  :FSSC:-NOE-ORIG-MO                    PIC 9(2).
               20  :FSSC:-NOE-ORIG-DY                    PIC 9(2).
           10  :FSSC:-CWF-PRT-A-EFF-DT-CYMD.
             15  :FSSC:-CWF-PRT-A-EFF-DT-CC              PIC 9(2).
             15  :FSSC:-CWF-PRT-A-EFF-DT.
               20  :FSSC:-CWF-PRT-A-EFF-YR               PIC 9(2).
               20  :FSSC:-CWF-PRT-A-EFF-MO               PIC 9(2).
               20  :FSSC:-CWF-PRT-A-EFF-DY               PIC 9(2).
           10  :FSSC:-BTCH-AUD-REC-DT-CYMD.
             15  :FSSC:-BTCH-AUD-REC-DT-CC               PIC 9(2).
             15  :FSSC:-BTCH-AUD-REC-DT.
               20 :FSSC:-BTCH-AUD-REC-YY                 PIC 9(2).
               20 :FSSC:-BTCH-AUD-REC-MM                 PIC 9(2).
               20 :FSSC:-BTCH-AUD-REC-DD                 PIC 9(2).
           10  :FSSC:-PART-B-EFF-DATE-CYMD.
             15  :FSSC:-PART-B-EFF-DATE-CC               PIC 9(2).
             15  :FSSC:-PART-B-EFF-DATE.
               20 :FSSC:-PART-B-EFF-YY                   PIC 9(2).
               20 :FSSC:-PART-B-EFF-MM                   PIC 9(2).
               20 :FSSC:-PART-B-EFF-DD                   PIC 9(2).
           10  :FSSC:-HMO-EFF-DT-CYMD.
             15  :FSSC:-HMO-EFF-DT-CC                    PIC 9(2).
             15  :FSSC:-HMO-EFF-DT.
               20  :FSSC:-HMO-EFF-YR                     PIC 9(2).
               20  :FSSC:-HMO-EFF-MO                     PIC 9(2).
               20  :FSSC:-HMO-EFF-DY                     PIC 9(2).
           10  :FSSC:-HMO-CANC-DT-CYMD.
             15  :FSSC:-HMO-CANC-DT-CC                   PIC 9(2).
             15  :FSSC:-HMO-CANC-DT.
               20  :FSSC:-HMO-CANC-YR                    PIC 9(2).
               20  :FSSC:-HMO-CANC-MO                    PIC 9(2).
               20  :FSSC:-HMO-CANC-DY                    PIC 9(2).
           10  :FSSC:-CWF-TRANSMIT-DT-CYMD.
             15  :FSSC:-CWF-TRANSMIT-DT-CC               PIC 9(2).
             15  :FSSC:-CWF-TRANSMIT-DT.
               20  :FSSC:-CWF-YR                         PIC 9(2).
               20  :FSSC:-CWF-MO                         PIC 9(2).
               20  :FSSC:-CWF-DY                         PIC 9(2).
           10  :FSSC:-CWF-RESPONSE-DT-CYMD.
             15  :FSSC:-CWF-RESPONSE-DT-CC               PIC 9(2).
             15  :FSSC:-CWF-RESPONSE-DT.
               20  :FSSC:-CWF-RESPONSE-YR                PIC 9(2).
               20  :FSSC:-CWF-RESPONSE-MO                PIC 9(2).
               20  :FSSC:-CWF-RESPONSE-DY                PIC 9(2).
           10  :FSSC:-CWF-ORIG-TRNS-DT-CYMD     REDEFINES
                   :FSSC:-CWF-RESPONSE-DT-CYMD.
             15  :FSSC:-CWF-ORIG-TRANS-DT-CC             PIC 9(2).
             15  :FSSC:-CWF-ORIG-TRANS-DT.
               20  :FSSC:-CWF-ORIG-TRANS-YR              PIC 9(2).
               20  :FSSC:-CWF-ORIG-TRANS-MO              PIC 9(2).
               20  :FSSC:-CWF-ORIG-TRANS-DY              PIC 9(2).
           10  :FSSC:-VER-NCOV-FROM-DT-CYMD.
             15  :FSSC:-VERIF-NCOV-FROM-DT-CC            PIC 9(2).
             15  :FSSC:-VERIF-NCOV-FROM-DT.
               20  :FSSC:-VERIF-NCOV-FROM-YR             PIC 9(2).
               20  :FSSC:-VERIF-NCOV-FROM-MO             PIC 9(2).
               20  :FSSC:-VERIF-NCOV-FROM-DY             PIC 9(2).
           10  :FSSC:-VERIF-NCOV-TO-DT-CYMD.
             15  :FSSC:-VERIF-NCOV-TO-DT-CC              PIC 9(2).
             15  :FSSC:-VERIF-NCOV-TO-DT.
               20  :FSSC:-VERIF-NCOV-TO-YR               PIC 9(2).
               20  :FSSC:-VERIF-NCOV-TO-MO               PIC 9(2).
               20  :FSSC:-VERIF-NCOV-TO-DY               PIC 9(2).
           10  :FSSC:-CARRIER-CD-ID                      PIC 9(5).
           10  :FSSC:-LOCALITY-CD-ID                     PIC 9(2).
           10  :FSSC:-PATIENT-AGE                 COMP-3 PIC S9(3).
           10  :FSSC:-AUDIT-TRAIL-ENTRIES                PIC 9(2).
           10  :FSSC:-CWF-NB-OF-TIMES-SENT               PIC 9(2).
           10  :FSSC:-PATIENT-STATUS                     PIC 9(2).
             88  :FSSC:-PAT-STAT-01-DISC-HOME             VALUE 01.
             88  :FSSC:-PAT-STAT-02-DISC-HOSP             VALUE 02.
             88  :FSSC:-PAT-STAT-03-DISC-SNF              VALUE 03.
             88  :FSSC:-PAT-STAT-04-DISC-ICF              VALUE 04.
             88  :FSSC:-PAT-STAT-05-DISC-OTHR             VALUE 05.
             88  :FSSC:-PAT-STAT-06-DISC-HH               VALUE 06.
             88  :FSSC:-PAT-STAT-07-LEFT                  VALUE 07.
             88  :FSSC:-PAT-STAT-09-ADMITTED              VALUE 09.
             88  :FSSC:-PAT-STAT-20-EXPIRED               VALUE 20.
             88  :FSSC:-PAT-STAT-30-STILL-PAT             VALUE 30.
             88  :FSSC:-PAT-STAT-40-EXP-HOME              VALUE 40.
             88  :FSSC:-PAT-STAT-41-EXP-FAC               VALUE 41.
             88  :FSSC:-PAT-STAT-42-EXP-UNK               VALUE 42.
             88  :FSSC:-PAT-STAT-43-DISC-FED              VALUE 43.
             88  :FSSC:-PAT-STAT-50-HOSP-HOME             VALUE 50.
             88  :FSSC:-PAT-STAT-51-HOSP-MED              VALUE 51.
             88  :FSSC:-PAT-STAT-61-DISC-SWIN             VALUE 61.
             88  :FSSC:-PAT-STAT-62-DISC-IRF              VALUE 62.
             88  :FSSC:-PAT-STAT-63-DISC-LTC              VALUE 63.
             88  :FSSC:-PAT-STAT-64-DISC-NURS             VALUE 64.
             88  :FSSC:-PAT-STAT-65-DISC-PSYC             VALUE 65.
             88  :FSSC:-PAT-STAT-66-DISC-CAH              VALUE 66.
             88  :FSSC:-PAT-STAT-71-DISC-OUTO             VALUE 71.
             88  :FSSC:-PAT-STAT-72-DISC-OUTH             VALUE 72.
C10145       88  :FSSC:-PAT-STAT-82-DISC-INPH             VALUE 82.
           10  :FSSC:-UB04-FILLER-F41                    PIC X(1).
           10  :FSSC:-DRG-PATIENT-AGE             COMP-3 PIC S9(3).
           10  :FSSC:-EXPENSES-TO-DED             COMP-3 PIC S9(9)V99.
           10  :FSSC:-DISC-HR                            PIC 9(2).
             88  :FSSC:-DISC-HR-VALID                     VALUES
                                                      00  THRU  24, 99.
             88  :FSSC:-DISC-HR-99-UNKNOWN                VALUE 99.
           10  :FSSC:-UB04-FILLER-F42                    PIC X(1).
           10  :FSSC:-COV-DY-CNT                  COMP-3 PIC S9(3).
           10  :FSSC:-CST-REP-DYS                 COMP-3 PIC S9(3).
           10  :FSSC:-WAGE-INDEX                  COMP-3 PIC 99V9(04).
           10  :FSSC:-DRG-REIMB-AMT               COMP-3 PIC S9(9)V99.
           10  :FSSC:-FED-PORTION                 COMP-3 PIC S9(9)V99.
           10  :FSSC:-NAT-PCT                            PIC 9(1)V99.
           10  :FSSC:-FSP-PCT                            PIC 9(1)V99.
           10  :FSSC:-DAYS-CUTOFF                        PIC 9(2)V9.
           10  :FSSC:-AVG-LOS                            PIC 9(2)V9.
           10  :FSSC:-PPS-PAYMENT                 COMP-3 PIC S9(6)V99.
           10  :FSSC:-B-LOS                              PIC 9(4).
           10  :FSSC:-REG-DAY-USED                COMP-3 PIC S9(3).
           10  :FSSC:-DRG-WEIGHT                  COMP-3 PIC 99V9(4).
           10  :FSSC:-DSCHG-FRCTN                 COMP-3 PIC 9V9(4).
           10  :FSSC:-DRG-WT-FRCTN                COMP-3 PIC 99V9(4).
           10  :FSSC:-PASS-THRU-PDIEM-RATE        COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP-TOT-PAY                 COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP-FSP                     COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP-DSH-ADJ                 COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP2-B-FSP                  COMP-3 PIC S9(9)V99.
           10  :FSSC:-UTIL-FULL-DAYS              COMP-3 PIC S9(3).
           10  :FSSC:-CAP-HSP                     COMP-3 PIC S9(9)V99.
           10  :FSSC:-ORIG-COV-DY-CNT             COMP-3 PIC S9(3).
           10  :FSSC:-ORIG-CST-REP-DYS            COMP-3 PIC S9(3).
           10  :FSSC:-NCOV-DY-CNT                        PIC 9(4).
           10  :FSSC:-INTEREST-REIMB-PROV         COMP-3 PIC S9(7)V99.
           10  :FSSC:-CAP-OLD-HARM                COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP-IME-ADJ                 COMP-3 PIC S9(9)V99.
           10  :FSSC:-BLD-PINT-UNIT-VAL           COMP-3 PIC S9(7)V99.
           10  :FSSC:-VER-PAT-LIABILITY.
             15  :FSSC:-VER-PAT-DED-BLD           COMP-3 PIC S9(7)V99.
             15  :FSSC:-VER-PAT-CASH-DED          COMP-3 PIC S9(7)V99.
             15  :FSSC:-VER-PAT-COIN              COMP-3 PIC S9(7)V99.
           10  :FSSC:-BENE-SAVINGS                COMP-3 PIC S9(9)V99.
           10  :FSSC:-COINS-DY-CNT                COMP-3 PIC S9(3).
CR9015     10  :FSSC:-PPS-RTC                            PIC X(2).
           10  :FSSC:-COIN-DAYS-1ST-YR            COMP-3 PIC S9(3).
           10  :FSSC:-B-REVIEW-CD                        PIC 9(2).
             88  :FSSC:-B-REV-00-WITH-OUTLIER             VALUE 00.
             88  :FSSC:-B-REV-01-DAY-OUTLIER              VALUE 01.
             88  :FSSC:-B-REV-02-COST-OUTLIER             VALUE 02.
             88  :FSSC:-B-REV-03-PER-DIEM-DAY             VALUE 03.
             88  :FSSC:-B-REV-04-AVERAGE-STAY             VALUE 04.
             88  :FSSC:-B-REV-05-TRAN-W-COST              VALUE 05.
             88  :FSSC:-B-REV-06-TRAN-NO-COST             VALUE 06.
             88  :FSSC:-B-REV-07-WITHOUT-COST             VALUE 07.
             88  :FSSC:-B-REV-08-PPS-DRG-480              VALUE 08.
             88  :FSSC:-B-REV-09-POST-ACUTE               VALUE 09.
           10  :FSSC:-HMO-PAY-CD                         PIC 9(1).
             88  :FSSC:-HMO-PAY-0-FI-PAYS                 VALUE 0.
             88  :FSSC:-HMO-PAY-1-HMO-PAYS                VALUE 1.
           10  :FSSC:-CAP-OUTLIER                 COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAP2-B-OUTLIER              COMP-3 PIC S9(9)V99.
           10  :FSSC:-OUTLIER-DYS                 COMP-3 PIC S9(3).
           10  :FSSC:-HOSP-PORTION                COMP-3 PIC S9(9)V99.
           10  :FSSC:-PAT-PAID-EXC-BLD            COMP-3 PIC S9(9)V99.
           10  :FSSC:-REIMB-DIST-9.
             15  :FSSC:-REIMB-RATE                COMP-3 PIC S9(4)V99.
             15  :FSSC:-REIMB-RATE-R            REDEFINES
                 :FSSC:-REIMB-RATE                COMP-3 PIC S9(6).
             15  :FSSC:-REIMB-PAT-AMT             COMP-3 PIC S9(9)V99.
           10  :FSSC:-LIFE-DY-CNT                 COMP-3 PIC S9(3).
           10  :FSSC:-LTR-DAYS-USED               COMP-3 PIC S9(3).
           10  :FSSC:-LST-PAP-SMEAR-DT-CYMD.
             15  :FSSC:-LAST-PAP-SMEAR-DT-CC             PIC 9(2).
             15  :FSSC:-LAST-PAP-SMEAR-DT.
               20  :FSSC:-LAST-PAP-SMEAR-YY              PIC 9(2).
               20  :FSSC:-LAST-PAP-SMEAR-MM              PIC 9(2).
               20  :FSSC:-LAST-PAP-SMEAR-DD              PIC 9(2).
           10  :FSSC:-MSP-TOTAL-COIN              COMP-3 PIC S9(7)V99.
           10  :FSSC:-COIN-DAYS-2ND-YR            COMP-3 PIC S9(3).
           10  :FSSC:-LTR-DAYS-2ND-YR             COMP-3 PIC S9(3).
           10  :FSSC:-MSP-BLOOD-DED               COMP-3 PIC S9(7)V99.
           10  :FSSC:-TECH-PROV-CHRGS             COMP-3 PIC S9(9)V99.
           10  :FSSC:-CAPI-EXCEPTIONS             COMP-3 PIC S9(9)V99.
           10  :FSSC:-INTEREST-AMT-BENE           COMP-3 PIC S9(7)V99.
           10  :FSSC:-PAT-PAID-BLD-DED            COMP-3 PIC S9(7)V99.
           10  :FSSC:-TECH-PROV-DAYS              COMP-3 PIC S9(3).
           10  :FSSC:-NON-BEN-PROV-FLT                   PIC 9(4).
           10  :FSSC:-ESRD-DATA-9.
             15  :FSSC:-ESRD-HR-CNT                      PIC 9(2).
             15  :FSSC:-ESRD-SESS-CNT                    PIC 9(2).
           10  :FSSC:-INDICATORS-9.
             15  :FSSC:-REASON-FOR-ENT-IND               PIC 9(1).
               88  :FSSC:-REAS-ENT-0-NORMAL               VALUE 0.
               88  :FSSC:-REAS-ENT-1-DISABLITY            VALUE 1.
               88  :FSSC:-REAS-ENT-2-ESRD                 VALUE 2.
               88  :FSSC:-REAS-ENT-3-DISAB-ESRD           VALUE 3.
           10  :FSSC:-PRE-ENTI-PSYCH-DY-CNT       COMP-3 PIC S9(3).
           10  :FSSC:-ACTUAL-MEDA-REIMB           COMP-3 PIC S9(9)V99.
           10  :FSSC:-REIMB-PROV-AMT              COMP-3 PIC S9(9)V99.
           10  :FSSC:-PATIENT-RESP                COMP-3 PIC S9(9)V99.
           10  :FSSC:-ESRD-FINAL-REIMB            COMP-3 PIC S9(9)V99.
           10  :FSSC:-ESRD-HOLD-REIMB             COMP-3 PIC S9(9)V99.
           10  :FSSC:-ORIG-CALC-MEDA-REIMB        COMP-3 PIC S9(9)V99.
           10  :FSSC:-PAY-REDUCT-AMT              COMP-3 PIC S9(9)V99.
           10  :FSSC:-ORIG-PROV-REIMB             COMP-3 PIC S9(9)V99.
           10  :FSSC:-DUE-FROM-PAT.
             15  :FSSC:-DUE-EST-RESP              COMP-3 PIC S9(9)V99.
             15  :FSSC:-DUE-PRIOR-PMT             COMP-3 PIC S9(9)V99.
             15  :FSSC:-DUE-EST-AMT-DUE           COMP-3 PIC S9(9)V99.
           10  :FSSC:-MSP-CASH-DED                COMP-3 PIC S9(7)V99.
           10  :FSSC:-ORIG-EXPENSES-TO-DED        COMP-3 PIC S9(9)V99.
           10  :FSSC:-ORIG-PPS-PAYMENT            COMP-3 PIC S9(9)V99.
           10  :FSSC:-ORIG-PAT-LIABILITY.
             15  :FSSC:-ORIG-PAT-DED-BLD          COMP-3 PIC S9(7)V99.
             15  :FSSC:-ORIG-PAT-CASH-DED         COMP-3 PIC S9(7)V99.
             15  :FSSC:-ORIG-PAT-COIN             COMP-3 PIC S9(7)V99.
           10  :FSSC:-ORIG-PASS-THRU-PDIEM        COMP-3 PIC S9(9)V99.
           10  :FSSC:-ADR-ORIG-REQ-DT-CYMD.
             15  :FSSC:-ADR-ORIG-REQ-DT-CC               PIC 9(2).
             15  :FSSC:-ADR-ORIG-REQ-DT.
               20  :FSSC:-ADR-ORIG-REQ-YR                PIC 9(2).
               20  :FSSC:-ADR-ORIG-REQ-MO                PIC 9(2).
               20  :FSSC:-ADR-ORIG-REQ-DY                PIC 9(2).
           10  :FSSC:-EMC-RT21-EMPL-QUAL         OCCURS 4 TIMES.
             15  :FSSC:-RT21-QUAL-CODE                   PIC 9(2).
           10  :FSSC:-PPS-DOLLAR-THRES            COMP-3 PIC S9(7)V9(9).
           10  :FSSC:-EMC-REC21-EMPLYR-INF9      OCCURS 4 TIMES.
             15  :FSSC:-EMC-R21-EMPL-STATUS              PIC 9(1).
             15  :FSSC:-EMC-R21-EMPLYR-QUAL              PIC 9(2).
           10  :FSSC:-EMC-REC90-COUNTS.
             15  :FSSC:-EMC-R90-COUNT-ALL                PIC 9(3).
             15  :FSSC:-EMC-R90-COUNT-2N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-3N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-4N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-5N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-6N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-7N                 PIC 9(2).
             15  :FSSC:-EMC-R90-COUNT-8N                 PIC 9(2).
           10  :FSSC:-EMC-REC90-R91-QUAL                 PIC 9(1).
           10  :FSSC:-EMC-REC90-ACCOM-TOT         COMP-3 PIC S9(9)V99.
           10  :FSSC:-EMC-REC90-ACCOM-NCOV        COMP-3 PIC S9(9)V99.
           10  :FSSC:-EMC-REC90-ANCIL-TOT         COMP-3 PIC S9(9)V99.
           10  :FSSC:-EMC-REC90-ANCIL-NCOV        COMP-3 PIC S9(9)V99.
           10  :FSSC:-PRO-PROCESS-DT-CYMD.
             15  :FSSC:-PRO-PROCESS-DT-CC                PIC 9(2).
             15  :FSSC:-PRO-PROCESS-DT.
               20  :FSSC:-PRO-PROC-DT-YY                 PIC 9(2).
               20  :FSSC:-PRO-PROC-DT-MO                 PIC 9(2).
               20  :FSSC:-PRO-PROC-DT-DY                 PIC 9(2).
           10  :FSSC:-DATE-TOB-CHANGED-CYMD.
             15  :FSSC:-DATE-TOB-CHANGED-CC              PIC 9(2).
             15  :FSSC:-DATE-TOB-CHANGED.
               20 :FSSC:-TOB-CHANGED-YY                  PIC 9(2).
               20 :FSSC:-TOB-CHANGED-MM                  PIC 9(2).
               20 :FSSC:-TOB-CHANGED-DD                  PIC 9(2).
           10  :FSSC:-PROC-CODING-METHOD                 PIC 9(1).
             88  :FSSC:-PROC-CODING-4-CPT4                VALUE 4.
             88  :FSSC:-PROC-CODING-5-HCPCS               VALUE 5.
             88  :FSSC:-PROC-CODING-9-ICD9                VALUE 9.
           10  :FSSC:-ADR-COUNT                          PIC 9(1).
           10  :FSSC:-ADS-REQ-DT-CYMD.
             15  :FSSC:-ADS-REQ-DT-CC                    PIC 9(2).
             15  :FSSC:-ADS-REQ-DT.
               20  :FSSC:-ADS-REQ-YR                     PIC 9(2).
               20  :FSSC:-ADS-REQ-MM                     PIC 9(2).
               20  :FSSC:-ADS-REQ-DY                     PIC 9(2).
           10  :FSSC:-ADM-HR                             PIC 9(2).
           10  :FSSC:-UB04-FILLER-F43                    PIC X(1).
           10  :FSSC:-NUM-53-DISPS                       PIC 9(1).
           10  :FSSC:-REPRICE-PIMR-SW                    PIC X(1).
             88  :FSSC:-REPRICE-PIMR-Y-YES                VALUE 'Y'.
           10  :FSSC:-935-ADJ                            PIC X(1).
             88  :FSSC:-935-ADJ-Y-YES                     VALUE 'Y'.
FS7202       88  :FSSC:-935-ADJ-N-NO                      VALUE 'N'.    FS7202U1
FS7202       88  :FSSC:-935-ADJ-R-REVERSAL                VALUE 'R'.    FS7202U1
           10  :FSSC:-COB-NDC                            PIC X(24).
           10  :FSSC:-5010-EXPANSIONS.
             15  :FSSC:-UB04-FILLER-F39                  PIC X(88).
             15  :FSSC:-PAT-VISIT-REASON-TAB    OCCURS 03 TIMES.
               20  :FSSC:-PAT-VISIT-REASON-T             PIC X(7).
             15  :FSSC:-IDE-NUMBER-TAB          OCCURS 05 TIMES.
               20  :FSSC:-IDE-NUMBER-T                   PIC X(15).
           10  :FSSC:-HH-RECODE-IND                      PIC X(1).
             88  :FSSC:-HH-RECODE-DEFAULT                 VALUE ' '.
             88  :FSSC:-HH-RECODE-1-TRL8-524Q             VALUE '1'.
             88  :FSSC:-HH-RECODE-2-TRL8-539H             VALUE '2'.
             88  :FSSC:-HH-RECODE-3-TRL8-524P             VALUE '3'.
             88  :FSSC:-HH-RECODE-Z-TRL24                 VALUE 'Z'.
             88  :FSSC:-HH-RECODE-Y-FILE-FIX              VALUE 'Y'.
           10  :FSSC:-VALCD-CARRIER                      PIC X(5).
           10  :FSSC:-VALCD-LOCALITY                     PIC X(2).
           10  :FSSC:-ICD9-10-IND                        PIC X(1).
FS7202       88  :FSSC:-ICD9                              VALUE '9'.    FS7202U1
FS7202       88  :FSSC:-ICD10                             VALUE ZERO.   FS7202U1
           10  :FSSC:-PRIM-CARC-IND                      PIC X(1).
             88  :FSSC:-PRIM-CARC-Y-YES                   VALUE 'Y'.
             88  :FSSC:-PRIM-CARC-DEFAULT                 VALUE ' '.
           10  :FSSC:-REJ-CARC-IND                       PIC X(1).
             88  :FSSC:-REJ-CARC-Y-YES                    VALUE 'Y'.
             88  :FSSC:-REJ-CARC-DEFAULT                  VALUE ' '.
           10  :FSSC:-CWF-WORK-CARC-IND                  PIC X(1).
             88  :FSSC:-CWF-WORK-CARC-Y-YES               VALUE 'Y'.
             88  :FSSC:-CWF-WORK-CARC-DEFAULT             VALUE ' '.
           10  :FSSC:-PRIM-OVER-CARC-IND                 PIC X(1).
             88  :FSSC:-PRIM-WORK-CARC-Y-YES              VALUE 'Y'.
FS7202       88  :FSSC:-PRIM-WORK-CARC-NO                 VALUE ' '.    FS7202U1
           10  :FSSC:-SUS-CARC-IND                       PIC X(1).
             88  :FSSC:-SUS-CARC-Y-YES                    VALUE 'Y'.
             88  :FSSC:-SUS-CARC-DEFAULT                  VALUE ' '.
           10  :FSSC:-225-CARC-IND                       PIC X(1).
             88  :FSSC:-225-CARC-Y-YES                    VALUE 'Y'.
             88  :FSSC:-225-CARC-DEFAULT                  VALUE ' '.
           10  :FSSC:-CO-AMT                      COMP-3 PIC S9(9)V99.
           10  :FSSC:-TOT-CARC-AMT                COMP-3 PIC S9(9)V99.
           10  :FSSC:-CALC-OTAF-AMT               COMP-3 PIC S9(9)V99.
           10  :FSSC:-MSP-VALCD-ADJ             OCCURS 2 TIMES.
               15 :FSSC:-MSP-VALCD                       PIC X(02).
               15 :FSSC:-MSP-VALCD-ADJ-AMT        COMP-3 PIC S9(7)V99.
           10  :FSSC:-228-CARC-IND                       PIC X(1).
             88  :FSSC:-228-CARC-Y-YES                    VALUE 'Y'.
             88  :FSSC:-228-CARC-DEFAULT                  VALUE ' '.
           10  :FSSC:-230-CARC-IND                       PIC X(1).
             88  :FSSC:-230-CARC-Y-YES                    VALUE 'Y'.
             88  :FSSC:-230-CARC-DEFAULT                  VALUE ' '.
           10  :FSSC:-MCE-EDIT-SW                        PIC X(1).
             88  :FSSC:-MCE-EDIT-Y-YES                    VALUE 'Y'.
             88  :FSSC:-MCE-EDIT-DEFAULT                  VALUE ' '.
           10  :FSSC:-RAC-REGION                         PIC X(1).
             88  :FSSC:-RAC-REGION-DEFAULT                VALUE ' '.
             88  :FSSC:-RAC-REGION-A                      VALUE 'A'.
             88  :FSSC:-RAC-REGION-B                      VALUE 'B'.
             88  :FSSC:-RAC-REGION-C                      VALUE 'C'.
             88  :FSSC:-RAC-REGION-D                      VALUE 'D'.
           10  :FSSC:-ESRD-DIAG-LAST-DIGIT               PIC X(1).
           10  :FSSC:-DIAL-STRT-DT-CYMD.
             15  :FSSC:-DIAL-STRT-DT-CC                  PIC 9(2).
             15  :FSSC:-DIAL-STRT-DT.
               20  :FSSC:-DIAL-STRT-DT-YY                PIC 9(2).
               20  :FSSC:-DIAL-STRT-DT-MM                PIC 9(2).
               20  :FSSC:-DIAL-STRT-DT-DD                PIC 9(2).
           10  :FSSC:-CWF-RETURN-CODE                    PIC X(2).
           10  :FSSC:-PPS-COMORBID-PAY                   PIC X(2).
           10  :FSSC:-ESRD-ELIG-RESET-SW                 PIC X(1).
             88  :FSSC:-ESRD-ELIG-RES-Y-CALC              VALUE 'Y'.
             88  :FSSC:-ESRD-ELIG-RES-N-NOT               VALUE 'N'.
             88  :FSSC:-ESRD-ELIG-RES-DEFAULT             VALUE ' '.
           10  :FSSC:-ESRD-COMORBIDCC-SW                 PIC X(1).
             88  :FSSC:-ESRD-COMORBID-DEFAULT             VALUE ' '.
             88  :FSSC:-ESRD-COMORBID-Y-MATCH             VALUE 'Y'.
           10  :FSSC:-ATTEND-PHYS-SC                     PIC X(2).
FS7202       88  :FSSC:-ATTEND-PHYS-SC-DFLT               VALUE '  '.   FS7202U1
             88  :FSSC:-ATTEND-PHYS-SC-99-NOT             VALUE '99'.
           10  :FSSC:-OPER-PHYS-SC                       PIC X(2).
             88  :FSSC:-OPER-PHYS-SC-DEFAULT              VALUE '  '.
             88  :FSSC:-OPER-PHYS-SC-99-NOT               VALUE '99'.
           10  :FSSC:-OTH-PHYS-SC                        PIC X(2).
             88  :FSSC:-OTH-PHYS-SC-DEFAULT               VALUE '  '.
             88  :FSSC:-OTH-PHYS-SC-99-NOT                VALUE '99'.
           10  :FSSC:-PWK-ATTACH                         PIC X(1).
             88  :FSSC:-PWK-ATTACH-E-ELECTRON             VALUE 'E'.
             88  :FSSC:-PWK-ATTACH-F-FAX                  VALUE 'F'.
             88  :FSSC:-PWK-ATTACH-M-MAIL                 VALUE 'M'.
C10397       88  :FSSC:-PWK-ATTACH-T-FLTRN                VALUE 'T'.
             88  :FSSC:-PWK-ATTACH-DEFAULT                VALUE ' '.
C10397       88  :FSSC:-PWK-ATTACH-NONE                   VALUE 'N'.
C10397       88  :FSSC:-PWK-ATTACH-VALID                  VALUE 'E'
C10397                                                  'F' 'M' 'T'.
           10  :FSSC:-PWK-DEV-FLAG                       PIC X(1).
             88  :FSSC:-PWK-DEV-DEFAULT                   VALUE ' '.
FS7202       88  :FSSC:-PWK-DEV-D-DEVELOP                 VALUE 'D'.    FS7202U1
FS7202       88  :FSSC:-PWK-DEV-N-NO-DEVELOP              VALUE 'N'.    FS7202U1
           10  :FSSC:-PWK-PCN                            PIC X(10).
           10  :FSSC:-PWK-IND                            PIC X(2).
             88  :FSSC:-PWK-IND-DEFAULT                   VALUE '  '.
             88  :FSSC:-PWK-IND-P1-1-ITERAT               VALUE 'P1'.
             88  :FSSC:-PWK-IND-P2-2-ITERAT               VALUE 'P2'.
             88  :FSSC:-PWK-IND-P3-3-ITERAT               VALUE 'P3'.
             88  :FSSC:-PWK-IND-P4-4-ITERAT               VALUE 'P4'.
             88  :FSSC:-PWK-IND-P5-5-ITERAT               VALUE 'P5'.
             88  :FSSC:-PWK-IND-P6-6-ITERAT               VALUE 'P6'.
             88  :FSSC:-PWK-IND-P7-7-ITERAT               VALUE 'P7'.
             88  :FSSC:-PWK-IND-P8-8-ITERAT               VALUE 'P8'.
             88  :FSSC:-PWK-IND-P9-9-ITERAT               VALUE 'P9'.
             88  :FSSC:-PWK-IND-PO-10-ITERAT              VALUE 'P0'.
FS7202     10  :FSSC:-VBP-IND                            PIC X(1).      FS7202U1
           10  :FSSC:-VBP-ADJ                     COMP-3 PIC 9V9(11).
           10  :FSSC:-HRR-IND                            PIC X(1).
           10  :FSSC:-HRR-ADJ                     COMP-3 PIC 9V9(04).
           10  :FSSC:-BENE-NAME-SUBMITTED.
               15 :FSSC:-LAST-NAME-ORG                   PIC X(15).
               15 :FSSC:-FIRST-NAME-ORG                  PIC X(10).
               15 :FSSC:-MID-INIT-ORG                    PIC X(1).
           10  :FSSC:-RAC-SPCL-ACT-CD                    PIC X(01).
             88  :FSSC:-RAC-SPCL-ACT-DEFAULT              VALUE ' '.
             88  :FSSC:-RAC-SPCL-ACT-S-SUSP               VALUE 'S'.
           10  :FSSC:-REN-PHYS-ID                        PIC X(16).
           10  :FSSC:-REN-PHYS-UPIN                    REDEFINES
               :FSSC:-REN-PHYS-ID.
             15  :FSSC:-REN-PHYS-UPIN-NUM                PIC X(6).
             15  :FSSC:-REN-PHYS-NPI-NUM                 PIC 9(10).
           10  :FSSC:-REN-FILLER-F30                     PIC X(2).
           10  :FSSC:-REN-FILLER-F31                     PIC X(9).
           10  :FSSC:-REN-PHYS-NAME.
             15  :FSSC:-REN-PHYS-LNAME                   PIC X(17).
             15  :FSSC:-REN-PHYS-FNAME                   PIC X(8).
             15  :FSSC:-REN-FILLER-F32                   PIC X(4).
             15  :FSSC:-REN-PHYS-MINT                    PIC X(1).
           10  :FSSC:-REN-PHYS-FLAG                      PIC X(1).
           10  :FSSC:-REN-PHYS-SC                        PIC X(2).
             88  :FSSC:-REN-PHYS-SC-DEFAULT               VALUE '  '.
             88  :FSSC:-REN-PHYS-SC-99-NOT                VALUE '99'.
           10  :FSSC:-SFSCINFO-RA-NBR                    PIC X(05).
           10  :FSSC:-DEMO-CD-2.
             15  :FSSC:-DEMO-NUM-2                       PIC X(2).
             15  :FSSC:-DEMO-FLAG-2                      PIC X(1).
           10  :FSSC:-DEMO-CD-3.
             15  :FSSC:-DEMO-NUM-3                       PIC X(2).
             15  :FSSC:-DEMO-FLAG-3                      PIC X(1).
           10  :FSSC:-DEMO-CD-4.
             15  :FSSC:-DEMO-NUM-4                       PIC X(2).
             15  :FSSC:-DEMO-FLAG-4                      PIC X(1).
CR8546     10  :FSSC:-BM1-PER-FILLER              COMP-3 PIC V9(02).
           10  :FSSC:-BASE-OPER-DRG-AMT           COMP-3 PIC S9(09)V99.
           10  FILLER                                    PIC X(04).
           10  :FSSC:-OPER-HSP-AMT                COMP-3 PIC S9(09)V99.
           10  FILLER                                    PIC X(04).
           10  :FSSC:-REF-PHYS-ID                        PIC X(16).
           10  :FSSC:-REF-PHYS-UPIN                    REDEFINES
               :FSSC:-REF-PHYS-ID.
             15  :FSSC:-REF-PHYS-UPIN-NUM                PIC X(6).
             15  :FSSC:-REF-PHYS-NPI-NUM                 PIC 9(10).
           10  :FSSC:-REF-FILLER-F30                     PIC X(2).
           10  :FSSC:-REF-FILLER-F31                     PIC X(9).
           10  :FSSC:-REF-PHYS-NAME.
             15  :FSSC:-REF-PHYS-LNAME                   PIC X(17).
             15  :FSSC:-REF-PHYS-FNAME                   PIC X(8).
             15  :FSSC:-REF-FILLER-F32                   PIC X(4).
             15  :FSSC:-REF-PHYS-MINT                    PIC X(1).
           10  :FSSC:-REF-PHYS-FLAG                      PIC X(1).
           10  :FSSC:-REF-PHYS-SC                        PIC X(2).
             88  :FSSC:-REF-PHYS-SC-DEFAULT               VALUE '  '.
             88  :FSSC:-REF-PHYS-SC-99-NOT                VALUE '99'.
           10  :FSSC:-FIND-RESP-IND                      PIC X(01).
             88  :FSSC:-FIND-RESP-DEFAULT                 VALUE ' '.
             88  :FSSC:-FIND-RESP-1-1-CYCLE               VALUE '1'.
             88  :FSSC:-FIND-RESP-2-2-CYCLES              VALUE '2'.
             88  :FSSC:-FIND-RESP-3-3-CYCLES              VALUE '3'.
             88  :FSSC:-FIND-RESP-4-4-CYCLES              VALUE '4'.
             88  :FSSC:-FIND-RESP-5-5-CYCLES              VALUE '5'.
           10   :FSSC:-MSN-FROM-DT-CYMD.
             15  :FSSC:-MSN-FROM-DT-CC                   PIC 9(2).
             15  :FSSC:-MSN-FROM-DT.
               20  :FSSC:-MSN-FROM-DT-YY                 PIC 9(2).
               20  :FSSC:-MSN-FROM-DT-MO                 PIC 9(2).
               20  :FSSC:-MSN-FROM-DT-DY                 PIC 9(2).
           10  :FSSC:-MSN-TO-DT-CYMD.
             15  :FSSC:-MSN-TO-DT-CC                     PIC 9(2).
             15  :FSSC:-MSN-TO-DT.
               20  :FSSC:-MSN-TO-DT-YY                   PIC 9(2).
               20  :FSSC:-MSN-TO-DT-MO                   PIC 9(2).
               20  :FSSC:-MSN-TO-DT-DY                   PIC 9(2).

           10  :FSSC:-OVERPAY                            PIC X(1).
           10  :FSSC:-OVERPAY-CODE                       PIC X(3).
           10  :FSSC:-FPS-H-MODEL                        PIC X(02).
           10  :FSSC:-FPS-H-MSN1                         PIC X(05).
           10  :FSSC:-FPS-H-MSN2                         PIC X(05).
           10  :FSSC:-READMT-IND                         PIC X(1).
             88  :FSSC:-READMT-DEFAULT                    VALUE ' '.
             88  :FSSC:-READMT-1-MOD4-RELATED             VALUE '1'.
             88  :FSSC:-READMT-2-MOD4-1ST                 VALUE '2'.
             88  :FSSC:-READMT-3-MOD4-2ND                 VALUE '3'.
           10  :FSSC:-DEMO-PROCESS-IND                   PIC X(1).
             88  :FSSC:-DEMO-PROCESS-DEFAULT              VALUE ' '.
             88  :FSSC:-DEMO-PROCESS-A-NO                 VALUE 'A'.
CR9415       88  :FSSC:-DEMO-75-JOINT-REPL-NO             VALUE 'C'.
C10432       88  :FSSC:-DEMO-86-BPCI-ADV-NO               VALUE 'D'.
           10  :FSSC:-PRTB-DED-APP                COMP-3 PIC S9(7)V99.
           10  :FSSC:-PWK-DAYS                    COMP-3 PIC S9(03).
           10  :FSSC:-TRLR07-IND                         PIC X(1).
             88  :FSSC:-TRLR07-DEFAULT                    VALUE ' '.
             88  :FSSC:-TRLR07-Y-YES                      VALUE 'Y'.
FS0050       88  :FSSC:-TRLR07-B-SAVE-DED                 VALUE 'B'.
           10  :FSSC:-COIN-DY-YRS-2-6.
               15  :FSSC:-COIN-DAYS-YR-2          COMP-3 PIC S9(3).
               15  :FSSC:-COIN-DAYS-YR-3          COMP-3 PIC S9(3).
               15  :FSSC:-COIN-DAYS-YR-4          COMP-3 PIC S9(3).
               15  :FSSC:-COIN-DAYS-YR-5          COMP-3 PIC S9(3).
               15  :FSSC:-COIN-DAYS-YR-6          COMP-3 PIC S9(3).
           10  FILLER REDEFINES :FSSC:-COIN-DY-YRS-2-6.
               15  :FSSC:-COIN-DY-YRS       OCCURS 5 TIMES
                                                  COMP-3 PIC S9(3).
           10  :FSSC:-LTR-DY-YRS-2-6.
               15  :FSSC:-LTR-DAYS-YR-2           COMP-3 PIC S9(3).
               15  :FSSC:-LTR-DAYS-YR-3           COMP-3 PIC S9(3).
               15  :FSSC:-LTR-DAYS-YR-4           COMP-3 PIC S9(3).
               15  :FSSC:-LTR-DAYS-YR-5           COMP-3 PIC S9(3).
               15  :FSSC:-LTR-DAYS-YR-6           COMP-3 PIC S9(3).
           10  FILLER REDEFINES :FSSC:-LTR-DY-YRS-2-6.
               15  :FSSC:-LTR-DY-YRS        OCCURS 5 TIMES
                                                  COMP-3 PIC S9(3).
           10  :FSSC:-HI-DEMO-AMT                 COMP-3 PIC S9(7)V99.
           10  :FSSC:-SMI-DEMO-AMT                COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-PPS-UCOMP-PYMT              COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-BUNDLE-PYMT                 COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-VAL-PUR-AMT                 COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-READM-AMT                   COMP-3 PIC S9(7)V99.
CR8358     10  :FSSC:-SERVICE-FAC-NPI                    PIC 9(10).
CR8746     10  :FSSC:-PPS-STNDRD-VALUE            COMP-3 PIC S9(7)V99.
CR9031     10  :FSSC:-HAC-PAY-AMT                 COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-PPS-FLX7-PYMT               COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-EHR-ADJ-AMT                 COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-HAC-RED-IND                        PIC X(01).
CR8546         88  :FSSC:-HAC-RED-PPS             VALUE 'N'.
CR8546         88  :FSSC:-HAC-RED-NON-PPS         VALUE ' '.
CR8546     10  :FSSC:-PSF-UCOMP-PYMT              COMP-3 PIC S9(7)V99.
CR8546     10  :FSSC:-EHR-RED-IND                        PIC X(01).
CR8546     10  :FSSC:-BM1-PER                     COMP-3 PIC V9(03).
CR7825     10  :FSSC:-ORIG-VC17                   COMP-3 PIC S9(7)V99.  CR7825R7
CR8746     10  :FSSC:-FINL-STNDRD-AMT             COMP-3 PIC S9(9)V99.
CR8746     10  :FSSC:-PPS-RTC-PSEUDO-MW                  PIC 9(02).
CR8743     10  :FSSC:-GAF-AMT                     COMP-3 PIC S9V9(4).
CR8562     10  :FSSC:-UTN                                PIC X(14).
CR8562     10  :FSSC:-PROGRAM-INDICATOR                  PIC X(04).
CR8357     10  :FSSC:-ACTIVE-RECD-DT-CYMD.
CR8357         15  :FSSC:-ACTIVE-RECD-DT-CC              PIC 9(2).
CR8357         15  :FSSC:-ACTIVE-RECD-DT.
CR8357             20  :FSSC:-ACTIVE-RECD-DT-YR          PIC 9(2).
CR8357             20  :FSSC:-ACTIVE-RECD-MO             PIC 9(2).
CR8357             20  :FSSC:-ACTIVE-RECD-DY             PIC 9(2).
CR8800     10  :FSSC:-TREAT-AUTH-REMAIN.
CR8800         15  :FSSC:-TREAT-AUTH-REM  OCCURS 3 TIMES PIC X(32).
CR8932     10  :FSSC:-COIN-DY-YRS-7-15.
CR8932         15  :FSSC:-COIN-DAYS-YR-7          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-8          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-9          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-10         COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-11         COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-12         COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-13         COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-14         COMP-3 PIC S9(3).
CR8932         15  :FSSC:-COIN-DAYS-YR-15         COMP-3 PIC S9(3).
CR8932     10  FILLER REDEFINES :FSSC:-COIN-DY-YRS-7-15.
CR8932         15  :FSSC:-COIN-DY-YRS-EXT   OCCURS 9 TIMES
CR8932                                            COMP-3 PIC S9(3).
CR8932     10  :FSSC:-LTR-DY-YRS-7-15.
CR8932         15  :FSSC:-LTR-DAYS-YR-7           COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-8           COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-9           COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-10          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-11          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-12          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-13          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-14          COMP-3 PIC S9(3).
CR8932         15  :FSSC:-LTR-DAYS-YR-15          COMP-3 PIC S9(3).
CR8932     10  FILLER REDEFINES :FSSC:-LTR-DY-YRS-7-15.
CR8932         15  :FSSC:-LTR-DY-YRS-EXT    OCCURS 9 TIMES
CR8932                                            COMP-3 PIC S9(3).
CR9015     10  :FSSC:-SN-PAYMT-BASE-ON-COST       COMP-3 PIC S9(07)V99.
CR9015     10  :FSSC:-SN-PAYMT-BASE-ON-IPPS       COMP-3 PIC S9(07)V99.
CR9015     10  :FSSC:-STD-PAYMT-FULL              COMP-3 PIC S9(07)V99.
CR9015     10  :FSSC:-STD-PAYMT-SSO               COMP-3 PIC S9(07)V99.
CR8913     10  :FSSC:-PIMR-ACTIVITY-CODE                 PIC X(02).
CR9017         88  :FSSC:-PIMR-ZZ-PRIOR-AUTH             VALUE 'ZZ'.    CR9017A
CR9015     10  :FSSC:-CWF-REJCT-IUR-IND                  PIC X(01).     CR9015A
CR9015         88 :FSSC:-CWF-REJCT1-CD-728A              VALUE '1'.     CR9015A
CR9015         88 :FSSC:-CWF-REJCT2-CD-728B              VALUE '2'.     CR9015A
CR9015         88 :FSSC:-CWF-REJCT3-CD-728C              VALUE '3'.     CR9015A
CR9015         88 :FSSC:-CWF-IUR1-CD-728D                VALUE '4'.     CR9015A
CR9015         88 :FSSC:-CWF-IUR2-CD-728E                VALUE '5'.     CR9015A
CR9015         88 :FSSC:-CWF-IUR3-CD-728F                VALUE '6'.     CR9015A
CR9015         88 :FSSC:-CWF-IUR4-CD-728G                VALUE '7'.     CR9015A
CR9015         88 :FSSC:-CWF-IUR5-CD-728H                VALUE '8'.     CR9015A
CR8972     10  :FSSC:-SUBMITTER-ETIN                     PIC X(80).
CR8984     10  :FSSC:-MSP-ORM-IND                        PIC X(01).
CR8984     10  :FSSC:-MSP-ORM-CARC-IND                   PIC X(01).
CR9017     10  :FSSC:-PA-PIMR-ACT-CODE                   PIC X(02).     CR9017A
CR9017         88  :FSSC:-PA-PIMR-VALID                  VALUE '  '     CR9017A
CR9017                                                         'ZZ'.    CR9017A
CR9017         88  :FSSC:-PA-PIMR-ZZ-PRIOR-AUTH          VALUE 'ZZ'.    CR9017A
CR8486     10  :FSSC:-MSP1-CARC-DATA.
CR8486         15  :FSSC:-MSP1-PAID-DATE-CYMD.
CR8486             20  :FSSC:-MSP1-PAID-CC          PIC 9(02).
CR8486             20  :FSSC:-MSP1-PAID-DATE.
CR8486                 25  :FSSC:-MSP1-PAID-YY      PIC 9(02).
CR8486                 25  :FSSC:-MSP1-PAID-MM      PIC 9(02).
CR8486                 25  :FSSC:-MSP1-PAID-DD      PIC 9(02).
CR8486         15  :FSSC:-MSP1-PAID-AMT             COMP-3 PIC S9(9)V99.
CR8486         15  :FSSC:-MSP1-CARC-TABLE           OCCURS 20 TIMES.
CR8486             20  :FSSC:-MSP1-CODES.
CR8486                 25  :FSSC:-MSP1-CARC-GRP-CD  PIC X(02).
CR8486                 25  :FSSC:-MSP1-CARC-CD      PIC X(04).
CR8486             20  :FSSC:-MSP1-CARC-AMT         COMP-3 PIC S9(9)V99.
CR8486     10  :FSSC:-MSP2-CARC-DATA.
CR8486         15  :FSSC:-MSP2-PAID-DATE-CYMD.
CR8486             20  :FSSC:-MSP2-PAID-CC          PIC 9(02).
CR8486             20  :FSSC:-MSP2-PAID-DATE.
CR8486                 25  :FSSC:-MSP2-PAID-YY      PIC 9(02).
CR8486                 25  :FSSC:-MSP2-PAID-MM      PIC 9(02).
CR8486                 25  :FSSC:-MSP2-PAID-DD      PIC 9(02).
CR8486         15  :FSSC:-MSP2-PAID-AMT             COMP-3 PIC S9(9)V99.
CR8486         15  :FSSC:-MSP2-CARC-TABLE           OCCURS 20 TIMES.
CR8486             20  :FSSC:-MSP2-CODES.
CR8486                 25  :FSSC:-MSP2-CARC-GRP-CD  PIC X(02).
CR8486                 25  :FSSC:-MSP2-CARC-CD      PIC X(04).
CR8486             20  :FSSC:-MSP2-CARC-AMT         COMP-3 PIC S9(9)V99.
CR8486     10  :FSSC:-CO-AMT2                       COMP-3 PIC S9(9)V99.
CR8486     10  :FSSC:-TOT-CARC-AMT2                 COMP-3 PIC S9(9)V99.
CR9289     10  :FSSC:-PRIOR-BP-DAYS-CNT           COMP-3 PIC S9(03).
CR9289     10  :FSSC:-HOSP-SIA-END-DT-CYMD.
CR9289         15  :FSSC:-HOSP-SIA-END-DT-CC             PIC 9(2).
CR9289         15  :FSSC:-HOSP-SIA-END-DT.
CR9289             20  :FSSC:-HOSP-SIA-END-DT-YY         PIC 9(2).
CR9289             20  :FSSC:-HOSP-SIA-END-DT-MM         PIC 9(2).
CR9289             20  :FSSC:-HOSP-SIA-END-DT-DD         PIC 9(2).
CR9151     10  :FSSC:-NG-ACOP-ENH-TYP-PBP                PIC X(01).     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-RES-1              VALUE ' '.     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-PBP-1              VALUE '1'.     CR9151A
CR9151     10  :FSSC:-NG-ACOP-ENH-TYP-TELEH              PIC X(01).     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-RES-2              VALUE ' '.     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-TEL-2              VALUE '2'.     CR9151A
CR9151     10  :FSSC:-NG-ACOP-ENH-TYP-PST-D              PIC X(01).     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-RES-3              VALUE ' '.     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-PST-3              VALUE '3'.     CR9151A
CR9151     10  :FSSC:-NG-ACOP-ENH-SNF                    PIC X(01).     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-RES-4              VALUE ' '.     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-SNF-4              VALUE '4'.     CR9151A
CR9151     10  :FSSC:-NG-ACOP-ENH-CAPIT                  PIC X(01).     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-RES-5              VALUE ' '.     CR9151A
CR9151         88  :FSSC:-NG-ACOP-ENH-CAP-5              VALUE '5'.     CR9151A
CR9009     10  :FSSC:-RESIDUAL-PAY-IND                   PIC X(01).
CR9009       88  :FSSC:-RESIDUAL-SEC-PAY                  VALUE 'X'.
CR9009       88  :FSSC:-RESIDUAL-DEFAULT                  VALUE ' '.
CR9009       88  :FSSC:-RESIDUAL-VALID                  VALUES 'X' ' '.
CR9202     10  :FSSC:-REP-PAYEE                           PIC X(01).
CR9202         88  :FSSC:-REP-PAYEE-VALID               VALUES ' '
CR9202                                                         'R'.
CR9202         88  :FSSC:-REP-PAYEE-SPACE               VALUE ' '.
CR9202         88  :FSSC:-REP-PAYEE-R                   VALUE 'R'.
CR9634     10  :FSSC:-PA-REDUCT-BYPASS                  PIC X(01).
CR9634         88  :FSSC:-PA-REDUCT-BYPAS-VALID         VALUES ' '
CR9634                                                         'Y'.
CR9634         88  :FSSC:-PA-REDUCT-BYPAS-SPACE         VALUE ' '.
CR9634         88  :FSSC:-PA-REDUCT-BYPAS-Y             VALUE 'Y'.
CR9634     10  :FSSC:-ZPIC                              PIC X(01).
CR9634         88  :FSSC:-ZPIC-VALID                    VALUES ' '
CR9634                                                         'Y'.
CR9634         88  :FSSC:-ZPIC-SPACE                    VALUE ' '.
CR9634         88  :FSSC:-ZPIC-Y                        VALUE 'Y'.
CR9630     10  :FSSC:-MBI-SUBM-BENE-IND                  PIC X(01).
CR9630         88  :FSSC:-MBI-SUBM-BENE-HIC              VALUE 'H'.
CR9630         88  :FSSC:-MBI-SUBM-BENE-MBI              VALUE 'M'.
CR9630         88  :FSSC:-MBI-SUBM-BENE-SPACE            VALUE ' '.
CR9613     10  :FSSC:-PRAC-LOC.
CR9613         15  :FSSC:-PRAC-LOC-ADDR1                PIC X(55).
CR9613         15  :FSSC:-PRAC-LOC-ADDR2                PIC X(55).
CR9613         15  :FSSC:-PRAC-LOC-CITY                 PIC X(30).
CR9613         15  :FSSC:-PRAC-LOC-STATE                PIC X(2).
CR9613         15  :FSSC:-PRAC-LOC-ZIP                  PIC X(15).
CR9656     10  :FSSC:-ACO-ID                             PIC X(10).
CR9662     10  :FSSC:-PIP-PAY-AS-CASH                    PIC X(01).
CR9662         88  :FSSC:-PIP-PAY-AS-CASH-N              VALUE ' '.
CR9662         88  :FSSC:-PIP-PAY-AS-CASH-Y              VALUE 'Y'.
CR9826     10  :FSSC:-HH-NCVD-SK-SVC                     PIC X(01).
CR9826         88  :FSSC:-HH-NCVD-SK-SVC-N               VALUE 'N'.
CR9826         88  :FSSC:-HH-NCVD-SK-SVC-Y               VALUE 'Y'.
CR9858     10  :FSSC:-MBI-SUBM-MED-ID                   PIC X(12).
CR9858     10  :FSSC:-MBI-SUBM-MED-ID-IND               PIC X(01).
CR9858         88  :FSSC:-MBI-SUBM-MED-ID-YES            VALUE 'Y'.
CR9858         88  :FSSC:-MBI-SUBM-MED-ID-NO             VALUE 'N'.
CR9858         88  :FSSC:-MBI-SUBM-MED-ID-SPC            VALUE ' '.
CR9681     10  :FSSC:-ANSI-RMRK-CDS             OCCURS 04 TIMES.
CR9681         15  :FSSC:-ANSI-RMRK-CD                  PIC X(05).
CR9926     10  :FSSC:-COUNTY-CODE                       PIC 9(05).
CR9926     10  :FSSC:-COUNTY-CODE-X             REDEFINES
CR9926             :FSSC:-COUNTY-CODE                   PIC X(5).
CR9926     10  :FSSC:-PAYMENT-CBSA-CODE                 PIC X(05).
C10046     10  :FSSC:-ACOP-PCT-REDUCTN                  PIC S9(01)V99
C10046                                                  PACKED-DECIMAL.
CR9911     10  :FSSC:-QMB-IND                           PIC X(01).
CR9911         88  :FSSC:-QMB-IND-N                      VALUE ' '.
CR9911         88  :FSSC:-QMB-IND-Y                      VALUE 'Y'.
C10155     10  :FSSC:-ATT-PHYS-TAXO-CODE                PIC X(10).
C10231     10  :FSSC:-CWF-ADJ-REAS-CD                   PIC X(02).
C10231          88 :FSSC:-CWF-ADJ-RSN-ZW-DEBIT          VALUE 'ZW'.
C10288     10  :FSSC:-TDL-CR-CC                         PIC X(10).
C10456     10  :FSSC:-ADJ-MBI                           PIC X(11).
C10456     10  :FSSC:-ADJ-MBI-IND                       PIC X(01).
C10456         88  :FSSC:-ADJ-MBI-IND-HIC                VALUE 'H'.
C10456         88  :FSSC:-ADJ-MBI-IND-MBI                VALUE 'M'.
C10456         88  :FSSC:-ADJ-MBI-IND-SPACE              VALUE ' '.
C10456     10  :FSSC:-FIXED-FUTURE                      PIC X(2664).