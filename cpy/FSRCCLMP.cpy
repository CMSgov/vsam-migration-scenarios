      ********************** | ****************************************
      ***                                                             *
      *** MEDICARE PART A                                             *
      *** FISS SYSTEM DOCUMENTATION                                   *
      ***                                                             *
      *|* MEMBER: FSRCCLMP                                            *
      ***                                                             *
      *** DESCRIPTION: ONLINE CLAIM PHYSICAL RECORD (00 SEQUENCE)     *
      ***                                                             *
      *** VARIABLE LRECL: 27,250 = 12,250 BASE + (1,500 X 10 LINES)   *
      ***                                                             *
      *****************************************************************
      ***                                                             *
      *** ANY CHANGES TO THE FOLLOWING COPYBOOKS MUST BE MADE TO ALL: *
      ***   `CLMB, `CLML, `CLMP, `CLMQ, `CLMR, `CLMS, `CLMX,          *
      ***   'CLMY, `IDRC, `PDCL, `PDCP, `PDCR, `259R, SFSPDCL.        *
      *** ANY CHANGES TO THE SUMMARY PORTION FOR THE CLAIM FILE       *
      *** (FIRST 250 BYTES) WILL REQUIRE FSSCHIST ALSO BE CHANGED.    *
      ***                                                             *
      *** TO USE THIS COPYBOOK FOR AN ONLINE CLAIM FILE:              *
      ***                                                             *
      ***     REPLACE ==:FSSC:==      BY ==FSSCCLMP==.                *
      ***     COPY FSRCCLMP.                                          *
      ***     REPLACE OFF.                                            *
      ***                                                             *
      ***$************************************************************$

       01  :FSSC:-CLAIM-RECORD.

         05  :FSSC:-CLAIM-SEGMENT.

      *** SUMMARY RECORD STARTS HERE ***********************************

          07  :FSSC:-CLAIM-BASE-PART1.
         COPY FSRCCLXS.

      *** REST OF BASE RECORD STARTS HERE ******************************

          07  :FSSC:-CLAIM-BASE-PART2.
         COPY FSRCCLXB.

      *** REVENUE LINE STARTS HERE *************************************

         05  :FSSC:-LINES-SEGMENT.
           10  :FSSC:-CLM-LINES                 OCCURS 0 TO 10 TIMES
               DEPENDING ON :FSSC:-LINES
               INDEXED   BY :FSSC:-NDX.

         COPY FSRCCLXL.
