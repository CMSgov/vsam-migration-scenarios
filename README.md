# Introduction

These scenarios describe difficult or awkward situations which a VSAM migration data solution must address to suit our needs. Please demonstrate or describe how your solution addresses these scenarios by default or through user configuration.

# Attachments

The following are copybooks which describe the logical and physical layouts of a claim in FISS.

Logical copybooks: FSRCCLMS (composed of 1xFSRCCLXS, 1xFSRCCLXB, 0-450xFSRCCLXL).

Physical copybooks: one logical claim includes exactly 1xFSRCCLMP and 0-25x FSRCCLMR.
FSRCCLMP is composed of 1xFSRCCLXS, 1xFSRCCLXB, and 0-10xFSRCCLXL.
FSRCCLMR is composed of 1xFSRCCLXS and 0-18xFSRCCLXL.

To compile these copybooks, please use the replacing option REPLACING ==:FSSC:== BY ==FSSCCLMS==.

## Scenario 1: 

Forward change-record-capture information into DynamoDB which represents time-ordered physical record changes (before any transformation happens). 

Then, in cases where a single logical update (e.g. updating one claim) causes many physical record updates (up to 26), your solution must be able to guarantee that all physical record updates have been captured before attempting a synthesis/transformation to merge the multiple physical records into a single logical entry in another database.

The layout of physical records in the VSAM file will vary based on information found in the VSAM record, so your product must be configurable to be able to use a variety of copybooks depending on the contents of the record.

## Scenario 2: 

Batch jobs will copy the contents of a VSAM file to a non-VSAM file and then transform the non-VSAM file. The VSAM file will then be deleted, redefined, and then re-initialized using data from the non-VSAM file. Demonstrate how your solution supports keeping the cloud database updated once a VSAM file has been deleted and redefined.

## Scenario 3:

The layout of VSAM records frequently changes, both on a regular schedule and on an ad-hoc basis. Demonstrate how your solution tracks copybook changes.

## Scenario 4:

Not all VSAM files in all CICS regions are targets for migration. Demonstrate how your solutions allows users to easily configure which VSAM files in which CICS regions are targets for capture.

## Scenario 5:

Demonstrate how your solution allows its extract-transform-load pipeline to be controlled as source code which can be controlled in a source control management system (rather than being configured exclusively through a graphical user interface). 

## Scenario 6:

Demonstrate how your solution allows users to define custom data validation rules at both the field and record level. 
