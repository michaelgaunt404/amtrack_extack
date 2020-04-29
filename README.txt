#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a generic README
#
# By: mike gaunt, michael.gaunt@wsp.com
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


PURPOSE OF THIS DOCUMENT
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
The project is currently on hold. 
Everything to date as been catalogued. 
Folders have been cleaned of unrequired files/folders.
The project has been pushed to GIT::: https://github.com/michaelgaunt404/amtrack_extack

HOW TO USE
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
Everything in this folder is self contained. 

IMPORTANT ITEMS:::::
amtrak_analysis_lightweight.R
|-> main script for the ananlysis
|-> most up-to-date
|-> intended to be lightweight
|-------> RMD files were becoming very cumbersome to work with
|-------> RMD files should pull from this
|-> perfroms main data import, munging, some vis (not main intent), record error correction 
|-> record error correction 
|-------> good and bad match records for DATES complete
|--------------> Some dates require so more effort to fix and were flagged and removed
|--------------> writes to ./output/data/corrected_records folder
|--------------> writes to ./output/data/outstanding folder
|-------> good and bad match records for all other records types are compelete-ish 
|-------> should likely be vetted before deployement

amtrak_extract_MD.RMD
|-> full analysis report
|-> least up-to-date
|-> covers all of mehtodology to reporting
|-------> does not cover fixes
|-> this is a very heavy RMD

amtrak_short_report.RMD
|-> a breif summary report report
|-> pretty up-to-date
|-> covers only reporting and some outlined fixes

amtrak_extract.RMD
|-> builds look up tables required for the analysis
|-> pretty up-to-date
|-> not related to amtrak_extract_MD.RMD in any way
|-> depends on whats inside the data folder (dont touch that folder)

General Notes:::::
The data folder should not be touched.
 

