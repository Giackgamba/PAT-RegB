# PAT-RegB

This repository explores the pratical uses of SDMX.
Its purpose is to keep an always up-to-date and reliable method of dissemination about some indicators to be chosen.

It should, in intentions, show data, plots and possibly maps about some indicators and some EU regions in comparison with the Provincia Autonoma di Trento(ITH2).

-----------

For this to work you need to set up a DB -here we used MSQL Server, but others implementation should be easy enough-.

The DB structure must be as follow:
- tabIndicatori:
    - idDataFlow: Dataflow id
    - idSettore: Sector id to which Dataflow belongs
    - nome: Name of the Dataflow
    - descriz: Description of the indicator
    - concepts: Concept of the dataFlowas single string (e.g.: for DF 'demo_r_d2jan': 'FREQ.SEX.AGE')
- tabConcepts:
    - idConcept: Concept id
    - idDataFlow: Dataflow to which concept belongs
    - concept: Name of the concept (e.g.: 'FREQ' or 'AGE')
    - value: Value to be filtered on by the application
- tabSettori:
    - idSettore: Sector id
    - descriz: Description of the sector
    
The connection to the DB is done using RODBC package and a named DataOrigin. The connection itself is handled in an ignored file for security purpose.



GG - ISPAT