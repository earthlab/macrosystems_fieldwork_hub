# Process Macrosystems UAS Data

This workflow describes how to process UAS data for the Macrosystems project. It is a more specific version of the [Earth Lab metashape workflow](https://github.com/earthlab/el-drones/blob/master/docs/03_post-mission_agisoft_metashape_workflow.md), which was developed by Megan Cattau from a guide posted in this [Agisoft forum post](https://www.agisoft.com/forum/index.php?topic=7851.0). Also see Megan Cattau's [UASWorkflows repo](https://github.com/mcattau/UASWorkflows).

## Access CU Research Computing via OnDemand
* Go to CU OnDemand: https://ondemand.rc.colorado.edu/
* Sign in - If you do not have access to CURC, see below ("Getting CURC Access")
Start desktop (top nav bar, “Interactive Apps”, “Core desktop”)
Set the number of hours you expect to use the desktop (over-estimate!)
Set the number of cores, either 8 or 16 usually
Wait for the job to start. It will be ‘Queued’ for a little while, exact time totally depends on current demand.
Open the Terminal and type the following commands:
source ~/.metashape
metashape

### Getting CURC Access
If the user has a CU Identikey, they can just sign up for an account as normal at [https://rcamp.rc.colorado.edu/accounts/account-request/create/verify/ucb]. Once they have an account they can be added to any user groups/accounts on the system that they need to access Earthlab software/files. Have the Macrosystems project manager email Research Computing Help (rc-help@colorado.edu) and ask for them to have access to Earth Lab petalibrary files and Metashape.
 
If the user does not have an Identikey, you can request one through the CU POI or Research Affiliate process ([see Earth Lab's guide to this process](https://github.com/earthlab/earth-lab-operations/wiki/CU-Research-Affiliates-&-Persons-of-Interest-(POIs))). Once that process is finished, the user can sign up for a CURC account per the above instructions.
