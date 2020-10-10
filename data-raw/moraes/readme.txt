INTRODUCTION
------------
* This CSV dataset (AMD_baseline_MEH_v1.csv) is associated with the paper Moraes et al. Quantitative analysis of optical coherence tomography for neovascular age-related macular degeneration using deep learning. Ophthalmology. (2020). Available at: https://doi.org/10.1016/j.ophtha.2020.09.025
* The dataset comprises anonymised metadata and OCT segmentation data of patients undergoing treatment for wet AMD at Moorfields Eye Hospital, London, United Kingdom. The dataset includes 2473 first-treated eyes, and 493 second-treated eyes

DATA FIELDS
-----------
ID				Anonymous ID - an integer between 1 and 2967
DaysSinceBaseline		Replaces visit dates of patient. Baseline is denoted as '0' for the day the first eye was treated with an injection
First_or_Second_Treated_Eye	Indicating if it is the first-treated eye or the second-treated eye of that patient to receive injection
Eye				Left or Right
FirstTreated			The eye that first received an injection (Left or Right)
Gender				Male or female
Age_grouped			Age groups of 50-59, 60-69, 70-79, >80, or NULL if unknown.
Ethnicity_grouped		Ethnic groups combined into: 'White', 'Black', 'Asian', 'Other or unknown'
InjectionGiven			TRUE or FALSE, whether an injection was given on visit date
InjectionNumber			Number of the injection, or NULL if InjectionGiven==FALSE. In this baseline dataset, the number is always equal to 1 if the injection is given
VA_ETDRS			Visual acuity measurement in number of ETDRS letters, or NULL if unknown
VAUnder1Letter			Where visual acuity is under 1 letter, if this is hand movements (HM), counting fingers (CF), (no) perception of light (N)PL, or NULL if visual acuity was greater than 1 letter
oct_shape			Dimensions of the OCT volume image. 128 is the number of slices, 885 is the height and 512 is the width
segmentation_voxel_size_um	The dimensions in microns of each voxel
Neurosensory_volume_voxels	Number of voxels segmented as the feature neurosensory retina (NSR)
RPE_volume_voxels		Number of voxels segmented as the feature retinal pigment epithelium (RPE)
IRF_volume_voxels		Number of voxels segmented as the feature intraretinal fluid (IRF)
SRF_volume_voxels		Number of voxels segmented as the feature subretinal fluid (SRF)
SHRM_volume_voxels		Number of voxels segmented as the feature subretinal hyperreflective material (SHRM)
Drusen_volume_voxels		Number of voxels segmented as the feature drusen
sPED_volume_voxels		Number of voxels segmented as the feature serous pigment epithelium detachment (sPED)
fvPED_volume_voxels		Number of voxels segmented as the feature fibrovascular pigment epithelium detachment (fvPED)
HRF_volume_voxels		Number of voxels segmented as the feature hyperreflective foci (HRF)
Neurosensory_thickness_um	Thickness (measured in μm) of the segmented feature neurosensory retina (NSR)
IRF_thickness_um		Thickness (measured in μm) of the segmented feature intraretinal fluid (IRF)
SRF_thickness_um		Thickness (measured in μm) of the segmented feature subretinal fluid (SRF)
SHRM_thickness_um		Thickness (measured in μm) of the segmented feature subretinal hyperreflective material (SHRM)
HRF_thickness_um		Thickness (measured in μm) of the segmented feature hyperreflective foci (HRF)
CST_um				Central subfield retinal thickness measured as the sum of NSR, IRF, SRF, SHRM, HRF thickness (measured in μm)

NOTES
-----
* Each voxel equates to 2.60 x 11.72 x 47.24 μm in the A-scan, B-scan, and C-scan directions, respectively
* Each eye included in the dataset is given a unique ID. To ensure anonymisation, it is not possible to link a patient’s two eyes if the individual has both eyes in the dataset
* In this published dataset, only one image is available per eye. As per the methodology outlined in the paper associated with this dataset, the image with the lowest segmented artifacts was chosen if multiple scans were available at the same visit
* For first-treated eyes, DaysSinceBaseline will generally equal 0. Where a scan at day 0 was not available, a scan up to 14 days prior to the first injection (i.e. up to -14 days), is used for analysis
* All OCT data is captured using 3DOCT-2000 devices (Topcon Corp., Tokyo, Japan). All images comprise 512*885*128 voxels covering a volume of 6x6x2.3mm
* Segmentation data was output using a deep learning segmentation model described further in De Fauw et al. Clinically applicable deep learning for diagnosis and referral in retinal disease. Nature Medicine (2018); and Yim and Chopra et al. Predicting conversion to wet age-related macular degeneration using deep learning. Nature Medicine. (2020)