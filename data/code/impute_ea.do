clear all
set maxvar 15000

use ".././raw/folklore/Replication_Tables_Figures/Ethnographic_Atlas_Regressions_Ready.dta", clear
	   


************************************************************************************************************************************
**** APPENDIX FIGURES 6A-6D*********************************************************************************************************
************************************************************************************************************************************
**** APPENDIX FIGURE 6A
binscatter lncrops               v5 if motifs_total>4, absorb(continent_EA)  line(qfit) controls(lnyear_firstpub lnnmbr_title) ytitle(Ln(Share of Wheat, Rice and Maize Motifs in the Group's Folklore))  xtitle(Share of Subsistence from Agriculture)           title(Share of Motifs on Crops and % of Subsistence from Agriculture)            subtitle("Conditional on Baseline Controls and Continental FE") note("Baseline Controls: ln(# of Publications), ln(Year of First Publication)")
**** APPENDIX FIGURE 6B
binscatter lnpastoralism_related v4 if motifs_total>4, absorb(continent_EA)  line(qfit) controls(lnyear_firstpub lnnmbr_title) ytitle(Ln(Share of Motifs on Pastoralism in the Group's Folklore))         xtitle(Share of Subsistence from Animal Husbandry)      title(Share of Motifs on Pastoralism and % of Subsistence from Animal Husbandry) subtitle("Conditional on Baseline Controls and Continental FE") note("Baseline Controls: ln(# of Publications), ln(Year of First Publication)")
**** APPENDIX FIGURE 6C
binscatter lnfish_related        v3 if motifs_total>4, absorb(continent_EA)  line(qfit) controls(lnyear_firstpub lnnmbr_title) ytitle(Ln(Share of Motifs on Fish in the Group's Folklore))                xtitle(Share of Subsistence from Fishing)               title(Share of Fish Motifs and % of Subsistence from Fishing)                    subtitle("Conditional on Baseline Controls and Continental FE") note("Baseline Controls: ln(# of Publications), ln(Year of First Publication)")
**** APPENDIX FIGURE 6D
binscatter lnhunt_related       h_g if motifs_total>4, absorb(continent_EA)  line(qfit) controls(lnyear_firstpub lnnmbr_title) ytitle(Ln(Share of Motifs on Hunt in the Group's Folklore))                xtitle(Share of Subsistence from Hunting and Gathering) title(Share of Hunt Motifs and % of Subsistence from H/G)                        subtitle("Conditional on Baseline Controls and Continental FE") note("Baseline Controls: ln(# of Publications), ln(Year of First Publication)")
************************************************************************************************************************************

*******************************************************************************************************
********** Table IV: Folklore and High Gods************************************************************					
*******************************************************************************************************
   
areg lnpunishment_related   v34   $cont, cluster(v98) a(continent_EA)
est store k4
areg lnaward_related        v34   $cont, cluster(v98) a(continent_EA)
est store k5
areg lnsupernatural_related v34   $cont, cluster(v98) a(continent_EA)
est store k6
areg pca_highgods v34   $cont, cluster(v98) a(continent_EA)
est store k7
areg pca_highgods v34   $cont, cluster(v98) a(country_EA)
est store k8

estout k4 k5 k6 k7 k8, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(Adj. R-square)) keep(v34) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)

	   
	   
***********************************************************************************************************************************************************************************************************************************************************************************************************************
**** APPENDIX FIGURE 8A**********************************************************************************************************************************************************************************************************************************************************************************************
binscatter pca_highgods        v34   if motifs_total>4, line(qfit) controls(lnyear_firstpub lnnmbr_title) absorb(continent_EA)       ytitle(PCA on Share of Punish Award and Supernatural Motifs) xtitle(High Gods) title(PCA on Punishment Award Supernatural Motifs in the Oral Tradition and High Gods)   subtitle("Conditional on Continental FE and Baseline Controls")  note("Baseline Controls: ln(# of Publications) and ln(Year of First Publication)")
***********************************************************************************************************************************************************************************************************************************************************************************************************************
	   
*******************************************************************************************************
**********Appendix Table 4: Folklore, Family Structure, and Political Complexity
*******************************************************************************************************

global cont   lnyear_firstpub lnnmbr_title  if motifs_total>4

xi: areg lnmother_related extended $cont, cluster(v98) a(continent_EA)
est store k0
xi: areg lnmother_related extended $cont, cluster(v98) a(country_EA)
est store k1

areg lnking_related v33           $cont, cluster(v98) a(continent_EA)
est store k2
areg lnking_related v33           $cont, cluster(v98) a(country_EA)
est store k3
estout k0 k1 k2 k3, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(Adj. R-square)) keep(extended v33) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)
************************************************************************************************************************************

	   
************************************************************************************************************************************
**** APPENDIX FIGURES 7A-7B ********************************************************************************************************
************************************************************************************************************************************
**** APPENDIX FIGURE 7A
binscatter lnmother_related extended if motifs_total>4, line(qfit) controls(lnyear_firstpub lnnmbr_title) absorb(continent_EA)       ytitle(Ln (Share of Mother Motifs)) xtitle(Groups Organized Along Extended Family Lines) title(Mother-Related Motifs in the Oral Tradition and Family Organization)   subtitle("Conditional on Continental FE and Baseline Controls")  note("Baseline Controls: ln(# of Publications) and ln(Year of First Publication)")
**** APPENDIX FIGURE 7B
binscatter lnking_related      v33   if motifs_total>4, line(qfit) controls(lnyear_firstpub lnnmbr_title) absorb(continent_EA)       ytitle(Ln (Share of King Motifs)) xtitle(Degree of Political Complexity) title(King-Related Motifs in the Oral Tradition and Political Hierarchy)   subtitle("Conditional on Continental FE and Baseline Controls")  note("Baseline Controls: ln(# of Publications) and ln(Year of First Publication)")
************************************************************************************************************************************


g gr_comp=0 if v33==.
replace gr_comp=1 if v33!=.

reg  v33 lnking_related if motifs_total>4, cluster(v98)
predict pred_v33 if atlas!="" & motifs_total>4
sum pred_v33 if v33!=., d
sum pred_v33 if v33==., d
ttest pred_v33, by(gr_comp)

************************************************************************************************************************************
**** APPENDIX FIGURE 7C ****************************************
kdensity pred_v33 if v33==., addplot(kdensity pred_v33 if v33!=.)
************************************************************************************************************************************

************************************************************************************************************************************
***** Table III: Comparing Correlations Between the Sample where Ethnographic Values are Observed to those in the Imputed Sample****
************************************************************************************************************************************
***** PANEL A****
************************************************************************************************************************************
cor v33      h_g if v33!=.
cor pred_v33 h_g if v33==.

cor v33      v3 if v33!=.
cor pred_v33 v3 if v33==.

cor v33      v4 if v33!=.
cor pred_v33 v4 if v33==.

cor v33      v5 if v33!=.
cor pred_v33 v5 if v33==.

cor v33      agri_int if v33!=.
cor pred_v33 agri_int if v33==.

cor v33      class_strat if v33!=.
cor pred_v33 class_strat if v33==.

cor v33      v34 if v33!=.
cor pred_v33 v34 if v33==.

cor v33      v33 if v33!=.
//cor pred_v33 v33 if v33==.




g v34_comp=0       if v34==.
replace v34_comp=1 if v34!=.

reg  v34 pca_highgods if motifs_total>4, cluster(v98) 
predict pred_v34 if atlas!="" & motifs_total>4
sum pred_v34 if v34!=., d
sum pred_v34 if v34==., d
ttest pred_v34, by(v34_comp)

************************************************************************************************************************************
**** APPENDIX FIGURE 8B ****************************************
kdensity pred_v34 if v34==., addplot(kdensity pred_v34 if v34!=.)
************************************************************************************************************************************


************************************************************************************************************************************
***** Table III: Comparing Correlations Between the Sample where Ethnographic Values are Observed to those in the Imputed Sample****
************************************************************************************************************************************
***** PANEL B****
************************************************************************************************************************************

cor v34      h_g if v34!=.
cor pred_v34 h_g if v34==.

cor v34      v3 if v34!=.
cor pred_v34 v3 if v34==.

cor v34      v4 if v34!=.
cor pred_v34 v4 if v34==.

cor v34      v5 if v34!=.
cor pred_v34 v5 if v34==.

cor v34      agri_int if v34!=.
cor pred_v34 agri_int if v34==.

cor v34      class_strat if v34!=.
cor pred_v34 class_strat if v34==.

cor v34      v33 if v34!=.
cor pred_v34 v33 if v34==.

cor v34 v34 if v34!=.
*cor pred_v34 v34 if v34==.


*********************************************************************************************************************************
*************** Table V: Punishment of Tricksters in the Oral Traditions and Trust Today						
*************** PANEL C: ACROSS GROUPS; COLUMNS (3) - (6) *****************************************************************
*********************************************************************************************************************************
global cont0  lnyear_firstpub lnnmbr_title if motifs_total>4

areg v31 tricksters_punish $cont0             , cluster(v98) a(continent_EA)
est store c3
areg v31 tricksters_punish $cont0             , cluster(v98) a(country_EA)
est store c4
areg v31 tricksters_punish $cont0 & agridom==1, cluster(v98) a(continent_EA)
est store c5
areg v31 tricksters_punish $cont0 & agridom==1, cluster(v98) a(country_EA)
est store c6

estout c3 c4 c5 c6, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(Adj. R-square)) keep(tricksters_punish) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)
*********************************************************************************************************************************



********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
**** FIGURE Vc ****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
binscatter v31 tricksters_punish if motifs_total>4,  absorb(continent_EA) line(qfit) controls(lnyear_firstpub lnnmbr_title) ytitle("Mean Size of Local Community in EA") xtitle(Relative Frequency of Motifs where Antisocial Behavior is Explicitly Punished versus Not) title(Tricksters in Folklore and Historical Development across Groups) subtitle("Conditional on Baseline Controls and Continental FE") note("MTurk classification into trickster punished or not of motifs tagged by the concepts 'cheat' 'deceive' and 'trick'")
********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

*********************************************************************************************************************************
*************** Table VII: Table VII: Gender Norms across Oral Traditions and the Role of Women Today and Historically***********						
*************** PANEL C: ACROSS EA GROUPS; COLUMNS (3) - (6) ********************************************************************
*********************************************************************************************************************************
global cont0  lnyear_firstpub lnnmbr_title if motifs_total>4

 reg malebias plow      $cont0, cluster(v98)
est store a1  
areg malebias plow      $cont0, cluster(v98) a(continent_EA)
est store a2  
areg malebias plow      $cont0, cluster(v98) a(country_EA)
est store a3
 reg malebias male_more $cont0, cluster(v98) 
est store a4  
areg malebias male_more $cont0, cluster(v98) a(continent_EA)
est store a5  
areg malebias male_more $cont0, cluster(v98) a(country_EA)
est store a6

estout a4 a5 a6 a1 a2 a3, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(Adj. R-square)) keep(plow male_more) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)
*********************************************************************************************************************************


