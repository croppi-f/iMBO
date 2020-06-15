# imlMBO
This repo contains tools to interpret Model Based Optimization processes

* **Folder R**
  + tools for the interpretation and internal subfunctions.
  + PredictorAf is used for FeatureEffectMBO(), is a container for the AF and the candidate points. It has a general structure and might be therefore used also in combination with other iml tools. Yet, it is not tested for such cases.
  + utils_fembo_inflInst.R contains different subfunctions used to run FeatureEffectMBO() and inflInst().
  + utils_xplxpl is a folder containing the relevant modified (sub)functions of mlrMBO as well as other subfunctions to compute xplxpl(). These function allow the storage of the candidate points.
  
* **Folder tests**
  + testthat for the tests
  + test_data are the data sets used for the tests (Materials_Design and Kapton)

* **Shiny_App**
  + Shiny App for interactive visualization