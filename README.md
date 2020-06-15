# imlMBO
This repo contains tools to interpret Model Based Optimization processes

* **Folder R**
  + tools for the interpretation and internal subfunctions.
  + PredictorAf is used for FeatureEffectMBO(), is a container for the AF and the candidate points. It has a general structure and might be therefore used also in combination with other iml tools. Yet, it is not tested for such cases.
  + utils_fembo_inflInst.R contains different subfunctions used to run FeatureEffectMBO() and inflInst().
  
* **Folder tests**
  + tests for the functions

* **Shiny_App**
  + Shiny App for interactive visualization