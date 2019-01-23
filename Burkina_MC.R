

setwd("/Users/macbook/Publications/17_Burkina_Vignette")

library(decisionSupport)
#library(DAutilities) #compound_figure AND empirical_EVPI ARE ONLY IN DAutilities



sediment_calc<-function(x, varnames)
{
  
  ### 4 ex-post risks, impacts on the benefits ###
  HazardEvent<-chance_event(NaturHazard,1,0,n=n_years)
  BadMaintEvent<-chance_event(BadMaintenance,1,0,n=n_years)
  BadDesignEvent<-chance_event(BadDesign,1,0,n=n_years, one_draw = TRUE)
  
  Hazard_scaling_irrig_area<-1-HazardEvent*vv(Hazard_reduction_irrigated_area,var_CV,n=n_years)/100
  BadMaint_scaling_irrig_area<-1-BadMaintEvent*vv(BadMaint_reduction_irrigated_area,var_CV,n=n_years)/100
  BadDesign_scaling_irrig_area<-1-BadDesignEvent*vv(BadDesign_reduction_irrigated_area,var_CV,n=n_years)/100
  
  
  ### 3 ex-ante risks, impacts on the implementation of interventions ###
  dredge_NonPopInvolvEvent<-chance_event(dredge_NonPopInvolv,1,0,n=1)
  dredge_NonDonorsInvolvEvent<-chance_event(dredge_NonDonorsInvolv,1,0,n=1)
  
  check_NonPopInvolvEvent<-chance_event(check_NonPopInvolv,1,0,n=1)
  check_NonInstInvolvEvent<-chance_event(check_NonInstInvolv,1,0,n=1)
  check_NonDonorsInvolvEvent<-chance_event(check_NonDonorsInvolv,1,0,n=1)
  
  buffer_NonPopInvolvEvent<-chance_event(buffer_NonPopInvolv,1,0,n=1)
  buffer_NonInstInvolvEvent<-chance_event(buffer_NonInstInvolv,1,0,n=1)
  buffer_NonDonorsInvolvEvent<-chance_event(buffer_NonDonorsInvolv,1,0,n=1)
  
  
  ##calculation of common random draws for all intervention model runs
  
  TLU<-vv(TLU_no_buffer,var_CV,n_years)
  TLU_profit<-vv(profit_per_TLU,var_CV,n_years)
  
  precalc_buffer_fruit_benefits<-vv(buffer_fruit_area_ha,var_CV,n_years)*
    vv(buffer_fruit_yield_t_ha,var_CV,n_years)*
    vv(buffer_fruit_profit_USD_t,var_CV,n_years)
  precalc_buffer_vegetable_benefits<-vv(buffer_vegetable_area_ha,var_CV,n_years)*
    vv(buffer_vegetable_yield_t_ha,var_CV,n_years)*
    vv(buffer_vegetable_profit_USD_t,var_CV,n_years)
  precalc_buffer_rainfed_crop_benefits<-vv(buffer_rainfed_crop_area_ha,var_CV,n_years)*
    vv(buffer_rainfed_crop_yield_t_ha,var_CV,n_years)*
    vv(buffer_rainfed_crop_profit_USD_t,var_CV,n_years)                                 
  precalc_scheme2_vegetable_yield_t_ha<-vv(scheme2_vegetable_yield_t_ha,var_CV,n_years)
  precalc_scheme2_vegetable_profit_USD_t<-vv(scheme2_vegetable_profit_USD_t,var_CV,n_years)
  
  precalc_scheme2_rice_yield_t_ha<-vv(scheme2_rice_yield_t_ha,var_CV,n_years)
  precalc_scheme2_rice_profit_USD_t<-vv(scheme2_rice_profit_USD_t,var_CV,n_years)
  
  precalc_irrigation_scheme_vegetable_yield_t_ha<-vv(irrigation_scheme_vegetable_yield_t_ha,var_CV,n_years)
  precalc_irrigation_scheme_vegetable_profit_USD_t<-vv(irrigation_scheme_vegetable_profit_USD_t,var_CV,n_years)
  
  precalc_irrigation_scheme_rice_yield_t_ha<-vv(irrigation_scheme_rice_yield_t_ha,var_CV,n_years)
  precalc_irrigation_scheme_rice_profit_USD_t<-vv(irrigation_scheme_rice_profit_USD_t,var_CV,n_years)
  
  precalc_proportion_irrigation_scheme_rice<-vv(proportion_irrigation_scheme_rice,var_CV,n_years)  
  
  precalc_fish_hazards<-HazardEvent*vv(Hazard_reduction_fish_perc/100,var_CV,n=n_years)
  precalc_current_fish_value<-vv(current_annual_fish_value_USD,var_CV,n_years)
  
  
  for (decision_dredging in c(FALSE,TRUE))
    for (decision_check_dams in c(FALSE,TRUE))
      for (decision_buffer_strips in c(FALSE,TRUE))
      {
        ###  Intervention 1: dredging ###
        if(decision_dredging)
        {dredging<-TRUE
        dredging_PlanningCost<-TRUE
        dredging_Cost<-TRUE} else
        {dredging<-FALSE
        dredging_PlanningCost<-FALSE
        dredging_Cost<-FALSE}
        
        if (dredge_NonPopInvolvEvent){ dredging<-FALSE ; dredging_Cost<-FALSE}
        # Non institutional involvement is assumed to have no  effect  #
        if (dredge_NonDonorsInvolvEvent){ dredging<-FALSE ; dredging_Cost<-FALSE ; dredging_PlanningCost<-FALSE}
        
        ###  Intervention 2: check_dams ###
        if(decision_check_dams)
        {check_dams<-TRUE
        check_dams_PlanningCost<-TRUE
        check_dams_Cost<-TRUE} else
        {check_dams<-FALSE
        check_dams_PlanningCost<-FALSE
        check_dams_Cost<-FALSE}
        
        if (check_NonPopInvolvEvent){check_dams<-FALSE ; check_dams_Cost<-FALSE}
        if (check_NonInstInvolvEvent){check_dams<-FALSE ; check_dams_Cost<-FALSE}
        if (check_NonDonorsInvolvEvent){check_dams<-FALSE ; dredging_Cost<-FALSE ; check_dams_PlanningCost<-FALSE}
        
        ###  Intervention 3: buffer_strips ###
        if(decision_buffer_strips)
        {buffer_strips<-TRUE
        buffer_strips_PlanningCost<-TRUE
        buffer_strips_Cost<-TRUE} else
        {buffer_strips<-FALSE
        buffer_strips_PlanningCost<-FALSE
        buffer_strips_Cost<-FALSE}
        
        if (buffer_NonPopInvolvEvent){buffer_strips<-FALSE ; buffer_strips_Cost<-FALSE}
        if (buffer_NonInstInvolvEvent){buffer_strips<-FALSE ; buffer_strips_Cost<-FALSE}
        if (buffer_NonDonorsInvolvEvent){buffer_strips<-FALSE ; buffer_strips_Cost<-FALSE ; buffer_strips_PlanningCost<-FALSE}
        
        
        ###Costs
        if(dredging_Cost) {cost_dredging<-dredging_supervision_cost+dredging_admin_cost+dredging_transport_cost+
          dredging_culvert_supervision_cost
        } else cost_dredging<-0 
        if(check_dams_Cost) {cost_check_dams<-check_supervision_cost+check_training_cost+check_tech_devices_cost+check_material_cost+
          check_rocks_cost+check_transport_cost
        } else cost_check_dams<-0 
        if(buffer_strips_Cost) {cost_buffer_strips<-buffer_adaptation_cost+buffer_tech_devices_cost+buffer_nursery_cost+buffer_wells_cost+
          buffer_training_cost+buffer_mngmt_oprt_cost+buffer_mngmt_follow_cost+buffer_mngmt_audit_cost
        } else cost_buffer_strips<-0 
        
        if(dredging_PlanningCost) {plan_cost_dredging<-dredging_study_cost+dredging_communication_cost+dredging_culvert_feasibility_cost
        } else plan_cost_dredging<-0 
        if(check_dams_PlanningCost) {plan_cost_check_dams<-check_location_cost+check_feasibility_cost+check_topobatymetry_cost+check_communication_cost
        } else plan_cost_check_dams<-0
        if(buffer_strips_PlanningCost) {plan_cost_buffer_strips<-buffer_communication_cost+buffer_zoning_cost
        } else plan_cost_buffer_strips<-0 
        
        maintenance_cost<-rep(0,n_years)
        
        if(check_dams) maintenance_cost<-maintenance_cost+vv(maintenance_check_dams,var_CV,n_years)
        
        if(buffer_strips) maintenance_cost<-maintenance_cost+vv(maintenance_buffer_strips,var_CV,n_years)
        
        intervention_cost<-maintenance_cost
        intervention_cost[1]<-intervention_cost[1]+cost_dredging+cost_check_dams+cost_buffer_strips+
          plan_cost_dredging+plan_cost_check_dams+plan_cost_buffer_strips
        
        ###irrigation scheme 1 - area decline and delay by interventions
        gompertz_time1_time_until_irrigated_area_declines<-sum(c(baseline_time_until_irrig_area_declines,
                                                                 dredging*dredging_delay_of_irrig_area_decline,
                                                                 check_dams*check_dam_delay_of_irrig_area_decline,
                                                                 buffer_strips*buffer_strip_delay_of_irrig_area_decline))
        gompertz_time2_time_until_irrigated_area_halved<-sum(c(gompertz_time1_time_until_irrigated_area_declines,
                                                               baseline_start_losses_to_half_irrig_area_lost,
                                                               dredging*dredging_delay_of_irrig_area_halved,
                                                               check_dams*check_dam_delay_of_irrig_area_halved,
                                                               buffer_strips*buffer_strip_delay_of_irrig_area_halved))
        
        irrig_scheme1_area_share<-1-gompertz_yield(max_harvest=1,
                                                   time_to_first_yield_estimate=gompertz_time1_time_until_irrigated_area_declines,
                                                   time_to_second_yield_estimate=gompertz_time2_time_until_irrigated_area_halved,
                                                   first_yield_estimate_percent=10,
                                                   second_yield_estimate_percent=50, n_years=n_years, var_CV = 0,
                                                   no_yield_before_first_estimate = TRUE)
        
        irrig_scheme1_area_share<-irrig_scheme1_area_share[1:30]
        
        irrig_scheme1_area<-current_irrig_area*irrig_scheme1_area_share
        
        ###irrigation scheme 1 - risk of blockage and pipe clearing
        
        gompertz_time2_time_until_pipe_blockage_occurs_every_second_year<-sum(c(baseline_time_until_pipes_blocked_every_second_year,
                                                                                dredging*dredging_delay_of_pipes_blocked_every_second_year,
                                                                                check_dams*check_dam_delay_of_pipes_blocked_every_second_year,
                                                                                buffer_strips*buffer_strip_delay_of_pipes_blocked_every_second_year))
        
        risk_blockage<-gompertz_yield(max_harvest=1,
                                      time_to_first_yield_estimate=0,
                                      time_to_second_yield_estimate=gompertz_time2_time_until_pipe_blockage_occurs_every_second_year,
                                      first_yield_estimate_percent=100*current_risk_of_pipe_blockage,
                                      second_yield_estimate_percent=50,
                                      n_years=n_years, var_CV = 0,
                                      no_yield_before_first_estimate = TRUE)
        risk_blockage[which(risk_blockage>1)]<-1
        risk_blockage[which(risk_blockage<0)]<-0
        
        gompertz_time2_time_until_chance_cleared_50percent<-sum(c(baseline_time_until_chance_cleared_50percent,
                                                                  dredging*dredging_delay_of_time_until_chance_cleared_50percent,
                                                                  check_dams*check_dam_delay_of_time_until_chance_cleared_50percent,
                                                                  buffer_strips*buffer_strip_delay_of_time_until_chance_cleared_50percent))
        
        chance_cleared<-gompertz_yield(max_harvest=1,
                                       time_to_first_yield_estimate=0,
                                       time_to_second_yield_estimate=gompertz_time2_time_until_chance_cleared_50percent,
                                       first_yield_estimate_percent=100*current_chance_of_blocked_pipe_cleared,
                                       second_yield_estimate_percent=50,
                                       n_years=n_years, var_CV = 10,
                                       no_yield_before_first_estimate = TRUE)
        chance_cleared[which(chance_cleared>1)]<-1
        chance_cleared[which(chance_cleared<0)]<-0
        
        
        
        ### Creation of intermediate variables: irrigation area potentially irrigated given the risk of pipe blockage (in agricultural development) ###
        
        ### Risk that pipes are blocked/cleared given the time period ###
        
        ### Irrigated area given the risk of pipe blockage (in agricultural development) ###
        
        pipe_clogging<-sapply(1:n_years,function(x) rbinom(1,1,risk_blockage[x]))
        pipe_cleared<-sapply(1:n_years,function(x) rbinom(1,1,chance_cleared[x]))
        
        pipe_blocked <- pipe_clogging
        for (i in 2:length(pipe_blocked))
          if (pipe_clogging[i] == 0) 
            if (pipe_blocked[i - 1] == 1) 
              if (!pipe_cleared[i] == 1) pipe_blocked[i] <- 1
        
        irrig_scheme1_irrigated_area_ex_ante<-irrig_scheme1_area*(1-pipe_blocked*vv(pipe_blocked_area_lost_perc/100,var_CV,n_years))
        
        
        ### Impact of ex-post risks on irrigated area###
        irrigated_area_scheme1<-irrig_scheme1_irrigated_area_ex_ante*
          Hazard_scaling_irrig_area*BadMaint_scaling_irrig_area*
          BadDesign_scaling_irrig_area #*NonCompli_scaling_irrig_area
        
        
        ### Benefits from rice cultivation in the shore of the reservoir (==0 if buffer strips implemented) ###
        if (buffer_strips)
          buffer_strip_cultivation<-TRUE else buffer_strip_cultivation<-FALSE
        
        scheme2_time_until_benefits_gone<-scheme2_time_until_dredging_benefits_gone_baseline+
          check_dams*check_dams_added_scheme2_area_benefit_time
        
        scheme2_area_scaler<-gompertz_yield(max_harvest=1,
                                            time_to_first_yield_estimate=1,
                                            time_to_second_yield_estimate=scheme2_time_until_benefits_gone,
                                            first_yield_estimate_percent=100,
                                            second_yield_estimate_percent=0,
                                            n_years=n_years, var_CV = 0,
                                            no_yield_before_first_estimate = TRUE)
        scheme2_area_ha<-scheme2_area_no_dredging_ha*(1+
                                                        dredging*scheme2_area_scaler*dredging_bump_scheme2_area_perc/100)
        
        #rice area remains unchanged (except dredging bump), because the 
        #water comes from very close to where it's needed in the rainy season.
        scheme2_rice_benefits<-as.numeric(!buffer_strips)*
          vv(scheme2_area_ha,var_CV,n_years)*
          precalc_scheme2_rice_yield_t_ha*
          precalc_scheme2_rice_profit_USD_t
        
        
        
        gompertz_time1_time_until_irrigated_area2_declines<-sum(c(baseline_time_until_irrig_area2_declines,
                                                                  dredging*dredging_delay_of_irrig_area2_decline,
                                                                  check_dams*check_dam_delay_of_irrig_area2_decline))
        gompertz_time2_time_until_irrigated_area2_halved<-sum(c(gompertz_time1_time_until_irrigated_area2_declines,
                                                                baseline_start_losses_to_half_irrig_area2_lost,
                                                                dredging*dredging_delay_of_irrig_area2_halved,
                                                                check_dams*check_dam_delay_of_irrig_area2_halved))
        
        irrig_scheme2_area_share<-1-gompertz_yield(max_harvest=1,
                                                   time_to_first_yield_estimate=gompertz_time1_time_until_irrigated_area2_declines,
                                                   time_to_second_yield_estimate=gompertz_time2_time_until_irrigated_area2_halved,
                                                   first_yield_estimate_percent=0,
                                                   second_yield_estimate_percent=50, n_years=n_years, var_CV = 0,
                                                   no_yield_before_first_estimate = TRUE)
        
        scheme2_vegetable_area_ha<-scheme2_area_ha*irrig_scheme2_area_share
        
        scheme2_vegetable_benefits<-as.numeric(!buffer_strips)*
          vv(scheme2_vegetable_area_ha,var_CV,n_years)*
          precalc_scheme2_vegetable_yield_t_ha*
          precalc_scheme2_vegetable_profit_USD_t
        
        buffer_fruit_benefits<-as.numeric(buffer_strips)*precalc_buffer_fruit_benefits
        buffer_vegetable_benefits<-as.numeric(buffer_strips)*precalc_buffer_vegetable_benefits
        buffer_rainfed_crop_benefits<-as.numeric(buffer_strips)*precalc_buffer_rainfed_crop_benefits
        
        
        rainy_season_rice_area_scheme1<-irrigated_area_scheme1*precalc_proportion_irrigation_scheme_rice
        rainy_season_vegetable_area_scheme1<-irrigated_area_scheme1-rainy_season_rice_area_scheme1
        
        irrigation_season_rainy_season_benefits_scheme1<-rainy_season_rice_area_scheme1*
          precalc_irrigation_scheme_rice_yield_t_ha*
          precalc_irrigation_scheme_rice_profit_USD_t+
          rainy_season_vegetable_area_scheme1*
          precalc_irrigation_scheme_vegetable_yield_t_ha*
          precalc_irrigation_scheme_vegetable_profit_USD_t
        
        irrigation_season_dry_season_benefits_scheme1<-irrigated_area_scheme1*
          precalc_irrigation_scheme_vegetable_yield_t_ha*
          precalc_irrigation_scheme_vegetable_profit_USD_t
        
        ### Total benefits from crop production (agricultural development and riparian zone) ###     
        crop_production<-scheme2_rice_benefits+
          scheme2_vegetable_benefits+
          buffer_fruit_benefits+
          buffer_vegetable_benefits+
          buffer_rainfed_crop_benefits+
          irrigation_season_rainy_season_benefits_scheme1+
          irrigation_season_dry_season_benefits_scheme1
        
        ### Benefits from fishing ###      
        
        ### Impact of interventions on fish population ###
        time_to_start_fish_decline<-sum(c(time_to_start_fish_decline_baseline,
                                          dredging_delay_start_fish_decline,
                                          check_dams_delay_start_fish_decline,
                                          buffer_strips_delay_start_fish_decline))
        
        time_to_fish_population_halved<-sum(c(time_to_start_fish_decline,
                                              time_to_halve_fish_population_baseline,
                                              dredging_delay_in_time_to_halve_fish_population,
                                              check_dams_delay_in_time_to_halve_fish_population,
                                              buffer_strips_delay_in_time_to_halve_fish_population))
        
        fish_benefit_scaler<-1-gompertz_yield(max_harvest=1,
                                              time_to_first_yield_estimate=time_to_start_fish_decline,
                                              time_to_second_yield_estimate=time_to_fish_population_halved,
                                              first_yield_estimate_percent=0,
                                              second_yield_estimate_percent=50, n_years=n_years, var_CV = 10,
                                              no_yield_before_first_estimate = TRUE)
        
        risk_adjusted_fish_benefits<-fish_benefit_scaler*(1-precalc_fish_hazards)
        
        ### Fish benefits ###
        Fish_benefits<-precalc_current_fish_value*risk_adjusted_fish_benefits
        
        ### Benefits from livestock ###   
        # The following allows considering that buffer strips may
        # restrict access to the reservoir for livestock.
        
        if(buffer_strips)  TLU_intervention<-TLU*(1+change_TLU_buffer_perc/100) else TLU_intervention<-TLU
        
        livestock_benefits<-TLU_intervention*TLU_profit
        
        total_benefits<-crop_production+Fish_benefits+livestock_benefits
        
        net_benefits<-total_benefits-intervention_cost
        
        if(decision_dredging & decision_check_dams & decision_buffer_strips) result_dredge_check_buff<-net_benefits
        if(decision_dredging & decision_check_dams & !decision_buffer_strips) result_dredge_check_nbuff<-net_benefits
        if(decision_dredging & !decision_check_dams & decision_buffer_strips) result_dredge_ncheck_buff<-net_benefits
        if(decision_dredging & !decision_check_dams & !decision_buffer_strips) result_dredge_ncheck_nbuff<-net_benefits
        if(!decision_dredging & decision_check_dams & decision_buffer_strips) result_ndredge_check_buff<-net_benefits
        if(!decision_dredging & decision_check_dams & !decision_buffer_strips) result_ndredge_check_nbuff<-net_benefits
        if(!decision_dredging & !decision_check_dams & decision_buffer_strips) result_ndredge_ncheck_buff<-net_benefits
        if(!decision_dredging & !decision_check_dams & !decision_buffer_strips) result_ndredge_ncheck_nbuff<-net_benefits
        
      } #close intervention loop bracket
  
  NPV_dredge_check_buff<-discount(result_dredge_check_buff,discount_rate,calculate_NPV = TRUE)
  NPV_dredge_check_nbuff<-discount(result_dredge_check_nbuff,discount_rate,calculate_NPV = TRUE)
  NPV_dredge_ncheck_buff<-discount(result_dredge_ncheck_buff,discount_rate,calculate_NPV = TRUE)
  NPV_dredge_ncheck_nbuff<-discount(result_dredge_ncheck_nbuff,discount_rate,calculate_NPV = TRUE)
  NPV_ndredge_check_buff<-discount(result_ndredge_check_buff,discount_rate,calculate_NPV = TRUE)
  NPV_ndredge_check_nbuff<-discount(result_ndredge_check_nbuff,discount_rate,calculate_NPV = TRUE)
  NPV_ndredge_ncheck_buff<-discount(result_ndredge_ncheck_buff,discount_rate,calculate_NPV = TRUE)
  NPV_ndredge_ncheck_nbuff<-discount(result_ndredge_ncheck_nbuff,discount_rate,calculate_NPV = TRUE)
  
  
  return(list(NPV_dredge_check_buff=NPV_dredge_check_buff-NPV_ndredge_ncheck_nbuff,
              NPV_dredge_check_nbuff=NPV_dredge_check_nbuff-NPV_ndredge_ncheck_nbuff,
              NPV_dredge_ncheck_buff=NPV_dredge_ncheck_buff-NPV_ndredge_ncheck_nbuff,
              NPV_dredge_ncheck_nbuff=NPV_dredge_ncheck_nbuff-NPV_ndredge_ncheck_nbuff,
              NPV_ndredge_check_buff=NPV_ndredge_check_buff-NPV_ndredge_ncheck_nbuff,
              NPV_ndredge_check_nbuff=NPV_ndredge_check_nbuff-NPV_ndredge_ncheck_nbuff,
              NPV_ndredge_ncheck_buff=NPV_ndredge_ncheck_buff-NPV_ndredge_ncheck_nbuff,
              cashflow_NPV_dredge_check_buff=result_dredge_check_buff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_dredge_check_nbuff=result_dredge_check_nbuff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_dredge_ncheck_buff=result_dredge_ncheck_buff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_dredge_ncheck_nbuff=result_dredge_ncheck_nbuff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_ndredge_check_buff=result_ndredge_check_buff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_ndredge_check_nbuff=result_ndredge_check_nbuff-result_ndredge_ncheck_nbuff,
              cashflow_NPV_ndredge_ncheck_buff=result_ndredge_ncheck_buff-result_ndredge_ncheck_nbuff))
}      
###############################################################################################################

### Running of the model ###     

decisionSupport("Sediment-2.csv",
                outputPath='results',
                welfareFunction=sediment_calc,
                numberOfModelRuns=10000,
                functionSyntax="plainNames")

mc<-read.csv("results/mcSimulationResults.csv")
legend_table<-read.csv("Sediment_input_table.csv")
mc_EVPI<-mc[,-grep("cashflow",colnames(mc))]
dir.create("Figures")
empirical_EVPI(mc_EVPI,"NPV_dredge_check_buff",write_table=TRUE,fileformat="png",outfolder="Figures",
               p_spearman=0.05, legend_table=read.csv("Sediment_legend.csv"),#legend_table,
               output_legend_table=read.csv("Sediment_legend.csv"))#legend_table)

#produce compound figures
#variable_name="implementer_NPV"

for (variable_name in c("NPV_dredge_check_buff","NPV_dredge_check_nbuff",
                        "NPV_dredge_ncheck_buff","NPV_dredge_ncheck_nbuff",
                        "NPV_ndredge_check_buff","NPV_ndredge_check_nbuff",
                        "NPV_ndredge_ncheck_buff"))
  compound_figure(variable_name=variable_name,
                  MC_table=mc,
                  PLS_table=read.csv(paste("results/",variable_name,"_pls_results.csv",sep="")),
                  EVPI_table=read.csv(paste("Figures/","EVPI_table_",variable_name,".csv",sep="")),
                  cash_flow_vars=paste("cashflow_",variable_name,sep=""),
                  nbreaks=100,scaler="auto",percentile_remove=c(.01,.99),
                  npls=15,plsthreshold=0.8,colorscheme="ICRAF_colors",MCcolor="mango",fonttype='sans',
                  borderlines=FALSE,lwd=2,
                  fileformat="png",filename=paste("Figures/","Combined_",variable_name,sep=""),
                  legend_table=read.csv("Sediment_legend.csv"))




make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("Sediment_input_table.csv"))



