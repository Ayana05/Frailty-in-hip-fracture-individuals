#Data Import
data.parent.directory <- "C:/Users/ayana/Downloads/NHATS_Roundwise_SP_File"

#Load required packages
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr","survey","haven","labelled","tidyverse",
  "gtsummary", "gt", "nnet", "broom", "emmeans", "openxlsx"
)
invisible(lapply(required_pkgs, library, character.only = TRUE))

#------------------------------------------------------------------
# Select whether cutpoints should be pulled from the Bandeen-Roche 2015 paper
# or Fried (2001)
cutpoint.type = "NHATS"
#cutpoint.type =S"

### Cutpoint Selections
if(cutpoint.type == "NHATS"){
  # Low BMI Cutpoint
  bmi.lower.bound = 18.5
  
  # Grip
  # sex-specific bmi cutoffs for determining strength cutoff
  f.bmi = c(18.5, 25, 30)
  m.bmi = c(18.5, 25, 30)
  # sex/bmi-specific grip cutoffs
  f.grip = c(12.66, 12.66, 12.92, 12.23)
  m.grip = c(22.75, 22.75, 25.59, 25.96)
  
  # Note: it was recommended by the author that the grip strength cutoff for
  # each of the lowest BMI categories (<18.5kg/m^2) be substituted with the
  # cutoff for the next-highest BMI category (18.5 to <25 kg/m^2). Estimates
  # published in their work (9.21kg for females, 5.52kg for males) may have
  # been very low due to small analytic sample sizes.
  
  # Slow Gait Parameters
  
  # sex-specific height cutoff for using higher/lower gait speed cutoff
  f.height = 1.59
  m.height = 1.73
  
  # sex/height-specific gait speed cutoffs
  f.gait = c(0.35, 0.45)
  m.gait = c(0.5, 0.58)
  
}else if(cutpoint.type == "CHS"){
  # Low BMI Cutpoint
  bmi.lower.bound = 18.5
  
  # Weak Grip Parameters
  f.bmi = c(23, 26, 29)
  m.bmi = c(24, 26, 28)
  f.grip = c(17, 17.3, 18, 21)
  m.grip = c(29, 30, 30, 32)
  
  # Slow Gait Parameters
  
  # sex-specific height cutoff for using higher/lower gait speed cutoff
  f.height = 1.59
  m.height = 1.73
  
  # sex-specific gait speed cutoff for taller and shorter group
  f.gait = c(0.65, 0.76)
  m.gait = c(0.65, 0.76)
}
#------------------------------------------------------------------
df_files <- tibble(
  file = dir(
    data.parent.directory,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE,
    # FIXED: Pattern now matches "NHATS_Round_X_SP_File..." instead of "roundX"
    pattern = "NHATS_Round_[0-9]+_SP_File(_V[0-9]+)?\\.dta"
  )
) %>% 
  # FIXED: Extract the round number directly from the filename right here
  mutate(round = as.numeric(str_extract(basename(file), "[0-9]+"))) %>% 
  # Now this filter statement works safely without throwing an error
  filter(!is.na(round))

##Constructing Fried Frailty Phenotype measures
#===============================================
#Baseline characteristics table by frailty group (raw variables)
df_raw <- df_files %>% 
  mutate(
    data = map(
      file,
      ~haven::read_dta(
        .,
        col_select = c(
          starts_with("spid"),
          
          # Weight & Design variables
          ends_with("anfinwgt0"), ends_with("varunit"), ends_with("varstrat"),
          
          # Demographics & Health
          ends_with("intvrage"), ends_with("dgender"), ends_with("racehisp"),
          ends_with("spouseduc"), ends_with("rehab"),ends_with("disescn1"),ends_with("disescn2"),ends_with("disescn3"), ends_with("disescn4"),
          ends_with("disescn5"), ends_with("disescn6"), ends_with("disescn7"),ends_with("disescn8"), ends_with("disescn9"), ends_with("disescn10"),
          ends_with("dresid"), ends_with("ability"), ends_with("abilworse"),
          ends_with("nowworse"), ends_with("primpayer"), ends_with("fllsinmth"),
          ends_with("hosptstay"), ends_with("placelast"), ends_with("rehabcond"),
          ends_with("rehabmo"),ends_with("rehabsurg"),ends_with("hipsrgyr"),ends_with("brokebon1"),
          
          # Anthropometry (Height & Weight)
          ends_with("howtallft"), ends_with("howtallin"), ends_with("currweigh"),
          ends_with("lst10pnds"), ends_with("trytolose"),
          
          # Low Energy & Activity items
          ends_with("loenlmtat"), ends_with("lowenergy"),
          ends_with("vigoractv"), ends_with("evrgowalk"),
          
          # Mobility & Gait Exclusions
          ends_with("insdwlchr"), ends_with("insdsctr"), ends_with("wlkdsself"),
          
          # Gait Speed Items
          ends_with("wlkc1secs"), ends_with("wlk1hndr"), ends_with("wlkc2secs"),
          ends_with("wlk2hndr"), ends_with("rsn31wkc"), ends_with("rsn21wkc"),
          ends_with("rsn11wkc"), ends_with("dwlkadm"), ends_with("wlkc1rslt"),
          
          # Grip Strength Items
          ends_with("surghdwrt"), ends_with("surgyside"), ends_with("surgarmsh"),
          ends_with("sidsurgar"), ends_with("flruppain"), ends_with("sideflrup"),
          ends_with("grp1rdng"), ends_with("grp2rdng"), ends_with("dgripadm"),
          ends_with("grp1reslt"), ends_with("grp1noat1"), ends_with("grp1noat2"),
          ends_with("grp1noat3"),
          
          # Rehab Setting, Goals & Concerns
          ends_with("place1"), ends_with("place2"), ends_with("place3"),
          ends_with("place4"), ends_with("place5"), ends_with("metgoals"),
          ends_with("metinsur"), ends_with("abilnow"), ends_with("impchew"),
          ends_with("impspeak"), ends_with("impspain"), ends_with("impbreath"),
          ends_with("impweak"), ends_with("impmove"), ends_with("impenergy"),
          ends_with("impbal"), ends_with("impfall"), ends_with("impmemory"),
          ends_with("impnone"), ends_with("impbed"), ends_with("impwalk"),
          ends_with("impleave"), ends_with("impdistnc"), ends_with("impclimb"),
          ends_with("impdrive"), ends_with("imptransp"), ends_with("impnotths"),
          ends_with("impsc"), ends_with("imphh"), ends_with("impcomp"),
          ends_with("impwork"), ends_with("impcare"), ends_with("impactiv"),
          ends_with("impactnon"), ends_with("mealwheel"),
          
          # Frailty & Function
          ends_with("dnhatssppb"), ends_with("dorigbasc"), ends_with("dorigwksc"),
          ends_with("dorigchsc"), ends_with("marstat"),
          
          # Depression/Anxiety
          ends_with("depresan1"), ends_with("depresan2"), ends_with("depresan3"),
          ends_with("depresan4"),
          
          #SPPB score
          ends_with("dorigsppb"),
          ends_with("sppb"),
          # Self-rated health
          ends_with("health"),
          # Anxiety item
          ends_with("lm5sadhlp"),
          
          # Education (round-specific — only asked in Rounds 1, 5, 12, 13)
          ends_with("el1higstschl"), ends_with("el5higstschl"),
          ends_with("el12dhigstschl"), ends_with("el13dhigstschl"),
          #survey weight
          ends_with("anfinwgt"),ends_with("varstrat"),ends_with("varunit")
        )
      ) %>% 
        rename_with(
          # FIXED: Removes prefixes (like 'r3') AND uses make.unique() to prevent column-name collisions
          .fn = function(v) make.unique(gsub("(r|fq|ha|hw|ss|va|pa|wa|gr|rh|mo|pe|hc|w|hh|rl)[0-9]+", "", v))
        )
    )
  ) %>% unnest(data)%>% group_by(
    spid
  ) %>% mutate(
    # sex was not reported the same way during round 2 so we need to
    # aggregate across rounds
    female = ifelse(
      sum(dgender==1,na.rm=T)>sum(dgender==2,na.rm=T),
      0,
      1
    ),
    .after = "spid"
  ) %>% ungroup %>% mutate(
    # anthropometrics
    height.m = ifelse(
      (howtallft>0)&(howtallin>=0),
      (howtallft*12+howtallin)/39.37,
      NA
    ),
    weight.kg = ifelse(
      currweigh > 0,
      currweigh/2.205,
      NA
    ),
    bmi = weight.kg/height.m^2,
    
    # Weight Loss/Low BMI
    low.bmi = (bmi <= bmi.lower.bound),
    weight.lower = ifelse(
      (lst10pnds < 1),
      NA,
      ifelse(
        (trytolose == 1) | (lst10pnds == 2),
        0,
        # were trying, or did not lose
        ifelse(
          trytolose == 2,
          # was not trying
          1,
          NA
        )
      )
    ),
    
    # speed
    gait1 = ifelse(
      (wlkc1secs > 0)&(wlk1hndr > -1),
      3/(wlkc1secs + wlk1hndr/100),
      NA
      
    ),
    gait2 = ifelse(
      (wlkc2secs > 0)&(wlk2hndr > -1),
      3/(wlkc2secs+ wlk2hndr/100),
      NA
      
    ),
    gait.speed.max = pmax(gait1,gait2,na.rm = T),
    gait.unable.unsafe = ifelse(
      # we will only use this indicator if there is no recorded gait speed
      is.na(gait.speed.max)&(
        # participant always uses wheelchair OR scooter OR
        (insdsctr==1)|(insdwlchr==1)|
          # indicates cannot walk short distance by self OR
          (wlkdsself==2)|
          # someone indicates safety concern OR
          (rsn31wkc == 1)|(rsn21wkc == 1)|(rsn11wkc == 1)|
          # they do not complete the first trial
          (wlkc1rslt == 2)
      ), 1, NA
    ),
    
    # grip strength
    grip.strength1 = ifelse(
      grp1rdng > 0, grp1rdng, NA
    ),
    grip.strength2 = ifelse(
      grp2rdng > 0, grp2rdng, NA
    ),
    grip.strength.max = pmax(grip.strength1,grip.strength2, na.rm = T),
    grip.unable.unsafe = ifelse(
      is.na(grip.strength.max)&(
        (grp1reslt == 2)|
          # first trial attempted but not completed
          (dgripadm == 4)|
          # Not Eligible due to exclusion criteria
          (grp1noat1 == 1)|(grp1noat2 == 1)|(grp1noat3 == 1)
        # at least one person felt unsafe
      ), 1, NA
    ),
    
    
    # low energy
    low.energy = ifelse(
      (loenlmtat == 1)&(lowenergy==1),
      1,
      ifelse(
        (lowenergy == 2) | (loenlmtat == 2),
        0, NA
      )
    ),
    # low activity
    low.activity = ifelse(
      (vigoractv == 2)&(evrgowalk == 2),
      1,
      ifelse(
        (vigoractv < 0)&(evrgowalk < 0),
        NA, 0
      )
    ),
    
    # weight loss (combined)
    weight.loss = 1*((low.bmi == 1)|(weight.lower == 1)),
    
    # slow gait
    slow.gait = NA,
    # use strict cutpoints when height and gait are measured
    slow.gait = ifelse(
      (!is.na(height.m))&(!is.na(gait.speed.max))&(!is.na(female)),
      ((female == 1)&(height.m<=f.height)&(gait.speed.max <= f.gait[1]))|
        ((female == 1)&(height.m>f.height)&(gait.speed.max <=f.gait[2]))|
        ((female == 0)&(height.m<=m.height)&(gait.speed.max <=m.gait[1]))|
        ((female == 0)&(height.m>m.height)&(gait.speed.max <=m.gait[2])),
      slow.gait
    ),
    # use max/min cutpoints when height is not measured
    slow.gait = ifelse(
      (is.na(slow.gait))&
        (is.na(height.m))&
        (!is.na(gait.speed.max))&
        (!is.na(female))&(
          ((female == 1)&(gait.speed.max <= min(f.gait)))|
            ((female == 0)&(gait.speed.max <= min(m.gait)))
        ), 1, slow.gait
    ),
    slow.gait = ifelse(
      (is.na(slow.gait))&
        (is.na(height.m))&
        (!is.na(gait.speed.max))&
        (!is.na(female))&(
          ((female == 1)&(gait.speed.max >= max(f.gait)))|
            ((female == 0)&(gait.speed.max >= max(m.gait)))
        ), 0, slow.gait
    ),
    slow.gait = ifelse(
      (is.na(slow.gait))&
        (!is.na(gait.unable.unsafe))&
        (gait.unable.unsafe == 1),
      1,
      slow.gait
    ),
    
    # weak. grip
    # use strict cutpoints when BMI and grip are measured
    weak.grip = ifelse(
      (!is.na(female))&(!is.na(bmi))&(!is.na(grip.strength.max)),
      ((female == 1)&(bmi<=f.bmi[1])&(grip.strength.max <= f.grip[1]))|
        ((female == 1)&(bmi>f.bmi[1])&(bmi<=f.bmi[2])&(grip.strength.max <=f.grip[2]))|
        ((female == 1)&(bmi>f.bmi[2])&(bmi<=f.bmi[3])&(grip.strength.max <=f.grip[3]))|
        ((female == 1)&(bmi>f.bmi[3])&(grip.strength.max <=f.grip[4]))|
        ((female == 0)&(bmi<=m.bmi[1])&(grip.strength.max <=m.grip[1]))|
        ((female == 0)&(bmi>m.bmi[1])&(bmi<=m.bmi[2])&(grip.strength.max <=m.grip[2]))|
        ((female == 0)&(bmi>m.bmi[2])&(bmi<=m.bmi[3])&(grip.strength.max <=m.grip[3]))|
        ((female == 0)&(bmi>m.bmi[3])&(grip.strength.max <= m.grip[4])),
      NA
    ),
    # use max/min cutpoints when BMI not measured and grip is measured
    weak.grip = ifelse(
      (is.na(weak.grip)) &
        (!is.na(female)) &
        (is.na(bmi)) &
        (!is.na(grip.strength.max))&(
          ((female == 1)&(grip.strength.max <=min(f.grip)))|
            ((female == 0)&(grip.strength.max <=min(m.grip)))
        ), 1, weak.grip
    ),
    weak.grip = ifelse(
      is.na(weak.grip) &
        (!is.na(female)) &
        (is.na(bmi)) &
        (!is.na(grip.strength.max))&(
          ((female == 1)&(grip.strength.max >max(f.grip)))|
            ((female == 0)&(grip.strength.max >max(m.grip)))
        ), 0, weak.grip
    ),
    
    # use unable/unsafe criteria otherwise
    weak.grip = ifelse(
      is.na(weak.grip) &
        (!is.na(grip.unable.unsafe))&
        (grip.unable.unsafe == 1),
      1,
      weak.grip
    ),
    
    # Count Frailty Components
    # the minimum possible score is derived by assuming NAs are all 0
    min.score =
      ifelse(is.na(weak.grip),0,weak.grip) +
      ifelse(is.na(slow.gait),0,slow.gait) +
      ifelse(is.na(low.energy),0,low.energy) +
      ifelse(is.na(low.activity),0,low.activity) +
      ifelse(is.na(weight.loss),0,weight.loss),
    
    # the maximum possible score is derived by assuming NAs are all 1
    max.score =
      ifelse(is.na(weak.grip),1,weak.grip) +
      ifelse(is.na(slow.gait),1,slow.gait) +
      ifelse(is.na(low.energy),1,low.energy) +
      ifelse(is.na(low.activity),1,low.activity) +
      ifelse(is.na(weight.loss),1,weight.loss),
    
    # a frailty phenotype is derived when the range of possible scores falls
    # definitively within the criteria for a given category
    
    frailty.at.round =
      ifelse(
        min.score >=3, "Frail",
        ifelse(
          (min.score > 0)&(max.score<3), "Prefrail",
          ifelse(
            max.score == 0, "Robust",
            ifelse(
              (min.score>0),"Not Robust",
              ifelse(
                (max.score<3), "Not Frail",
                ifelse(
                  (min.score==0)&(max.score==5), "Not Assessed",
                  "Indeterminate"
                )
              )
            )
          )
        )
      ),
    
    # Weak grip reason
    
    weak.grip.reason = NA,
    weak.grip.reason = ifelse(
      is.na(weak.grip), "Missing Weakness Component", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      (weak.grip==0)&!is.na(weak.grip), "Not Weak", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason)&(!is.na(grip.strength.max)),
      "Weak, Grip Strength Below Threshold", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason)&(dgripadm == 4)&((surghdwrt ==1)|(surgarmsh ==1)),
      "Weak, Surgery or Injury Exclusion", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason)&(dgripadm == 4)&(flruppain ==1),
      "Weak, Pain Exclusion", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason)&(dgripadm == 4),
      "Weak, Other Exclusion", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason) & ((grp1noat1 == 1)|(grp1noat2 == 1)|(grp1noat3 == 1)),
      "Weak, Safety Objection", weak.grip.reason
    ),
    weak.grip.reason = ifelse(
      is.na(weak.grip.reason) & (grp1reslt == 2),
      "Weak, Did Not Complete", weak.grip.reason
    ),
    
    # slow gait reason
    slow.gait.reason = NA,
    slow.gait.reason = ifelse(
      is.na(slow.gait), "Missing Slowness Component", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      (slow.gait==0)&!is.na(slow.gait), "Not Slow", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      is.na(slow.gait.reason)&(!is.na(gait.speed.max)),
      "Slow, Gait Speed Below Threshold", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      is.na(slow.gait.reason)&(insdsctr==1)|(insdwlchr==1),
      "Slow, Scooter or Wheelchair Use", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      is.na(slow.gait.reason) & (wlkdsself == 2),
      "Slow, Unable to Walk by Self", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      is.na(slow.gait.reason) & ((rsn31wkc == 1)|(rsn21wkc == 1)|(rsn11wkc == 1)),
      "Slow, Safety Objection", slow.gait.reason
    ),
    slow.gait.reason = ifelse(
      is.na(slow.gait.reason) & (wlkc1rslt == 2),
      "Slow, Did Not Complete", slow.gait.reason
    ),
  )

#education variable
df_raw <- df_raw %>%
  mutate(
    education_raw = coalesce(el1higstschl, el5higstschl, el12dhigstschl, el13dhigstschl)
  ) %>%
  group_by(spid) %>%
  fill(education_raw, .direction = "downup") %>%   # carry the one non-missing value across all that person's rows
  ungroup()

#Build a continuous frailty score 
df_raw <- df_raw %>%
  mutate(frailty_score = min.score) 

#commorbodites previous round input fixing
df_raw <- df_raw %>%
  arrange(spid, round) %>%
  group_by(spid) %>%
  mutate(
    across(
      c(disescn2, disescn3, disescn4, disescn5,disescn6,disescn7,disescn8,disescn9),
      ~ {
        x <- .x
        
        for (i in 1:length(x)) {
          if (x[i] == 7) {
            x[i] <- x[i - 1]
          }
        }
        
        x
      }
    )
  ) %>%
  ungroup()
df_raw

#marital status fixing
df_raw <- df_raw %>%
  arrange(spid, round) %>%
  group_by(spid) %>%
  mutate(
    mar = if (all(is.na(dmarstat))) NA else first(na.omit(dmarstat))
  ) %>%
  ungroup()
df_raw

#Extracting hip fracture patients (Hip fratcure round and the subsequent 3 rounds)
library(dplyr)


# 1. Identify participants with hip fracture
df1 <- df_raw %>%
  filter(brokebon1 == 1)


# 2. Find the first round in which each participant had a hip fracture
min_round <- df1 %>%
  group_by(spid) %>%
  summarise(
    min_round = min(round, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Add the first fracture round to the complete dataset
df2 <- df_raw %>%
  left_join(min_round, by = "spid")


# 4. Keep only rounds from the first fracture round onwards
df3 <- df2 %>%
  filter(
    !is.na(min_round),
    round >= min_round
  ) %>%
  arrange(spid, round)


# 5. Assign sequential row numbers within each participant
df3 <- df3 %>%
  group_by(spid) %>%
  mutate(
    row_num = row_number()
  ) %>%
  ungroup()


# 6. Find the total number of available rounds for each participant
max_round <- df3 %>%
  group_by(spid) %>%
  summarise(
    max_round = max(row_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(max_round >= 4)
max_round

# 7. Keep only participants with at least 4 rounds
#    and retain their first 4 rounds
df4 <- df3 %>%
  left_join(max_round, by = "spid") %>%
  filter(
    !is.na(max_round),
    row_num <= 4
  ) %>%
  arrange(spid, row_num)
