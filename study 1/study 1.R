#####################################
# face vs behavior trait inference
# study 1
# trustworthy vs untrustworthy
#####################################

pckgs <- c(
  'lme4',
  'lmerTest',
  'interactions',
  'parameters',
  'effectsize',
  'tidyverse',
  'rstatix'
)

source('misc_functions.R')

package_loader(pckgs)

# read in all data files
raw_data <- list.files(path = "study 1/data",
                       pattern = "*.csv",
                       full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = "c")))

# prep the dataframe
clean_data <- raw_data %>%
  select('stimID',
         'face',
         'mTrustworthy',
         'trust',
         'behavior',
         'trait',
         'trialType',
         'congruence',
         'keyResp.keys',
         'condition',
         'participant') %>%
  na.omit() %>%
  mutate(participant = rep(1:160, each = 36)) %>%
  mutate_at(
    .vars = c('stimID',
              'face',
              'trust',
              'trait',
              'trialType',
              'congruence',
              'condition',
              'participant',
              'behavior'),
    .funs = as.factor
  ) %>%
  mutate_at(.vars = 'mTrustworthy', .funs = as.numeric) %>%
  rename('recognition' = 'keyResp.keys') %>%
  mutate(recognition = if_else(recognition == 'left', 1, 0))


# now sort out the implicit and mismatch responses (since explicit is a filler)
test_data <- clean_data %>%
  filter(trialType != "explicit") %>%
  mutate(behaviorCat = case_when(
    trust == 'Trustworthy' & congruence == 'congruent' ~ 'positive',
    trust == 'Untrustworthy' & congruence == 'incongruent' ~ 'positive',
    trust == 'Trustworthy' & congruence == 'incongruent' ~ 'negative',
    trust == 'Untrustworthy' & congruence == 'congruent' ~ 'negative',
    TRUE ~ 'nada'
  )) %>%
  mutate(
    trialType = if_else(trialType == 'mismatch', 'face_based', 'behavior_based'),
    trial_c = if_else(trialType == 'face_based', -1, 1),
    trust_c = if_else(trust == 'Untrustworthy', -1, 1),
    congruence_c = if_else(congruence == 'incongruent', -1, 1),
    behaviorCat_c = if_else(behaviorCat == 'Negative', -1, 1)
  )

# test for an overall effect for STIs (conforming to classic standards)
STImod <- glmer(recognition ~ trial_c 
                + (1|participant) 
                + (1|stimID:behavior), family = binomial, data = test_data)
model_summary(STImod)

test_data %>%
  group_by(trialType) %>%
  get_summary_stats(recognition, type = 'mean_sd')


# test the full effects
mod1 <- glmer(recognition ~ trial_c * trust_c * congruence_c 
              + (trust_c|participant) 
              + (1|stimID:behavior),
              control = glmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e6)
              ),
              family = binomial,
              data = test_data)
model_summary(mod1)

(int1 <- interact_plot(mod1, 
                       pred = trial_c, 
                       modx = congruence_c, 
                       #  mod2 = trustC,
                       colors = "green",
                       modx.labels = c("Congruent", "Incongruent"),
                       #  mod2.labels = c("Untrustworthy", "Trustworthy"),
                       pred.labels = c("Mismatch", "Match"),
                       x.label = "Trial Type",
                       y.label = "Trait Recognition",
                       legend.main = "Congruence") + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, size = 8)))


# split by congruence (main hypothesis test of the two-way interaction)
congruent <- test_data %>%
  filter(congruence == 'congruent')
incongruent <- test_data %>%
  filter(congruence == 'incongruent')

# model congruence effect first
mod2.1 <- glmer(recognition ~ trial_c 
                + (1|participant) 
                + (1|stimID:behavior),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e7)
                ),
                family = binomial,
                data = congruent)
model_summary(mod2.1)

# now incongruence
mod2.2 <- glmer(recognition ~ trial_c 
                + (1|participant) 
                + (1|stimID:behavior),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e7)
                ),
                family = binomial,
                data = incongruent)
model_summary(mod2.2)


# additional analyses for the three-way interaction

# split by trustworthiness
trust <- test_data %>%
  filter(trust == 'Trustworthy')
untrust <- test_data %>%
  filter(trust == 'Untrustworthy')

# trustworthiness first 
mod3.1 <- glmer(recognition ~ trial_c * congruence_c
                + (1|participant) 
                + (1|stimID:behavior),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e6)
                ),
                family = binomial,
                data = trust)
model_summary(mod3.1)


# simple effects for the two-way interaction for the trustworthy analysis
con_trust <- trust %>%
  filter(congruence == 'congruent')
incon_trust <- trust %>%
  filter(congruence == 'incongruent')

# congruent 
mod4.1 <- glmer(recognition ~ trial_c 
                + (1|participant) 
                + (1|stimID:behavior), 
                control = glmerControl(
                  optimizer = 'bobyqa',
                  optCtrl = list(maxfun = 2e6)
                ),
                family = binomial,
                data = con_trust)
model_summary(mod4.1)


# incongruent
mod4.2 <- glmer(recognition ~ trial_c 
                + (1|participant) 
                + (1|stimID:behavior), 
                control = glmerControl(
                  optimizer = 'bobyqa',
                  optCtrl = list(maxfun = 2e6)
                ),
                family = binomial,
                data = incon_trust)

model_summary(mod4.2)


# untrustworthy tests 
mod3.2 <- glmer(recognition ~ trial_c * congruence_c 
                + (1|participant) 
                + (1|stimID:behavior), 
                control = glmerControl(
                  optimizer = 'bobyqa',
                  optCtrl = list(maxfun = 2e6)
                ),
                family = binomial,
                data = untrust)
model_summary(mod3.2)


# table and graph
participant_table <- test_data %>%
  group_by(participant, trust, congruence, trialType) %>%
  get_summary_stats(recognition, type = 'mean')

summary_table <- test_data %>%
  group_by(trust, congruence, trialType) %>%
  get_summary_stats(recognition, type = 'full')

# facet labels for trustworthiness
trust_labs <- c('Trustworthy Targets', 'Untrustworthy Targets')
names(trust_labs) <- c('Trustworthy', 'Untrustworthy')


ggplot(participant_table, aes(trialType, mean, fill = congruence)) +
  geom_point(position = position_jitterdodge(.1, .05, .9),
             alpha = .5,
             color = 'black') +
  geom_violin(color = 'black',
              alpha = .9,
              position = position_dodge(.9)) +
  geom_point(data = summary_table,
             aes(trialType, mean),
             color = 'black',
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = summary_table,
                aes(trialType,
                    mean,
                    ymin = mean - ci,
                    ymax = mean + ci),
                width = .2,
                color = 'black',
                position = position_dodge(.9)) +
  theme_bw() +
  facet_wrap(~ trust,
             labeller = labeller(trust = trust_labs)) +
  scale_fill_manual(labels = c('Face-Behavior Congruence',
                               'Face-Behavior Incongruence'),
                    values = c('#006bb6', '#f58426')) +
  scale_x_discrete(labels = c('Behiavior-Based\nTrials',
                              'Face-Based\nTrials')) + 
  labs(x = '',
       y = 'Inference Rate',
       fill = '') +
  theme(legend.position = 'bottom')

# ggsave('study 1 plot.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'study 1')
