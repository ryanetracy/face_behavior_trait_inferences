#####################################
# face vs behavior trait inference
# study 4
# trustworthy vs untrustworthy
# black male faces
#####################################


# set packages
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


# load data
raw_data <- read_csv('study 4/data/study 4 data.csv') %>%
  filter(consent == 1 &
           Progress > 43) %>%
  select(-contains(
    c('Date',
      'Progress',
      'IPAddress',
      'Status',
      'Duration',
      'Finished',
      'Recipient',
      'External',
      'Location',
      'Distribution',
      'Language',
      'consent',
      'First Click',
      'Last Click',
      'Page Submit',
      'Click Count',
      'distractor',
      '_TEXT')
  )) %>%
  rename('subj' = 'ResponseId',
         'block' = 'FL_19_DO')


# demographics
# age
raw_data %>%
  get_summary_stats(age, type = 'mean_sd')

# gender
raw_data %>%
  count(gender) %>%
  mutate(
    prop = round(n/sum(n) * 100, 2),
    gender = case_when(
      gender == 1 ~ 'male',
      gender == 2 ~ 'female',
      gender == 3 ~ 'nonbinary',
      gender == 4 ~ 'other',
      TRUE ~ 'no response'
    )
  )

# race
raw_data %>%
  count(race) %>%
  mutate(
    prop = round(n/sum(n) * 100, 2),
    race = case_when(
      race == 1 ~ 'asian',
      race == 2 ~ 'latino/a',
      race == 3 ~ 'black',
      race == 4 ~ 'white',
      race == 5 ~ 'middle eastern',
      race == 6 ~ 'hawaiian/PI',
      race == 7 ~ 'bi/multi racial',
      race == 8 ~ 'other',
      TRUE ~ 'no response'
    )
  )


# split by block
b1 <- raw_data %>%
  filter(block == 'encoding1') %>%
  select(contains(c('Progress', 'subj', '_r1', 'block')))

b2 <- raw_data %>%
  filter(block == 'encoding2') %>%
  select(contains(c('Progress', 'subj', '_r2', 'block')))

b3 <- raw_data %>%
  filter(block == 'encoding3') %>%
  select(contains(c('Progress', 'subj', '_r3', 'block')))

b4 <- raw_data %>%
  filter(block == 'encoding4') %>%
  select(contains(c('Progress', 'subj', '_r4', 'block')))

b5 <- raw_data %>%
  filter(block == 'encoding5') %>%
  select(contains(c('Progress', 'subj', '_r5', 'block')))

b6 <- raw_data %>%
  filter(block == 'encoding6') %>%
  select(contains(c('Progress', 'subj', '_r6', 'block')))

CONGRUENCE_1 <- c(
  rep('congruent', 9),
  rep('incongruent', 18),
  rep('congruent', 9)
)

CONGRUENCE_2 <- c(
  rep('incongruent', 9),
  rep('congruent', 18),
  rep('incongruent', 9)
)

# create naming vectors
b1_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('explicit', 'implicit', 'mismatch'), each = 3),
  rep('_'),
  CONGRUENCE_1
)

b2_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('mismatch', 'explicit', 'implicit'), each = 3),
  rep('_'),
  CONGRUENCE_1
)

b3_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('implicit', 'mismatch', 'explicit'), each = 3),
  rep('_'),
  CONGRUENCE_1
)

b4_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('explicit', 'implicit', 'mismatch'), each = 3),
  rep('_'),
  CONGRUENCE_2
)

b5_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('mismatch', 'explicit', 'implicit'), each = 3),
  rep('_'),
  CONGRUENCE_2
)

b6_names <- paste0(
  'stim',
  rep(1:36),
  rep('_'),
  rep(c('trustworthy', 'untrustworthy'), each = 18),
  rep('_'),
  rep(c('implicit', 'mismatch', 'explicit'), each = 3),
  rep('_'),
  CONGRUENCE_2
)

# apply the _names to the respective blocks
names(b1)[2:37] <- b1_names
names(b2)[2:37] <- b2_names
names(b3)[2:37] <- b3_names
names(b4)[2:37] <- b4_names
names(b5)[2:37] <- b5_names
names(b6)[2:37] <- b6_names


# reshape to long
b1_long <- b1 %>%
  pivot_longer(
    cols = b1_names[1]:b1_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )

b2_long <- b2 %>%
  pivot_longer(
    cols = b2_names[1]:b2_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )

b3_long <- b3 %>%
  pivot_longer(
    cols = b3_names[1]:b3_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )


b4_long <- b4 %>%
  pivot_longer(
    cols = b4_names[1]:b4_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )

b5_long <- b5 %>%
  pivot_longer(
    cols = b5_names[1]:b5_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )

b6_long <- b6 %>%
  pivot_longer(
    cols = b6_names[1]:b6_names[36],
    names_to = c('stimID', 'trust', 'trialType', 'congruence'),
    values_to = 'recog',
    names_sep = '_'
  )


# merge
long_data <- rbind(
  b1_long,
  b2_long,
  b3_long,
  b4_long,
  b5_long,
  b6_long
)

# recode recognition to binary
long_data$recog <- ifelse(long_data$recog == 1, 1, 0)

# get mean recognition rates across all trial types
long_data %>%
  group_by(trialType) %>%
  get_summary_stats(recog, type = 'mean_sd')


# compare implicit vs mismatch
long_data %>%
  filter(trialType != 'explicit') %>%
  t_test(recog ~ trialType, paired = T)


# drop explicit trials
long_data <- long_data %>%
  filter(trialType != 'explicit')

# add sum contrast columns
long_data <- long_data %>%
  mutate(
    trust_c = if_else(trust == 'trustworthy', 1, -1),
    con_c = if_else(congruence == 'congruent', 1, -1),
    trialType = if_else(trialType == 'implicit', 'behavior_based', 'face_based'),
    trial_c = if_else(trialType == 'behavior_based', 1, -1)
  )

# build models
# establish an STI effect
mSTI <- glmer(recog ~ trial_c 
              + (1|subj) 
              + (1|stimID), 
              control = glmerControl(
                optimizer = 'bobyqa',
                optCtrl = list(maxfun = 2e7)
              ),
              family = binomial,
              data = long_data)
model_summary(mSTI)


# test full model
m1 <- glmer(recog ~ trial_c * con_c * trust_c
            + (1|subj)
            + (0 + trial_c|stimID),
            control = glmerControl(
              optimizer = 'bobyqa',
              optCtrl = list(maxfun = 2e7)
            ),
            family = binomial,
            data = long_data)
model_summary(m1)


# plot 3-way interaction
interact_plot(m1,
              pred = 'trial_c',
              modx = 'con_c',
              mod2 = 'trust_c',
              interval = T,
              int.type = 'confidence',
              colors = 'green',
              pred.labels = c('Face-Based', 'Behavior-Based'),
              modx.labels = c('Incongruent\nTrials', 'Congruent\nTrials'),
              mod2.labels = c('Untrustworthy Targets', 'Trustworthy Targets'),
              y.label = 'False Recognition Rate',
              x.label = 'Trial Type',
              legend.main = '') +
  theme_classic() +
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 0, vjust = .7))

# 3-way interaction, explore at each level of facial trustworthiness
# trustworthy
m2 <- glmer(recog ~ trial_c * con_c
            + (1|subj) 
            + (0 + trial_c|stimID),
            control = glmerControl(
              optimizer = 'bobyqa',
              optCtrl = list(maxfun = 2e7)
            ),
            family = binomial,
            data = filter(long_data, trust_c == 1))
model_summary(m2)

# trustworthy, congruent
m2.1 <- glmer(recog ~ trial_c 
              + (1|subj) 
              + (0 + trial_c|stimID), 
              control = glmerControl(
                optimizer = 'bobyqa', 
                optCtrl = list(maxfun = 2e7)
              ),
              family = binomial,
              data = filter(long_data, trust_c == 1 & con_c == 1))
model_summary(m2.1)

# trustworthy, incongruent
m2.2 <- glmer(recog ~ trial_c 
              + (1|subj) 
              + (0 + trial_c|stimID), 
              control = glmerControl(
                optimizer = 'bobyqa', 
                optCtrl = list(maxfun = 2e7)
              ),
              family = binomial,
              data = filter(long_data, trust_c == 1 & con_c == -1))
model_summary(m2.2)


# untrustworthy
m3 <- glmer(recog ~ trial_c * con_c
            + (1|subj) 
            + (0 + trial_c|stimID),
            control = glmerControl(
              optimizer = 'bobyqa',
              optCtrl = list(maxfun = 2e7)
            ),
            family = binomial,
            data = filter(long_data, trust_c == -1))
model_summary(m3)

# untrustworthy, congruent
m3.1 <- glmer(recog ~ trial_c 
              + (1|subj) 
              + (1|stimID), 
              control = glmerControl(
                optimizer = 'bobyqa', 
                optCtrl = list(maxfun = 2e7)
              ),
              family = binomial,
              data = filter(long_data, trust_c == -1 & con_c == 1))
model_summary(m3.1)

# untrustworthy, incongruent
m3.2 <- glmer(recog ~ trial_c 
              + (1|subj) 
              + (0 + trial_c|stimID), 
              control = glmerControl(
                optimizer = 'bobyqa', 
                optCtrl = list(maxfun = 2e7)
              ),
              family = binomial,
              data = filter(long_data, trust_c == -1 & con_c == -1))
model_summary(m3.2)


# graph the results
participant_table <- long_data %>%
  group_by(subj, trust, congruence, trialType) %>%
  get_summary_stats(recog, type = 'mean')

summary_table <- long_data %>%
  group_by(trust, congruence, trialType) %>%
  get_summary_stats(recog, type = 'full')

# facet labels for dominance
trust_labs <- c('Trustworthy Targets', 'Untrustworthy Targets')
names(trust_labs) <- c('trustworthy', 'untrustworthy')


ggplot(participant_table, aes(congruence, mean, fill = trialType)) +
  geom_point(position = position_jitterdodge(.1, .05, .9),
             alpha = .5,
             color = 'black') +
  geom_violin(color = 'black',
              alpha = .9,
              position = position_dodge(.9)) +
  geom_point(data = summary_table,
             aes(congruence, mean),
             color = 'black',
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = summary_table,
                aes(congruence,
                    mean,
                    ymin = mean - ci,
                    ymax = mean + ci),
                width = .2,
                color = 'black',
                position = position_dodge(.9)) +
  theme_bw() +
  facet_wrap(~ trust,
             labeller = labeller(trust = trust_labs)) +
  scale_fill_manual(labels = c('Behiavior-Based\nTrials',
                               'Face-Based\nTrials'),
                    values = c('#006bb6', '#f58426')) +
  scale_x_discrete(labels = c('Face-Behavior\nCongruence',
                              'Face-Behavior\nIncongruence')) + 
  labs(x = '',
       y = 'Inference Rate',
       fill = '') +
  theme(legend.position = 'bottom')

# ggsave('study 4 plot.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'study 4')

