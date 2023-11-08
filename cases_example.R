head(esoph)
count(esoph)

all_cases = sum(esoph$ncases)
all_cases

all_controls = sum(esoph$ncontrols)
all_controls

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()
tob_cases/all_cases

tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
tob_controls/all_controls

high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc

high_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_tob <- high_tob_cases/all_cases
p_case_high_tob

high_alc_tob_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc_tob <- high_alc_tob_cases/all_cases
p_case_high_alc_tob

p_case_either_highest <- p_case_high_alc + p_case_high_tob - p_case_high_alc_tob
p_case_either_highest

high_alc_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
p_control_high_alc <- high_alc_controls/all_controls
p_control_high_alc

p_case_high_alc/p_control_high_alc

high_tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
p_control_high_tob <- high_tob_controls/all_controls
p_control_high_tob

high_alc_tob_controls <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
p_control_high_alc_tob <- high_alc_tob_controls/all_controls
p_control_high_alc_tob

p_control_either_highest <- p_control_high_alc + p_control_high_tob - p_control_high_alc_tob
p_control_either_highest

p_case_either_highest/p_control_either_highest

