library(MatchIt)
library(cobalt)

set.cobalt.options(binary = "std")

mod_match <- matchit(treated ~ percent_black_alone + weighted_income+ weighted_pop +
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "nearest", data = comparison_tab, ratio = 1) 
summary(mod_match)
love.plot(mod_match)

mod_match <- matchit(treated ~ percent_black_alone + weighted_income+ weighted_pop +
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "nearest", data = comparison_tab, ratio = 2)
summary(mod_match)
love.plot(mod_match, binary = 'std')
# comparison_tab = match.data(mod_match)


mod_match <- matchit(treated ~ percent_black_alone + weighted_income+weighted_pop +
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "nearest", data = comparison_tab, ratio = 3)
summary(mod_match)
love.plot(mod_match)

mod_match <- matchit(treated ~ percent_black_alone + weighted_income+weighted_pop +
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "full", data = comparison_tab)
summary(mod_match)
love.plot(mod_match)

mod_match <- matchit(treated ~ percent_black_alone + weighted_income+weighted_pop +
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "cem", data = comparison_tab)
summary(mod_match)
love.plot(mod_match)
