# ARTool 0.10.6

Minor changes:

* Minor changes to ensure tests pass due to changes in output from `lme4::lmer`


# ARTool 0.10.5

Minor changes:

* Replace `lsmeans` with `emmeans` in code and docs due to lsmeans being deprecated
* Use `psych::d.ci` for Cohen's _d_ CIs in effect size vignette


# ARTool 0.10.4

Minor changes:

* Reference phia vignette using function call instead of non-canonical URL
* Cautionary note about standardized effect sizes in effect size vignette
* Added testInteractions example as alternative in contrasts vignette

Bugfixes:

* Dependency fixes for failed test at testthat/test.artlm.R:35 (our use of lsmeans in that
test requires some additional packages only declared as "suggests" in lsmeans, so now
we "suggest" them as well).


# ARTool 0.10.1

New features:

* New vignette describing effect size estimates
* Using lsmeans interactions argument instead of phia for interaction contrasts vignette

Bugfixes:

* Require R >= 3.2 and lsmeans >= 2.22 to fix some bugs in earlier versions


# ARTool 0.10.0

New features:

* Support for Error() terms in model formulas (resulting models are run using aov())
* Checks for numeric variables passed into formulas that may cause incorrect results (if the user intended data to be treated as categorical)
* More detailed ANOVA tables
* New vignette describing contrast tests, particularly for interactions (vignette("art-contrasts"))

Bugfixes:

* Formulas now correctly support arbitrary expressions as terms (rather than just column names).


# ARTool 0.9.5

Testing fix for changes in upcoming version of lsmeans: round lsmeans p value tests to 5 decimal places to accommodate changes to Tukey adjustment


# ARTool 0.9.4

Minor changes to testing based on updated version of testthat (0.10.0):

* More closely follow recommended testthat usage
* Skip tests requiring lsmeans if it is not installed
