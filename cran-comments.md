This submission is primarily to fix a test that was failing in the
most recent Devel versions of R due to some missing dependencies.
I think those errors were caused by lsmeans declaring some
dependencies as "suggests" that were then loaded on-demand 
by some lsmeans functions my tests called. So I have added these
packages to my dependencies as "suggests". 

## Test environments
* local Windows install, R 3.3.1 and 3.4.0
* Ubuntu 12.04 (on travis-ci), R 3.3.1

## R CMD check results
1 NOTE:

> NOTE
> Maintainer: 'Matthew Kay <mjskay@umich.edu>'
> 
> New maintainer:
>   Matthew Kay <mjskay@umich.edu>
> Old maintainer(s):
>   Matthew Kay <mjskay@uw.edu>

I have moved from the University of Washington to the University
of Michigan. I will send a separate email to CRAN@R-project.org
to confirm.

## Downstream dependencies
There are no downstream dependencies for this package.
