## Resubmission

This is a resubmit from earlier today. I removed a non-canonical CRAN URL 
referencing the phia package vignette from one of the ARTool vignettes.

Per Uwe Ligges I will also note my change in email address here and
cite that exchange:

> On Sun, Oct 23, 2016 at 4:57 PM, Uwe Ligges <ligges@statistik.tu-dortmund.de> wrote:
> Thanks for confirming, please cite this message in your next submission comment.
> 
> Best,
> Uwe Ligges
> 
> 
> > On 23.10.2016 20:00, Matthew Kay wrote:
> > In the most recent submission for the ARTool package I have changed my
> > email address from:
> > 
> > mjskay@uw.edu <mailto:mjskay@uw.edu>
> > 
> > to:
> > 
> > mjskay@umich.edu <mailto:mjskay@umich.edu>
> > 
> > since I have moved to the University of Michigan.
> > 
> > ---Matt
> > 
> > --
> > Matthew Kay
> > mjskay@cs.washington.edu <mailto:mjskay@cs.washington.edu>
> > http://www.mjskay.com/


## Original submission comments

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
