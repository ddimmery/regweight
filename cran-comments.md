# Resubmission

This fixes issues identified by the check results on the latest R-devel:

* Resolves warning when running `donttest` check.
* Adds Suggests to resolve NOTEs for undeclared packages in Rd references.

# R CMD check results

## Local (Mac M1, R-release)

0 errors | 0 warnings | 0 notes

## rhub::check_for_cran()

### x86_64-pc-linux-gnu R-4.1.2

0 errors | 0 warnings | 0 notes

### x86_64-pc-linux-gnu R-devel

0 errors | 0 warnings | 0 notes

### Win R-devel

0 errors | 0 warnings | 1 note

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

Seeing others with the same issue [1] and no problems 
on `check_win_devel()`, I am ignoring this.

[1] https://github.com/anders-biostat/jrc/blob/master/cran-comments.md#test-environments

## devtools::check_win_devel()

0 errors | 0 warnings | 0 notes