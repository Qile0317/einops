## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release that fixes the issues that lead to the rejection of the previous 0.1.0 and 0.2.0 release. The previous CRAN check was failing due to the suggested torch dependency which requires manual user setup, so the issue has now been fixed to skip all tests related to torch.
