# Warn when there is a big PR
warn("Big PR") if git.lines_of_code > 500

# Don't let testing shortcuts get into master by accident
fail("fit left in tests") if `git grep fit "*-test.js"`.length > 1
fail("fdescribe left in tests") if `git grep fdescribe "*-test.js"`.length > 1
