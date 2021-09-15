1.5.1.0 2021-09-15
==================
- add checkDirW, checkExtantsDups, checkFileW, checkFileWs, checkMkdirs,
  checkOutputFiles, checkRunNICmds, checkRunNICmds', runNICmds,
  throwUsageErrors


1.5.0.1 2021-09-07
==================
- fix lvlToDoMock

1.5.0.0 2021-08-29
==================
- change UI of `stdMain` to pass a `DoMock` instance to the `io`.

1.4.0.0 2021-08-28
==================
- re-work UI to allow user selection of the incoming args (encouring explicit
  use of MonadIO.Base.getArgs rather than implicit)

1.3.5.0 2021-08-12
==================
- add Eq instance of UsageFPathIOError
- add UsageFPathError

1.3.4.3 2021-08-01
==================
- use optparse-plus 0.1.0.0

1.3.4.2 2021-08-01
===================
- use Options.Applicative.Extra.helper

1.3.4.1 2021-08-01
===================
- use parseOpts' from optparse-plus 0.0.3.0

1.3.4.0 2021-07-25
==================
- add UsageFPProcIOError

1.3.3.0 2021-06-22
==================
- add UsageFPathIOError

1.3.2.0 2021-06-19
==================
- add stdMainNoDR{,'}, stdMainT', export stdMainT

1.3.1.1 2021-06-04
==================
- use mockio-plus 0.2.0.0 / monadio-log 1.4.7.0 and friends

1.3.1.0 2021-05-11
==================
- add StdMain.OptionsTools

1.3.0.0 2021-05-08
==================
- add -!, -#; require that exceptions have callstacks

1.2.0.1 2021-02-25
==================
- use monadio-plus 1.3.1.0 (and other less significant module upgrades)

1.2.0.0 2021-02-04
==================
- simplify typesig for stdMainSimple

1.1.1.0 2021-02-04
==================
- export LogTIO type from StdMain

1.1.0.0 2020-12-25
==================
- stdMain' -> stdMainSimple; add stdMain' & stdMain''

1.0.1.0 2020-12-24
==================
- remove unnecessary Show constraint on stdMain*

1.0.0.0 2020-12-23
==================
- version 1
