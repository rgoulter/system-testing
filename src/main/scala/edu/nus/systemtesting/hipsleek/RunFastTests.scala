package edu.nus.systemtesting.hipsleek

/**
 * For parsing legacy `run-fast-tests.pl` (and similar) files.
 * It may be necessary to parse such files.
 */
object RunFastTests {
  // General structure of the info we want in run-fast-test:
  //
  // Hip:
  //   %hip_files=(
  //               "name"=>[["filename", #expected, "--args", "proc1", "exp1",...],...]
  //               ...
  //              );
  //
  // Sleek:
  //   %sleek_files=(
  //                 "name"=>[["filename", "--args", ([$var,"Exp."]), "Exp1.Exp2."],...]
  //                 ...
  //                 );
  //
  // * The %hip_files= will start at the beginning of the line
  //   * No other `);` until the end of hip_files.
  // * => *can* be separated by a space.
  // * Mix of tabs, spaces (not just leading tabs) for whitespace.
  // * Hip expected is entirely SUCCESS, FAIL
  // * Sleek is mostly Valid, Fail,
  //   * except for `musterr` folder, which has must, may.
  //     (and in older versions, even 'bot')
  //   * cf. w/ run-fast-tests functionality, but EXC is possible, too?
  
  // Sleek TestCase isn't so clear what the () is for.
  // e.g. line 1958 @ 79da
  // ["lemmas/ll_tail.slk", " --elp ", ([$lem,"Valid.Valid"]), "Valid.Valid"],

  // Regarding other perl files in the repo,
  // * run-medium-tests seems to be of similar format, but much shorter,
  // * run-long-tests is something quite different.
}