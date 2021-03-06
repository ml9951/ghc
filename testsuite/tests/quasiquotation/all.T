test('T3953', req_interp, compile_fail, [''])
test('T4150',
     [
      expect_broken(4150),
      extra_clean(['T4150A.hi', 'T4150A.o', 'T4150.hi', 'T4150.o'])],
     run_command,
     ['$MAKE -s --no-print-directory T4150'])
test('T5204', req_interp, compile_fail, [''])
test('T7918', 
      [req_interp,
       extra_run_opts('"' + config.libdir + '"'),
       only_ways(config.ghc_th_way),
       unless(have_dynamic(),skip),
       extra_clean(['T7918A.hi', 'T7918A.o', 'T7918A.dyn_hi', 'T7918A.dyn_o', 
                    'T7918B.hi', 'T7918B.o', 'T7918B.dyn_hi', 'T7918B.dyn_o'])],
      compile_and_run,
      ['-package ghc ' + config.ghc_th_way_flags])
