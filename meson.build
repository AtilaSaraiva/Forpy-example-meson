project('forpy test', 'fortran')


python3_config = find_program('python3-config')
flagsObj       = run_command([python3_config, '--ldflags', '--embed'])
flags          = flagsObj.stdout().split()

executable('forpy', ['forpy_mod.F90','intro.f90'],
  link_args: flags)
executable('forpyplot', ['forpy_mod.F90','matplot.f90'],
  link_args: flags)
