project('Akari','cpp')
src_assembler=run_command('./srcAssembler.py')
sources=src_assembler.stdout().strip().split('\n')
include_dir='./include'
sfmlWindow=dependency('sfml-window')
glbinding=dependency('glbinding')
swipl=dependency('swipl')
executable('akari',sources,include_directories:[include_dir],dependencies:[sfmlWindow,glbinding,swipl])
