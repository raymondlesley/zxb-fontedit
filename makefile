# makefile - zxb-fontedit

# builds a TZX format output file
#
# * "compiles" loader from Sinclair BASIC
# * cross-compiles ZX Basic program into TAP (includes a loader I don't want)
# * splits cross-compiled TAP
# * builds TZX from component parts

zxbc = ../../Python/zxbasic/zxbc.py
zxbc_opts = --tzx --BASIC

bas2tap = ../bas2tap/bas2tap
bas2tap_opts = -a10

tzxmerge = ../../Python/tzxtools/tzxmerge.py
tzxsplit = ../../Python/tzxtools/tzxsplit.py
split_opts = -1


zxb-fontedit.tzx: zxb-fontedit.bas
	${zxbc} ${zxbc_opts} zxb-fontedit.bas -o zxb-fontedit.tzx --autorun
