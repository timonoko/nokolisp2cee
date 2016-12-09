noko progn (load 'lispcee.lsp) (load '%1.lsp)(lispcee '%1 '%1) (nquit 0)
cl /F 2000 /Od /AL /Zi %1.c lispinit.c
