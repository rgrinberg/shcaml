:c
/\\$/{
N
s/\\\n//
bc
}
s/\.cm[ix]/.cmo/g
:m
s/\([^\n]*\): *\([^ ][^ ]*\)/\2 \1\
\1:/
tm
s/\n[^\n]*:.*//
