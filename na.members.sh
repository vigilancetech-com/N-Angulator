#! /bin/sh 
# this program takes a newline seporated list of inodes in its standard input
# and returns a list of 4 lists containing:
#    directory members, directory others, file members, and file others
# where each entry is followed by a list of that entries inodes which are
# in common with pool.
# This has to be as fast as possible


# it might be faster to consolidate these and printf the results to two different
# files but I rather doubt it because it does not nest in.


set *
if [ ! -e "$1" ]; then :
else
    dirs=`find * -maxdepth 0 -type d -printf '"%f" '`
    files=`find * -maxdepth 0 -type f -printf '"%f" '`
fi

myproc=$$ # might need to add an ip number to this eventually, if nfs is used

cat >/tmp/na.inodes.$myproc

doit()
{
    local members='' others=''

    for i in "$@"
    do
	inodes=`echo -n \"
	    find "$i" -type f -printf '%i\n' | 
	    sort -u | 
	    sort -m /tmp/na.inodes.$myproc - | 
	    uniq -d; echo -n \"` 
	if [ -z `eval echo -n "$inodes"|head -1` ]; then
	    others="$others (\"$i\"  "$inodes" ) "
	else
	    members="$members (\"$i\"  "$inodes" ) "
 	fi
    done
    echo '(' "$members" ') (' "$others" ')'
}

echo '('
eval doit "$dirs"
eval doit "$files"
echo ')'

rm /tmp/na.inodes.$myproc
