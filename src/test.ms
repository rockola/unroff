.LP
Here comes an EQ/EN pair that just serves defining the delimiters
for inline equations (we will use the dollar sign for this purpose).
No eqn-output should not appear here, and no hypertext reference
should be generated if we are creating GIF files.
.EQ
delim $$
.EN
.LP
Now lets typeset a real equation, again using EQ and EN.
The equation should either be displayed as preformatted text or as
an inline GIF image:
.EQ
size 18 { a+b over 2c = 1 }
.EN
Finally an inline equation such as $size 16 { sqrt a+b }$ which uses the
delimiters defined at the start of this document.
It should either be typeset in Italics or appear as a GIF image.
.LP
Now lets test tables.
Here is a simple table that uses the \f2box\fP attribute and
has two header rows, two columns, and three data rows:
.TS
box, tab(~);
c s
c | c
n n.
Price Development
_
Year~Price
_
1992~$1,500
1993~2,000
1994~12,220
.TE
.LP
The table should either appear as preformatted text or as a GIF
image (the latter will probably turn out a bit too small).
Finally a PIC drawing:
.PS
box "foo"
arrow
box "bar"
.PE
