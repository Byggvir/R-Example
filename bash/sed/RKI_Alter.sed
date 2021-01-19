# RKIAlter: Prepare CSV Table for awk
# 
1d;
/Gesamt/d;
s#[" ]##g;
s#[-+][^,]*,#,#;
h;
:a;
x;
s#^\([^,]*\),\([0-9]*\).*#\1,\2#p;
g;
s#^\([^,]*\),\([0-9]*\)#\1#;
t a;
