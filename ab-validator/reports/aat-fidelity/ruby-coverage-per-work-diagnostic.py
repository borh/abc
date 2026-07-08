import json,glob,os,collections,statistics
GLOBS={
 "aozora":"/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter/*.json",
 "aozora-rs":"/db/ab-validator/fidelity-corpus/aozora-rs/aat/aozora-rs-adapter/*.json",
 "aozora2html":"/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/*.json",
}
def wid(p):  # work id from filename: 000005_5-<hash>.json
    b=os.path.basename(p); return b.split('-')[0]
def rubycount(path):
    try: blocks=json.load(open(path)).get("blocks",[])
    except: return None
    c=[0]; st=[blocks]
    while st:
        v=st.pop()
        if isinstance(v,dict):
            k=v.get('kind')
            if k=='ruby' or v.get('x-source-marker-kind')=='ruby': c[0]+=1
            st.extend(v.values())
        elif isinstance(v,list): st.extend(v)
    return c[0]
per={}
for lab,g in GLOBS.items():
    d={}
    for f in glob.glob(g):
        r=rubycount(f)
        if r is not None: d[wid(f)]=d.get(wid(f),0)+r
    per[lab]=d
    print(f"{lab}: {len(d)} works, total ruby {sum(d.values()):,}")
# shared works a2html vs aozora-rs
sh=set(per['aozora2html'])&set(per['aozora-rs'])
print(f"\nshared works (a2html & rs): {len(sh)}")
a2=sum(per['aozora2html'][w] for w in sh); rs=sum(per['aozora-rs'][w] for w in sh)
print(f"  on shared works: a2html ruby {a2:,} / aozora-rs ruby {rs:,} = {a2/rs:.3f}")
# works a2html missing entirely (in rs but not a2html) and their ruby mass
missing=set(per['aozora-rs'])-set(per['aozora2html'])
mruby=sum(per['aozora-rs'][w] for w in missing)
print(f"  works aozora-rs has but a2html MISSING: {len(missing)} works, {mruby:,} ruby ({100*mruby/rs:.1f}% of rs ruby)")
# per-work ratio distribution on shared works with rs ruby>0
ratios=[per['aozora2html'][w]/per['aozora-rs'][w] for w in sh if per['aozora-rs'][w]>0]
zero=sum(1 for w in sh if per['aozora-rs'][w]>0 and per['aozora2html'][w]==0)
print(f"  per-work ratio (shared, rs>0): median={statistics.median(ratios):.3f} mean={statistics.mean(ratios):.3f}")
print(f"  works where rs>0 but a2html=0 ruby: {zero}")
