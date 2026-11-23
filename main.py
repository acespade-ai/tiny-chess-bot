import json,os,sys,urllib.request
E=os.environ.get("MAKEFUN_ENV_URL") or sys.exit("no env")
post=lambda p,b:json.load(urllib.request.urlopen(E+p,json.dumps(b).encode()))
B=bytearray(128);KW=KB=SD=0;N=0;BUD=200000
class St(Exception):pass
V=[0]+[100,320,330,500,900,0]*2
KN=[33,31,18,14,-14,-18,-31,-33];KG=[1,-1,16,-16,15,17,-15,-17];DG=[15,17,-15,-17];ST=[1,-1,16,-16]
DS={2:KN,3:DG,4:ST,5:DG+ST,6:KG}
C=lambda q:14-(abs(2*(q&7)-7)+abs(2*(q>>4)-7))
def atk(q,by):
 for d in(-15,-17)if by>0 else(15,17):
  t=q+d
  if 0<=t<128 and not t&136 and B[t]==(1 if by>0 else 7):return 1
 for D,p in((KN,2),(KG,6)):
  for d in D:
   t=q+d
   if 0<=t<128 and not t&136 and B[t]==(p if by>0 else p+6):return 1
 for d in DG+ST:
  t=q+d
  while 0<=t<128 and not t&136:
   v=B[t]
   if v:
    if v in((3,5)if by>0 else(9,11))if d in DG else v in((4,5)if by>0 else(10,11)):return 1
    break
   t+=d
 return 0
def gen(s,cp):
 ms=[]
 for f in range(128):
  if f&136:continue
  v=B[f]
  if not v or(v<=6)!=(s>0):continue
  p=v if v<=6 else v-6
  if p==1:
   d=16*s
   for dd in(d-1,d,d+1):
    t=f+dd
    if t<0 or t>127 or t&136:continue
    w=B[t];pr=t>>4==(7 if s>0 else 0)
    if dd==d:
     if not w:
      if pr:ms+=[f|t<<7|5<<14,f|t<<7|2<<14]
      elif not cp:
       ms.append(f|t<<7)
       if f>>4==(1 if s>0 else 6)and not B[f+2*d]:ms.append(f|f+2*d<<7)
    elif w and(w<=6)!=(s>0):
     ms+=[f|t<<7|5<<14,f|t<<7|2<<14]if pr else[f|t<<7]
  else:
   for d in DS[p]:
    t=f+d
    while 0<=t<128 and not t&136:
     w=B[t]
     if w:
      if(w<=6)!=(s>0):ms.append(f|t<<7)
      break
     if not cp:ms.append(f|t<<7)
     if p in(2,6):break
     t+=d
 return ms
def make(m,s):
 global KW,KB
 f=m&127;t=m>>7&127;pr=m>>14&7;v=B[f];cp=B[t];rf=-1;rt=-1
 B[f]=0;B[t]=(pr if s>0 else pr+6)if pr else v
 if v==6:KW=t
 elif v==12:KB=t
 if(v==6 or v==12)and abs(t-f)==2:
  rf=f+3 if t>f else f-4;rt=f+1 if t>f else f-1;B[rt]=B[rf];B[rf]=0
 return(cp,rf,rt)
def unmake(m,u):
 global KW,KB
 f=m&127;t=m>>7&127;pr=m>>14&7;cp,rf,rt=u;v=B[t]
 B[f]=(1 if v<=6 else 7)if pr else v;B[t]=cp
 if rf>=0:B[rf]=B[rt];B[rt]=0
 if v==6:KW=f
 elif v==12:KB=f
def ev():
 sc=0;ph=0
 for q in range(128):
  if q&136:continue
  v=B[q]
  if not v:continue
  p=v if v<=6 else v-6;w=1 if v<=6 else-1;f=q&7;r=q>>4
  sc+=w*V[p]
  if p>1:ph+=V[p]
  if p==1:sc+=w*8*(r if w>0 else 7-r)
  elif p in(2,3,5):sc+=w*C(q)*(4 if p==2 else 3 if p==3 else 1)
 sc+=(C(KW)-C(KB))*(1500-ph)//300
 return sc
def S(d,a,be,s,pl):
 global N;N+=1
 if N>BUD:raise St
 ck=0
 if d<=0:
  ck=atk(KW if s>0 else KB,-s)
  if not ck:
   st=s*ev()
   if st>=be:return be
   if st>a:a=st
 ms=gen(s,0 if ck or d>0 else 1)
 ms.sort(key=lambda m:-V[B[m>>7&127]])
 best=-10**6;lg=0
 for m in ms:
  u=make(m,s);ks=KW if s>0 else KB
  if atk(ks,-s):unmake(m,u);continue
  lg+=1;sc=-S(d-1,-be,-a,-s,pl+1);unmake(m,u)
  if sc>best:best=sc
  if sc>a:a=sc
  if a>=be:break
 if not lg and(d>0 or ck):return -10**5+pl if atk(KW if s>0 else KB,-s) else 0
 return best if d>0 or ck else a
def parse(fen):
 global KW,KB,SD
 B[:]=bytes(128)
 ps=fen.split();r=7;f=0
 for ch in ps[0]:
  if ch=="/":r-=1;f=0
  elif ch.isdigit():f+=int(ch)
  else:B[r*16+f]="PNBRQKpnbrqk".index(ch)+1;f+=1
 KW=B.index(6);KB=B.index(12)
 SD=1 if ps[1]=="w" else-1
def choose(fen,legal):
 global N
 parse(fen)
 rm=[]
 for u in legal:
  f=(ord(u[0])-97)+(int(u[1])-1)*16;t=(ord(u[2])-97)+(int(u[3])-1)*16
  rm.append(f|t<<7|(5<<14 if len(u)==5 and u[4]!="n" else 2<<14 if len(u)==5 else 0))
 N=0;best=rm[0]
 for d in range(1,13):
  try:
   a=-10**6;be=10**6;bs=-10**6;bb=rm[0]
   for m in rm:
    u=make(m,SD);sc=-S(d-1,-be,-a,-SD,1);unmake(m,u)
    if sc>bs:bs=sc;bb=m
    if sc>a:a=sc
   best=bb
  except St:break
 f=best&127;t=best>>7&127;pr=best>>14&7
 q=lambda x:chr(97+(x&7))+str((x>>4)+1)
 return q(f)+q(t)+("q"if pr==5 else"n"if pr==2 else"")
if __name__=="__main__":
 while True:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   g=post("/move",{"game":g["game"],"move":choose(g["fen"],g["legal_moves"])})
