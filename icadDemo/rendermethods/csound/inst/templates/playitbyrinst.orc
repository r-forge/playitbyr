/* ----R Header: P-field list-----
inst, start, dur, pitch, amp, pan, 
p1    p2     p3   p4     p5   p6
-----end header---- */


sr=44100
kr=4410
0dbfs=1
nchnls=2

    instr 1
/* 'drum' timbre */
  ipan      =  p6
  ipitch    =  cpsoct(p4)
  kenv      linen     p5, 0.01*p3, p3, 0.99*p3
  anoise    rand      kenv
  asig      reson     anoise, ipitch, 500
            outs      asig*ipan, asig*(1-ipan)
    endin


    instr 2
  ifr =         cpsoct(p4)
  ipan      =  p6
  asig        oscil      p5, ifr, 1
            outs      asig*ipan, asig*(1-ipan)
    endin