def im_sinks_in_front(fname,angle1,angle2,angle3):

  import numpy
  from numpy import zeros
  from numpy import ones
  from numpy import float64
  from numpy import uint8
  from numpy import log10

  import imcol_sinks_in_front


#define other necessary options

  inewdump='y'
  
#  angle1=0
#  angle2=0
#  angle3=90
  iptdim=1000

  print 'angles set to: ',angle1,angle2,angle3

  v1=zeros((iptdim),float64)

  blah=3.14

  nsink=0

  data=imcol_sinks_in_front.imcol_sinks_in_front(fname,inewdump,angle1,angle2,angle3,blah,v1,nsink) 

  gmw=2.

  numdensfac=1./(gmw*1.67e-24)

  v1=data[1]
  nsink=data[2]

  v1=v1[0:nsink]
 
  return v1,nsink
