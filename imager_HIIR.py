def imager(fname,xmin,xmax,ymin,ymax,zmin,zmax,islice,icolmap,logmax,logrange,logmin,hottemp):

  import numpy
  from numpy import zeros
  from numpy import ones
  from numpy import float64
  from numpy import uint8
  from numpy import log10

  import PIL
  from PIL import Image

#  import imcol_python_ions
  import imcol_python_EM

  import colorsys
  from colorsys import hsv_to_rgb

#define other necessary options

  inewdump='y'
  isink='y'
  sinksize=0.
  sinkrho=0.
  icent='n'
  iline=100
  maxline=2001
  angle1=0
  angle2=0
  angle3=0
  idt='d'
  blah=3.14159265359
  v1=zeros((maxline,maxline))


  coldens=zeros((maxline,maxline),float64)


  data=imcol_python_EM.imcol_python(fname,xmin,xmax,ymin,ymax,zmin,zmax,
                               inewdump,islice,isink,sinksize,sinkrho,icent,
                               iline,iline,angle1,angle2,angle3,idt,blah,v1,
                               hottemp)

  gmw=2.

  if hottemp>0:
    surfdensfac=1.991e33*1.991e33/(3.086e18**5.)/1.67e-24
  else:
    surfdensfac=1.991e33/3.086e18**2.

  coldens=data[1]

  coldens=coldens[0:iline,0:iline]

  coldens=coldens*surfdensfac

  coldens=log10(coldens)

  return coldens
