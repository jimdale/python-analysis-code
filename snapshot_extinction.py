#!/usr/bin/python

import os
import sys

import numpy
from numpy import *

import matplotlib.image as mpimg
import matplotlib.pyplot as plt
import matplotlib.pylab as pylab
from pylab import *
from matplotlib import colors
from matplotlib import rc
rc('font',**{'family':'serif','serif':['Times New Roman'],'size':12})
rc('text',usetex=True)
rc('patch',antialiased=False)

#matplotlib.use('Agg')

import imager_HIIR
import im_sinks_in_front

import fort_dump

import constants
from constants import *

#look for files to process

diri='./'
dirc='../r19t/'

try:
   finfo=sys.argv[1]
except:
   print 'Need filename or filename root'
   raise IndexError

file_list=[]
if len(sys.argv[1])==7:
   fname=sys.argv[1]
   file_list.append(fname)
else:
   fnameroot=sys.argv[1]
   for i in os.listdir(dir):
       if i[:4] == fnameroot:
          file_list.append(i)
   file_list.sort()

nfiles=len(file_list)

if nfiles < 1:
   print 'No files found to process'
   raise IndexError
else:
   print 'Processing ',nfiles,' files'

#variables for screenshots - NB imcol expects units in CODE UNITS

subplots_adjust(hspace=0.001)
subplots_adjust(wspace=0.001)


xmin=-10.
xmax=10.
ymin=-10.
ymax=10.
zmin=-100.
zmax=100.

islice='n'
icolmap='roy'

logsigmax=1
logrange=4
logsigmin=logsigmax-logrange

htemp=57940.

print 'USING HOTTEMP OF ',htemp

#speed of sound

cs=2e4

gcm2_msunpc2=4.783e3
cm2_kpc2=9.523e42
extfactor=1./1.67e-24/1.e21/gcm2_msunpc2
angle1=0.
angle2=0.
angle3=0.

set_cmap('hot')

xlab='x(pc)'
ylab='y(pc)'

#plt.tight_layout()

shr=0.73

extent=(xmin,xmax,ymin,ymax)

for fname in file_list:

   outfile='snap_'+fname

   hottemp=-1.

#call wrapper for imcol

   fnamec=dirc+fname
   fnamei=diri+fname

   coldensnc=imager_HIIR.imager(fnamec,xmin,xmax,ymin,ymax,zmin,zmax,islice,icolmap,logsigmax,logrange,logsigmin,hottemp)
   coldensni=imager_HIIR.imager(fnamei,xmin,xmax,ymin,ymax,zmin,zmax,islice,icolmap,logsigmax,logrange,logsigmin,hottemp)

   outsinkc=im_sinks_in_front.im_sinks_in_front(fnamec,angle1,angle2,angle3)
   outsinki=im_sinks_in_front.im_sinks_in_front(fnamei,angle1,angle2,angle3)

   sigmascc=outsinkc[0]

   sigmascc=[sc*extfactor for sc in sigmascc]

   sigmasci=outsinki[0]

   sigmasci=[sc*extfactor for sc in sigmasci]

   out=fort_dump.fort_dump(fnamec)

#move variables from out into more useful places

   umass=out[0]
   utime=out[1]
   udist=out[2]
   npart=out[3]
   gt=out[4]
   tkin=out[5]
   tgrav=out[6]
   tterm=out[7]
   escap=out[8]
   rhotemp=out[9]
   potentemp=out[10]

   iphasetemp=zeros((npart),dtype=int8)

   xtemp=out[11]
   ytemp=out[12]
   ztemp=out[13]
   mtemp=out[14]
   htemp=out[15]

   vxtemp=out[16]
   vytemp=out[17]
   vztemp=out[18]
   utemp=out[19]

   iphasetemp=out[20]

   sinkxc=[]
   sinkyc=[]
   sinkzc=[]
   sinkmc=[]

   for i in range(npart):
      if iphasetemp[i]>0:
        sinkxc.append(xtemp[i])
        sinkyc.append(ytemp[i])
        sinkzc.append(ztemp[i])
        sinkmc.append(mtemp[i])

   out=fort_dump.fort_dump(fnamei)

#move variables from out into more useful places

   umass=out[0]
   utime=out[1]
   udist=out[2]
   npart=out[3]
   gt=out[4]
   tkin=out[5]
   tgrav=out[6]
   tterm=out[7]
   escap=out[8]
   rhotemp=out[9]
   potentemp=out[10]

   iphasetemp=zeros((npart),dtype=int8)

   xtemp=out[11]
   ytemp=out[12]
   ztemp=out[13]
   mtemp=out[14]
   htemp=out[15]

   vxtemp=out[16]
   vytemp=out[17]
   vztemp=out[18]
   utemp=out[19]

   iphasetemp=out[20]

   sinkxi=[]
   sinkyi=[]
   sinkzi=[]
   sinkmi=[]

   for i in range(npart):
       if iphasetemp[i]>0:
          sinkxi.append(xtemp[i])
          sinkyi.append(ytemp[i])
          sinkzi.append(ztemp[i])
          sinkmi.append(mtemp[i])

   fig=plt.figure(figsize=(12,12))

   nsinkc=len(sinkxc)
   nsinki=len(sinkxi)

   plt.subplot(2,2,1)

   normsig=colors.Normalize(vmin=logsigmin,vmax=logsigmax)
   imgplot=plt.imshow(coldensnc,extent=extent,origin='lower',interpolation='nearest')
   imgplot.set_norm(normsig)

   for isink in range(nsinkc):
       msize=2.*log10(sinkmc[isink])
       plt.plot(sinkxc[isink],sinkyc[isink],'.',markersize=msize,color='white',alpha=exp(-sigmascc[isink]))
#       plt.plot(sinkx[isink],sinky[isink],'o',markersize=msize,markeredgewidth=0.0,markeredgecolor=None,color='white',aa=True,alpha=exp(-sigmasc[isink]))

   plt.xlim(xmin, xmax)
   plt.ylim(ymin, ymax)
   xlabel(xlab)
   ylabel(ylab)

   plt.subplot(2,2,2)

   imgplot=plt.imshow(coldensni,extent=extent,origin='lower',interpolation='nearest')
   imgplot.set_norm(normsig)

   for isink in range(nsinki):
       msize=2.*log10(sinkmi[isink])
       plt.plot(sinkxi[isink],sinkyi[isink],'.',markersize=msize,color='white',alpha=exp(-sigmasci[isink]))

   plt.xlim(xmin, xmax)
   plt.ylim(ymin, ymax)
   xlabel(xlab)
   ylabel(ylab)

   plt.subplot(2,2,3)

   imgplot=plt.imshow(coldensnc,extent=extent,origin='lower',interpolation='nearest')
   imgplot.set_norm(normsig)

   for isink in range(nsinkc):
       msize=2.*log10(sinkmc[isink])
       plt.plot(sinkxc[isink],sinkyc[isink],'.',markersize=msize,color='white',alpha=1.00)

   plt.xlim(xmin, xmax)
   plt.ylim(ymin, ymax)
   xlabel(xlab)
   ylabel(ylab)

   plt.subplot(2,2,4)

   imgplot=plt.imshow(coldensni,extent=extent,origin='lower',interpolation='nearest')
   imgplot.set_norm(normsig)

   for isink in range(nsinki):
       msize=2.*log10(sinkmi[isink])
       plt.plot(sinkxi[isink],sinkyi[isink],'.',markersize=msize,color='white',alpha=1.00)

   plt.xlim(xmin, xmax)
   plt.ylim(ymin, ymax)
   xlabel(xlab)
   ylabel(ylab)


   cax = plt.axes([0.91, 0.105, 0.01, 0.79])

   cb=plt.colorbar(imgplot, cax=cax)
   cb.set_label('Column density (g cm$^{-2}$)')

   plt.savefig('snap_extinction_'+fname+'.png',dpi=300,bbox_inches='tight')

print 'Done!'
