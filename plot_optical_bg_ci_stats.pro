pro plot_optical_bg_ci_stats
;
;+
; NAME: plot_optical_bg_ci_stats.pro
;       
; PURPOSE: Plot statistics of flux and trap occupancy
;
; CATEGORY: CDM
;
; INPUTS: ima_evo.txt
;         File storing flux values along image during 4315 parallel
;         readout transfers of the image section.
;  
;         t0_image_filled_endImageTr.txt
;         t1_image_filled_endImageTr.txt
;         t2_image_filled_endImageTr.txt
;         t3_image_filled_endImageTr.txt  
;         Files storing traps 1-4 occupancy stats during parallel
;         readout of the image section from start to end of image
;         frame readout.
;
; MODIFICATION HISTORY:
;       Written by:     
;               Claudio Pagani, 18/Oct/2021
;       Modified:
;               Claudio Pagani, 28/Feb/2022
;               Changed some of the plots to examine X-rays evolution
;               and trap stats when CI is off.
;  
; NOTES - TRAP1 = FAST, low density
;         TRAP2 = SLOW, high density
;         TRAP3 = MEDIUM, very low density
;         TRAP4 = VERY FAST, high density
;
  
;Graphics setup
DEVICE,DECOMPOSED=0.
tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
entry_device = !d.name

;;; FLUX AT STORE/SERIAL INTERFACE FOR 2 FRAMES.
;;; PLOT FRACTIONAL LOSSES, COLOR CODED.

readcol,'store_flux_frn1.txt', st1, flux1, format='(i,d)'
readcol,'store_flux_frn0.txt', st0, flux0, format='(i,d)'
flux_ref = dblarr(n_elements(flux0))
flux_ref[index] = [150.0, 500.0, 1600.0, 500.0, 1600.0, 150.0, 1600.0, 150.0, 500.0]
ox = where(flux_ref eq 150)
al = where(flux_ref eq 500)
mn = where(flux_ref eq 1600)
plot, st0, (flux_ref-flux0)/flux_ref, psym=1, background=1, color = 0, xtit = 'Store transfer N', ytit = 'Fractional losses: (FluxInput - FluxSerial)/FluxInput', tit = 'CTI effects: (black/red/blue = O/Al/Mn), +=Frame0, * = Frame1'
oplot, st0[al], (flux_ref[al]-flux0[al])/flux_ref[al], psym=1, color =2
oplot, st0[mn], (flux_ref[mn]-flux0[mn])/flux_ref[mn], psym=1, color =3
oplot, st0, (flux_ref-flux1)/flux_ref, psym=2, color = 0
oplot, st0[al], (flux_ref[al]-flux1[al])/flux_ref[al], psym=2, color =2
oplot, st0[mn], (flux_ref[mn]-flux1[mn])/flux_ref[mn], psym=2, color =3

entry_device = !d.name
plotfilenameps = 'flux_losses_at_store_serial_bg0_noci_t-120C.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, st0, (flux_ref-flux0)/flux_ref, psym=1, background=1, color = 0, xtit = 'Store transfer N', ytit = 'Fractional losses: (FluxInput - FluxSerial)/FluxInput', tit = 'CTI effects: (black/red/blue = O/Al/Mn), +=Frame0, * = Frame1'
oplot, st0[al], (flux_ref[al]-flux0[al])/flux_ref[al], psym=1, color =2
oplot, st0[mn], (flux_ref[mn]-flux0[mn])/flux_ref[mn], psym=1, color =3
oplot, st0, (flux_ref-flux1)/flux_ref, psym=2, color = 0
oplot, st0[al], (flux_ref[al]-flux1[al])/flux_ref[al], psym=2, color =2
oplot, st0[mn], (flux_ref[mn]-flux1[mn])/flux_ref[mn], psym=2, color =3
device,/close  
set_plot,entry_device



readcol, 'ima_evo_frn0.txt', flux, format='(d)'
readcol, 'ima_evo_frn1.txt', flux1, format='(d)'

;;; Select flux in a pixel as it moves down the column during readout.
n_lines_image = 3791
index_pixel = lonarr(n_lines_image)

;;; Plot an amimation of the image
for j = 0, 4314 do begin
   if j mod 5 eq 0 then begin
      plot, flux[long(3791)*j:long(3791)*j+3790], color=0, background=1, psym=1, tit = string(j), xr = [0, 500], xtit = 'DETY', yr = [100, 2000], /ylog, yst=1
      wait, 0.5
   endif
endfor

;;; Plot of flux losses from MnKalpha event at Y = 2200

for j = 1, n_lines_image do index_pixel[j-1] = long(n_lines_image*1.0*j -j-1590)  
entry_device = !d.name
plotfilenameps = 'flux_xray1600_dety2200_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, indgen(3791)+1, flux[index_pixel], yr = [1450, 1600], background=1, color = 0, xtit = 'Transfer N', ytit = 'Flux [e-]', tit = 'DETY = 2200 Mn-kalpha Flux evo - Black/Red = Frame0/1',xr = [1, 2300], xst=1, yst=1, psym=3
oplot, indgen(3791)+1, flux1[index_pixel], color=3, psym=3
device,/close  
set_plot,entry_device

;;; Plot of flux losses from Al event at Y = 2000
for j = 1, n_lines_image do index_pixel[j-1] = long(n_lines_image*1.0*j -j-1790)  
entry_device = !d.name
plotfilenameps = 'flux_xray500_dety2000_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, indgen(3791)+1, flux[index_pixel], yr = [350, 500], background=1, color = 0, xtit = 'Transfer N', ytit = 'Flux [e-]', tit = 'DETY = 2000 Al Flux evo - Black/Red = Frame0/1',xr = [1, 2300], xst=1, yst=1, psym=3
oplot, indgen(3791)+1, flux1[index_pixel], color=3, psym=3
device,/close  
set_plot,entry_device


;;; Plot of flux losses from MnKalpha event at Y = 3300

for j = 1, n_lines_image do index_pixel[j-1] = long(n_lines_image*1.0*j -j-490)  
entry_device = !d.name
plotfilenameps = 'flux_xray1600_dety3300_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, indgen(3791)+1, flux[index_pixel], yr = [1400, 1600], background=1, color = 0, xtit = 'Transfer N', ytit = 'Flux [e-]', tit = 'DETY = 3300 Mn-kalpha Flux evo - Black/Red = Frame0/1',xr = [1, 3300], xst=1, yst=1, psym=3
oplot, indgen(3791)+1, flux1[index_pixel], color=3, psym=3
device,/close  
set_plot,entry_device


;;; Plot of flux losses from Al event at Y = 3700

for j = 1, n_lines_image do index_pixel[j-1] = long(n_lines_image*1.0*j -j-90)  
entry_device = !d.name
plotfilenameps = 'flux_xray500_dety3700_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, indgen(3791)+1, flux[index_pixel], yr = [400, 500], background=1, color = 0, xtit = 'Transfer N', ytit = 'Flux [e-]', tit = 'DETY = 3700 Al Flux evolution - Black/Red = Frame0/1',xr = [1, 4000], xst=1, yst=1, psym=3
oplot, indgen(3791)+1, flux1[index_pixel], color=3, psym=3
device,/close  
set_plot,entry_device


;;; Plot of flux losses from Oxygen event at Y = 2500

for j = 1, n_lines_image do index_pixel[j-1] = long(n_lines_image*1.0*j -j-1290)  
entry_device = !d.name
plotfilenameps = 'flux_xray150_dety2500_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, indgen(3791)+1, flux[index_pixel], yr = [110, 150], background=1, color = 0, xtit = 'Transfer N', ytit = 'Flux [e-]', tit = 'DETY = 2500 O Flux evo - Black/Red = Frame0/1',xr = [1, 2500], xst=1, yst=1, psym=3
oplot, indgen(3791)+1, flux1[index_pixel], color=3, psym=3
device,/close  
set_plot,entry_device



;;; TRAP FILLING STATS

readcol,'t0_image_filled_frn0.txt',transfer0, dety0, fill0, format='(i,i,d)'
readcol,'t1_image_filled_frn0.txt',transfer1, dety1, fill1, format='(i,i,d)'
readcol,'t2_image_filled_frn0.txt',transfer2, dety2, fill2, format='(i,i,d)'

readcol,'t1_image_filled_frn1.txt',f1transfer1, f1dety1, f1fill1, format='(i,i,d)'

td_ref = [0.02, 0.15, 0.0018, 0.15]

;;; Some good examples are from Y=200 and 2000 for example
;;; The fastest trap is very fast, always empty, no point in plotting it in image/store

;;; Follow the filling of the trap at pixel Y=200 during transfer
pix200 = where(dety0 eq 200)

plot, transfer0[pix200], fill0[pix200]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = 'DETY = 200'
plot, transfer1[pix200], fill1[pix200]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 200'
plot, transfer2[pix200], fill2[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/slow)', tit = 'DETY = 200'


pix2000 = where(dety0 eq 2000)
plot, transfer0[pix2000], fill0[pix2000]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = 'DETY = 2000'
plot, transfer1[pix2000], fill1[pix2000]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 2000'
plot, transfer2[pix2000], fill2[pix2000]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/slow)', tit = 'DETY = 2000'

readcol,'t0_store_filled.txt',transfer0, dety0, fill0, format='(i,i,d)'
readcol,'t1_store_filled.txt',transfer1, dety1, fill1, format='(i,i,d)'
readcol,'t2_store_filled.txt',transfer2, dety2, fill2, format='(i,i,d)'

pix200 = where(dety0 eq 200)

plot, transfer0[pix200], fill0[pix200]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = 'DETY = 200'
plot, transfer1[pix200], fill1[pix200]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 200'
plot, transfer2[pix200], fill2[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/slow)', tit = 'DETY = 200'


pix10 = where(dety0 eq 10)

plot, transfer0[pix10], fill0[pix10]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = 'DETY = 10'
plot, transfer1[pix10], fill1[pix10]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 10'
plot, transfer2[pix10], fill2[pix10]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/slow)', tit = 'DETY = 10'

pix700 = where(dety0 eq 700)

plot, transfer0[pix700], fill0[pix700]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = 'DETY = 700'
plot, transfer1[pix700], fill1[pix700]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 700'
plot, transfer2[pix700], fill2[pix700]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/slow)', tit = 'DETY = 700'


;;; Plot of trap filling, E-centre, Y=10, Y=3000

pix10 = where(dety1 eq 10)
entry_device = !d.name
plotfilenameps = 't1_image_filling_dety10_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, transfer1[pix10], fill1[pix10]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 10 - Black/Red = Frame0/1', psym=1, background=1, color = 0
oplot, f1transfer1[pix10], f1fill1[pix10]/td_ref[1], color =3, psym=1    
device,/close  
set_plot,entry_device

pix3000 = where(dety1 eq 3000)
entry_device = !d.name
plotfilenameps = 't1_image_filling_dety3000_frn0_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
plot, transfer1[pix3000], fill1[pix3000]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = 'DETY = 3000 - Black/Red = Frame0/1', psym=1, background=1, color = 0, yr=[0,1]
oplot, f1transfer1[pix3000], f1fill1[pix3000]/td_ref[1], color =3, psym=1    
device,/close  
set_plot,entry_device


;;; ------------- STORE ----------

readcol,'t0_store_filled_frn0.txt',transfer0_frn0, dety0_frn0, fill0_frn0, format='(i,i,d)'
pix200 = where(dety0_frn0 eq 200)
plot, transfer0_frn0[pix200], fill0_frn0[pix200]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE -- DETY = 200', background=1, color = 0, /xlog

plotfilenameps = 'store_trap2stats_dety200_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer0_frn0[pix200], fill0_frn0[pix200]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE, FRAME 0 -- DETY = 200', background=1, color = 0, xr = [500, 720]
device,/close  
set_plot,entry_device

pix700 = where(dety0_frn0 eq 700)
plot, transfer0_frn0[pix700], fill0_frn0[pix700]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, /xlog

plotfilenameps = 'store_trap0stats_dety700_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer0_frn0[pix700], fill0_frn0[pix700]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 720]
device,/close  
set_plot,entry_device


readcol,'t0_store_filled_frn1.txt',transfer0_frn1, dety0_frn1, fill0_frn1, format='(i,i,d)'
pix700 = where(dety0_frn1 eq 700)
plot, transfer0_frn1[pix700], fill0_frn1[pix700]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 720]

plotfilenameps = 'store_trap0stats_dety700_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer0_frn1[pix700], fill0_frn1[pix700]/td_ref[0], xtit = 'Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 720]
device,/close  
set_plot,entry_device




readcol,'t1_store_filled_frn0.txt',transfer1_frn0, dety1_frn0, fill1_frn0, format='(i,i,d)'
pix700 = where(dety1_frn0 eq 700)
plot, transfer1_frn0[pix700], fill1_frn0[pix700]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 720]

plotfilenameps = 'store_trap1stats_dety700_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer1_frn0[pix700], fill1_frn0[pix700]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 1438]
device,/close  
set_plot,entry_device

readcol,'t1_store_filled_frn1.txt',transfer1_frn1, dety1_frn1, fill1_frn1, format='(i,i,d)'
pix700 = where(dety1_frn1 eq 700)
plot, transfer1_frn1[pix700], fill1_frn1[pix700]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 720]

plotfilenameps = 'store_trap1stats_dety700_frn0_zoomlate.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer1_frn0[pix700], fill1_frn0[pix700]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Slow)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 1438], yr = [0.998, 1.002]
device,/close  
set_plot,entry_device


pix200 = where(dety1_frn1 eq 200)
plot, transfer1_frn0[pix200], fill1_frn0[pix200]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1438], yr = [0.998, 1.002]


readcol,'t1_store_filled_frn1.txt',transfer1_frn1, dety1_frn1, fill1_frn1, format='(i,i,d)'
pix200 = where(dety1_frn1 eq 200)
plot, transfer1_frn1[pix200], fill1_frn1[pix200]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1438], yr = [0.998, 1.002]


plotfilenameps = 'store_trap1stats_dety200_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer1_frn1[pix200], fill1_frn1[pix200]/td_ref[1], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1438], yr = [0.998, 1.002]
device,/close  
set_plot,entry_device



readcol,'t2_store_filled_frn0.txt',transfer2_frn0, dety2_frn0, fill2_frn0, format='(i,i,d)'
pix700 = where(dety2_frn0 eq 700)
plot, transfer2_frn0[pix700], fill2_frn0[pix700]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 1440]

pix200 = where(dety2_frn0 eq 200)
plot, transfer2_frn0[pix200], fill2_frn0[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1440]

plotfilenameps = 'store_trap2stats_dety700_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer2_frn0[pix700], fill2_frn0[pix700]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 1440]
device,/close  
set_plot,entry_device

plotfilenameps = 'store_trap2stats_dety200_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer2_frn0[pix200], fill2_frn0[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1440]
device,/close  
set_plot,entry_device

readcol,'t2_store_filled_frn1.txt',transfer2_frn1, dety2_frn1, fill2_frn1, format='(i,i,d)'
pix200 = where(dety2_frn1 eq 200)
plot, transfer2_frn1[pix200], fill2_frn1[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' STORE -- DETY = 200', background=1, color = 0, xr = [1, 1440]

plotfilenameps = 'store_trap2stats_dety200_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer2_frn1[pix200], fill2_frn1[pix200]/td_ref[2], xtit = 'Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' FRAME1, STORE -- DETY = 200', background=1, color = 0, xr = [1, 1440]

device,/close  
set_plot,entry_device

readcol,'t3_store_filled_frn0.txt',transfer3_frn0, dety3_frn0, fill3_frn0, format='(i,i,d)'
pix700 = where(dety3_frn0 eq 700)
plot, transfer3_frn0[pix700], fill3_frn0[pix700]/td_ref[3], xtit = 'Tranfer n.', ytit = 'Fill fraction trap3 (Very fast)', tit = ' STORE -- DETY = 700', background=1, color = 0, xr = [1, 1440]



;;; ------------- SERIAL ----------
;;;
;;;  t0_serial_filled_frn0.txt --> Trap occupancy stats up to the end
;;;  of the store readout.
;;;
;;; t0_serial_final_filled_frn0.txt --> Trap occupancy after EDU time,
;;; when the following frame has been exposed and its image readout
;;; process starts.


readcol,'t0_serial_filled_frn0.txt',transfer0_frn0, detx0_frn0, fill0_frn0, format='(i,i,d)'
pix1500 = where(detx0_frn0 eq 1500)
plot, transfer0_frn0[pix1500], fill0_frn0[pix1500]/td_ref[0], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' SERIAL -- DETX = 200', background=1, color = 0, /xlog

pix20 = where(detx0_frn0 eq 20)
pix100 = where(detx0_frn0 eq 100)
pix500 = where(detx0_frn0 eq 500)
pix800 = where(detx0_frn0 eq 800)

plot, transfer0_frn0[pix20], fill0_frn0[pix20]/td_ref[0], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' SERIAL -- DETX = 20 (Black), 100 (Blue), 500 (Red)', background=1, color = 0
oplot, transfer0_frn0[pix100], fill0_frn0[pix100]/td_ref[0], color=4
oplot, transfer0_frn0[pix500], fill0_frn0[pix500]/td_ref[0], color=3


plotfilenameps = 'serial_trap0stats_detx20_100_500_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer0_frn0[pix20], fill0_frn0[pix20]/td_ref[0], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' SERIAL -- DETX = 20 (Black), 100 (Blue), 500 (Red)', background=1, color = 0
oplot, transfer0_frn0[pix100], fill0_frn0[pix100]/td_ref[0], color=4
oplot, transfer0_frn0[pix500], fill0_frn0[pix500]/td_ref[0], color=3
device,/close  
set_plot,entry_device

plotfilenameps = 'serial_trap0stats_detx20_100_500_frn0_zoom_end.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer0_frn0[pix20], fill0_frn0[pix20]/td_ref[0], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap0 (Fast)', tit = ' SERIAL -- DETX = 20 (Black), 100 (Blue), 500 (Red)', background=1, color = 0, xr = [600, 720]
oplot, transfer0_frn0[pix100], fill0_frn0[pix100]/td_ref[0], color=4
oplot, transfer0_frn0[pix500], fill0_frn0[pix500]/td_ref[0], color=3
device,/close  
set_plot,entry_device

readcol,'t0_serial_final_filled_frn0.txt', fdetx0_frn0, ffill0_frn0, format='(i,d)'
print, mean(ffill0_frn0), min(ffill0_frn0), max(ffill0_frn0)


readcol,'t1_serial_filled_frn0.txt', transfer1_frn0, detx1_frn0, fill1_frn0, format='(i,i,d)'

pix100 = where(detx1_frn0 eq 100)
pix1000 = where(detx1_frn0 eq 1000)

plot, transfer1_frn0[pix100], fill1_frn0[pix100]/td_ref[1], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' SERIAL -- DETX = 100 (Black), 1800 (Red)', background=1, color = 0, yr = [0.1, 1.1], yst=1, psym=1
oplot, transfer1_frn0[pix1000], fill1_frn0[pix1000]/td_ref[1], color=3, psym=1

plotfilenameps = 'serial_trap1stats_detx100_1000_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer1_frn0[pix100], fill1_frn0[pix100]/td_ref[1], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' SERIAL -- DETX = 100 (Black), 1000 (Red). FRAME0', background=1, color = 0, /ylog, yr = [0.1, 1.1], yst=1, psym=3
oplot, transfer1_frn0[pix1000], fill1_frn0[pix1000]/td_ref[1], color=3, psym=3
device,/close  
set_plot,entry_device


readcol,'t1_serial_filled_frn1.txt', transfer1_frn1, detx1_frn1, fill1_frn1, format='(i,i,d)'

pix100 = where(detx1_frn1 eq 100)
pix800 = where(detx1_frn1 eq 800)
pix1000 = where(detx1_frn1 eq 1000)

plot, transfer1_frn1[pix100], fill1_frn1[pix100]/td_ref[1], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' SERIAL -- DETX = 100 (Black), 1800 (Red)', background=1, color = 0, yr = [0.1, 1.1], yst=1, psym=1
oplot, transfer1_frn1[pix1000], fill1_frn1[pix1000]/td_ref[1], color=3, psym=1

plotfilenameps = 'serial_trap1stats_detx100_800_frn1.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer1_frn1[pix100], fill1_frn1[pix100]/td_ref[1], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap1 (Slow)', tit = ' SERIAL -- DETX = 100 (Black), 800 (Red). FRAME1', background=1, color = 0, /ylog, yr = [0.1, 1.1], yst=1, psym=3
oplot, transfer1_frn1[pix800], fill1_frn1[pix800]/td_ref[1], color=3, psym=3
device,/close  
set_plot,entry_device




readcol,'t1_serial_final_filled_frn0.txt', fdetx1_frn0, ffill1_frn0, format='(i,d)'
print, mean(ffill1_frn0), min(ffill1_frn0), max(ffill1_frn0)


readcol,'t2_serial_filled_frn0.txt',transfer2_frn0, detx2_frn0, fill2_frn0, format='(i,i,d)'
pix1500 = where(detx2_frn0 eq 1500)
plot, transfer2_frn0[pix1500], fill2_frn0[pix1500]/td_ref[2], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' SERIAL -- DETX = 200', background=1, color = 0

pix20 = where(detx2_frn0 eq 20)
pix100 = where(detx2_frn0 eq 100)
pix500 = where(detx2_frn0 eq 500)
pix800 = where(detx2_frn0 eq 800)
pix1800 = where(detx2_frn0 eq 1800)

plot, transfer2_frn0[pix20], fill2_frn0[pix20]/td_ref[2], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' SERIAL -- DETX = 20 (Black), 100 (Blue), 500 (Red)', background=1, color = 0, yr=[0.5, 1.02], psym=1
oplot, transfer2_frn0[pix100], fill2_frn0[pix100]/td_ref[2], color=4
oplot, transfer2_frn0[pix500], fill2_frn0[pix500]/td_ref[2], color=3

plotfilenameps = 'serial_trap2stats_detx20_1500_frn0.ps'
set_plot,'ps'
device, filename = plotfilenameps, /color
plot, transfer2_frn0[pix20], fill2_frn0[pix20]/td_ref[2], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap2 (Medium/Slow)', tit = ' SERIAL -- DETX = 20 (Black), 1500 (Blue), 1800 (Red)', background=1, color = 0, yr=[0.8, 1.02], yst=1
oplot, transfer2_frn0[pix1500], fill2_frn0[pix1500]/td_ref[2], color=4
oplot, transfer2_frn0[pix1800], fill2_frn0[pix1800]/td_ref[2], color=3
device,/close  
set_plot,entry_device

readcol,'t2_serial_final_filled_frn0.txt', fdetx2_frn0, ffill2_frn0, format='(i,d)'
print, mean(ffill2_frn0), min(ffill2_frn0), max(ffill2_frn0)


readcol,'t3_serial_filled_frn0.txt',transfer3_frn0, detx3_frn0, fill3_frn0, format='(i,i,d)'
pix1500 = where(detx3_frn0 eq 1500)
plot, transfer3_frn0[pix1500], fill3_frn0[pix1500]/td_ref[3], xtit = 'Store Tranfer n.', ytit = 'Fill fraction trap3 (Very fast)', tit = ' SERIAL -- DETX = 1500', background=1, color = 0



end
  
