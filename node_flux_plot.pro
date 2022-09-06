pro node_flux_plot

;
;+
; NAME: node_flux_plot.pro
;       
; PURPOSE: Plot flux at node output for each binned line during a
;frame readout
;
; CATEGORY: CDM
;
; INPUTS: node_flux_frn0.txt
;         File storing flux values at node at the end of the readout process
;
; MODIFICATION HISTORY:
;       Written by:     
;               Claudio Pagani, 2 March 2022
  
  ;;;; Read in flux at node
DEVICE,DECOMPOSED=0.
tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
entry_device = !d.name

readcol,'node_flux_frn0.txt', x, y, nodeflux, format='(i,i,d)'
for i = 1, max(y) do begin
   line = where(y eq i)
   plot, x[line], nodeflux[line], psym=1, background=1, color =0, tit = 'Binned Y transfer '+strtrim(string(i),2)
   k = get_kbrd(10)
   ;wait, 0.1
endfor
  
end
