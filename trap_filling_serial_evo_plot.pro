pro trap_filling_serial_evo_plot, trap_serial_stats_file

;
;+
; NAME: trap_filling_serial_evo_plot.pro
;       
; PURPOSE: Plot the evolution of a trap filling along detx during
; readout at each binned line transfer step.
;
; CATEGORY: CDM
;
; INPUTS: trap_serial_stats_file, for example t1_serial_filled_frn0.txt
;         File storing number of filled traps at detX locations during readout.
;
; EXAMPLE: trap_filling_serial_evo_plot, 't1_serial_filled_frn0.txt'
;  
; MODIFICATION HISTORY:
;       Written by:     
;               Claudio Pagani, 4 March 2022
  
; NOTE: Trap stats td_ref = [0.02, 0.15, 0.0018, 0.15]
;  
DEVICE,DECOMPOSED=0.
tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
entry_device = !d.name

td_ref = [0.02, 0.15, 0.0018, 0.15]

readcol, trap_serial_stats_file, transfer1_frn0, detx1_frn0, fill1_frn0, format='(i,i,d)'
for i = 1, max(transfer1_frn0) do begin
   line = where(transfer1_frn0 eq i)
   plot, detx1_frn0[line], fill1_frn0[line], psym=1, background=1, color =0, tit = 'Binned Y transfer '+strtrim(string(i),2), yr = [0, td_ref[1]]
   ;k = get_kbrd(10)
   wait, 0.1
endfor
  
end
