pro serial_trap_stats_plot, input_stats_file
;
;+
; NAME: serial_trap_stats_plot
;       
; PURPOSE: Plot evolution of mean filling for a trap specie in the
; serial register during readout
;
; CATEGORY: CDM
;
; INPUTS: File storing mean trap stats during readout for a trap specie in
;         the serial register
;         td_ref = Density of trap specie under investigation (as of
;         Dec 2021 the densities from Tom trap-pumping data is
;                  td_ref = [0.02, 0.15, 0.0018, 0.15])
;         section_type = Must be either image/store/serial. If not
;         specified assume it's image
;  
; RETURNS: none
;  
; MODIFICATION HISTORY:
;       Written by:     
;               Claudio Pagani, 14/Dec/2021
;;
; NOTES - TRAP1 = FAST, low density
;         TRAP2 = SLOW, high density
;         TRAP3 = MEDIUM, very low density
;         TRAP4 = VERY FAST, high density
;
; EXAMPLE -
; serial_trap_stats_plot,'/Users/cp232/SMILE/TestHarness/optical_loading/test_runs_pixel/flux1_frames2_T-100C/mean_serial_trap_filling_frn1.txt'

  td_ref = [0.02, 0.15, 0.0018, 0.15]
  if n_params() lt 1 then begin
     print,"Please enter correct parameters: serial_trap_stats_plot, 'mean_serial_trap_filling_frn1.txt'"
     return
  endif

  DEVICE,DECOMPOSED=0.
  tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
  entry_device = !d.name

  readcol,input_stats_file, tr_store, tr_serial, specie, fill, format='(i,i,i,d)'

  n_tr_store = max(tr_store)

  print, 'Type enter to start plotting occupancy along serial register for first specie...'
  keypressed = get_kbrd(10)

  ; Plot for specie 0 
  for i = min(tr_store), max(tr_store) do begin
     it0 = where(specie eq 0 and tr_store eq i)
     plot, tr_serial[it0], fill[it0]/td_ref[0], psym=1, xtit = 'Serial transfer n.', ytit = 'Filling fraction, specie 0'
     print, 'Store transfer n, fill fraction specie0 =', i, mean(fill[it0]/td_ref[0])
     wait, 0.2
  endfor

  print, 'Type enter to continue to next specie...'
  keypressed = get_kbrd(10)

  ; Plot for specie 1 
  for i = min(tr_store), max(tr_store) do begin
     it1 = where(specie eq 1 and tr_store eq i)
     plot, tr_serial[it1], fill[it1]/td_ref[1], psym=1, xtit = 'Serial transfer n.', ytit = 'Filling fraction, specie 1'
     print, 'Store transfer n, fill fraction specie1 =', i, mean(fill[it1]/td_ref[1])
     wait, 0.2
  endfor

  print, 'Type enter to continue to next specie...'
  keypressed = get_kbrd(10)

  ; Plot for specie 2
  for i = min(tr_store), max(tr_store) do begin
     it2 = where(specie eq 2 and tr_store eq i)
     plot, tr_serial[it2], fill[it2]/td_ref[2], psym=1, xtit = 'Serial transfer n.', ytit = 'Filling fraction, specie 2'
     print, 'Store transfer n, fill fraction specie2 =', i, mean(fill[it2]/td_ref[2])
     wait, 0.2
  endfor

  print, 'Type enter to continue to next specie...'
  keypressed = get_kbrd(10)

  ; Plot for specie 2
  for i = min(tr_store), max(tr_store) do begin
     it3 = where(specie eq 3 and tr_store eq i)
     plot, tr_serial[it3], fill[it3]/td_ref[3], psym=1, xtit = 'Serial transfer n.', ytit = 'Filling fraction, specie 3'
     print, 'Store transfer n, fill fraction specie3 =', i, mean(fill[it3]/td_ref[3])
     wait, 0.2
  endfor


  
end

     
  
