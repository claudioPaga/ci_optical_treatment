pro debug_serial_multiEn, input_serial_stats_file, binnedy, xray_id, bg_input

;;; CP, 22 March 2022
;;;
;;; Summary - Inspect the output statistics from the processing of a
;;;           diffuse optical bg + CI lines for pixels with X-ray
;;;           events.
;;;           X-ray events were positioned at specific DETY - locations,
;;;           at binned line  16, 33, 66, 333, 366, 416, 550, 583, 616
;;;           and at DETX = 600, 1200, 1800 (these are identified as
;;;           xray_id = 0, 1, 2)
;;;  
;;;           
;;; Example - debug_serial_multiEn, 'xrays_serial_evo_bg01.txt', 33, 0
;;;           debug_serial_multiEn, 'xrays_serial_evo_bg01.txt', 616, 2
;;;
;;; History - Adapted from debug_serial.pro 
;;;           Reads in modified xrays_serial_evo.txt files that now
;;;           include an identified xrayIndex for the 3 Xray energies
;;;           (O, Al, Mn)
;;;  
;;;           25 March 2022 - Now loops thru all the Xrays calculating
;;;                           scti, pcti after
;;;                           showing results for the selected one.
;;;
;;;          31 March 2022 - Updated to take into account the bg flux
;;;                          that gets added to the X-ray signal when
;;;                          binning into store if BG is not zero.  
  

    if n_params() lt 4 then begin
       print,'Please input the serial stats filename, the Y-binned location of the X-rays, the Xray identifier (0/1/2) and the diffuse BG value'
       print, "'debug_serial_multien, 'xrays_serial_evo_bg01.txt', 33, 0, 5"
       return
    endif
  
  readcol, input_serial_stats_file, storeTrN,  SerialTrN, X, Flux_iter, geo_frac, trap_index, capture_p, rv, traps_ltX, traps_free_ltX, traps_filled_ltX, losses, xrayx

  if xray_id eq 0 then detx = 600
  if xray_id eq 1 then detx = 1200
  if xray_id eq 2 then detx = 1800

  bg_binned_flux = bg_input * 5 ;;; (not 6 as the pixel with X-ray flux has only X-ray flux in it)
  if binnedy eq 16 then en_in = 150.0 + bg_binned_flux
  if binnedy eq 33 then en_in = 500.0 + bg_binned_flux
  if binnedy eq 66 then en_in = 1600.0 + bg_binned_flux
  if binnedy eq 333 then en_in = 500.0 + bg_binned_flux
  if binnedy eq 366 then en_in = 1600.0 + bg_binned_flux
  if binnedy eq 416 then en_in = 150.0 + bg_binned_flux
  if binnedy eq 550 then en_in = 1600.0 + bg_binned_flux
  if binnedy eq 583 then en_in = 150.0 + bg_binned_flux
  if binnedy eq 616 then en_in = 500.0 + bg_binned_flux

  binnedy_all = [16, 33, 66, 333, 366, 416, 550, 583, 616]
    
  ; Select data for the requested Xray
  
  xray1 = where(storeTrN eq binnedy and xrayx eq xray_id)

  SerialTrN20xray1 = SerialTrN[xray1]
  X20xray1 = X[xray1]
  Flux_iter20xray1 =Flux_iter[xray1]
  geo_frac20xray1 = geo_frac[xray1]
  trap_index20xray1 =trap_index[xray1]
  capture_p20xray1 =capture_p[xray1]
  rv20xray1 =rv[xray1]
  traps_ltX20xray1 =traps_ltX[xray1]
  traps_free_ltX20xray1 =traps_free_ltX[xray1]
  traps_filled_ltX20xray1 =traps_filled_ltX[xray1]
  losses20xray1 = losses[xray1]

  print, 'Start and End flux', Flux_iter20xray1[0], Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]
  cti_p = 1.0 - (Flux_iter20xray1[0]/en_in)^(1./(binnedy*6+719))
  cti_s = 1.0 - (Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]/Flux_iter20xray1[0])^(1./detx)
  print, 'CTIp, CTIs = ', cti_p, cti_s, format='(a, e12.5, e12.5)'
  plot, SerialTrN20xray1, Flux_iter20xray1, psym=1, yr = [Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]-10, Flux_iter20xray1[0]+10]

  stop
  t0 = where(trap_index20xray1 eq 0)
  SerialTrN20xray1t0 = SerialTrN20xray1[t0]
  geo_frac20xray1t0 = geo_frac20xray1[t0]
  capture_p20xray1_t0 = capture_p20xray1[t0]
  rv20xray1_t0 =  rv20xray1[t0]
  traps_ltX20xray1t0 =traps_ltX20xray1[t0]
  traps_free_ltX20xray1t0 =traps_free_ltX20xray1[t0]
  traps_filled_ltX20xray1t0 =traps_filled_ltX20xray1[t0]
  losses20xray1_t0 = (losses20xray1[t0])  
  losses20xray1_t0_tot = total(losses20xray1[t0])
  print, 'Trap0'
  print, 'Total traps, free', total(traps_ltX20xray1t0), total(traps_free_ltX20xray1t0)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray1t0/traps_ltX20xray1t0), max(traps_filled_ltX20xray1t0/traps_ltX20xray1t0), mean(traps_filled_ltX20xray1t0/traps_ltX20xray1t0)
  print, 'Total losses, mean Pc, expected losses', losses20xray1_t0_tot, mean(capture_p20xray1_t0), total(capture_p20xray1_t0*traps_free_ltX20xray1t0*geo_frac20xray1)



  t1 = where(trap_index20xray1 eq 1)
  SerialTrN20xray1t1 = SerialTrN20xray1[t1]
  geo_frac20xray1t1 = geo_frac20xray1[t1]
  capture_p20xray1_t1 = capture_p20xray1[t1]
  rv20xray1_t1 =  rv20xray1[t1]
  traps_ltX20xray1t1 =traps_ltX20xray1[t1]
  traps_free_ltX20xray1t1 =traps_free_ltX20xray1[t1]
  traps_free_ltX20xray1t1_tot = total(traps_free_ltX20xray1t1)
  traps_filled_ltX20xray1t1 =traps_filled_ltX20xray1[t1]
  losses20xray1_t1 = (losses20xray1[t1])    
  losses20xray1_t1_tot = total(losses20xray1[t1])
  print, 'Trap1'
  print, 'Total traps, free', total(traps_ltX20xray1t1), total(traps_free_ltX20xray1t1)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray1t1/traps_ltX20xray1t1), max(traps_filled_ltX20xray1t1/traps_ltX20xray1t1), mean(traps_filled_ltX20xray1t1/traps_ltX20xray1t1)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray1t1), traps_free_ltX20xray1t1_tot, traps_free_ltX20xray1t1_tot*mean(geo_frac20xray1t1)
  print, 'Total losses, mean Pc, expected losses', losses20xray1_t1_tot, mean(capture_p20xray1_t1), total(capture_p20xray1_t1*traps_free_ltX20xray1t1*geo_frac20xray1t1)

  t2 = where(trap_index20xray1 eq 2)
  SerialTrN20xray1t2 = SerialTrN20xray1[t2]
  geo_frac20xray1t2 = geo_frac20xray1[t2]
  capture_p20xray1_t2 = capture_p20xray1[t2]
  rv20xray1_t2 =  rv20xray1[t2]
  traps_ltX20xray1t2 =traps_ltX20xray1[t2]
  traps_free_ltX20xray1t2 =traps_free_ltX20xray1[t2]
  traps_free_ltX20xray1t2_tot = total(traps_free_ltX20xray1t2)
  traps_filled_ltX20xray1t2 =traps_filled_ltX20xray1[t2]
  losses20xray1_t2 = (losses20xray1[t2])    
  losses20xray1_t2_tot = total(losses20xray1[t2])
  print, 'Trap2'
  print, 'Total traps, free', total(traps_ltX20xray1t2), total(traps_free_ltX20xray1t2)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray1t2/traps_ltX20xray1t2), max(traps_filled_ltX20xray1t2/traps_ltX20xray1t2), mean(traps_filled_ltX20xray1t2/traps_ltX20xray1t2)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray1t2), traps_free_ltX20xray1t2_tot, traps_free_ltX20xray1t2_tot*mean(geo_frac20xray1t2)
  print, 'Total losses, mean Pc, expected losses', losses20xray1_t2_tot, mean(capture_p20xray1_t2), total(capture_p20xray1_t2*traps_free_ltX20xray1t2*geo_frac20xray1t2)


  t3 = where(trap_index20xray1 eq 3)
  SerialTrN20xray1t3 = SerialTrN20xray1[t3]
  geo_frac20xray1t3 = geo_frac20xray1[t3]
  capture_p20xray1_t3 = capture_p20xray1[t3]
  rv20xray1_t3 =  rv20xray1[t3]
  traps_ltX20xray1t3 =traps_ltX20xray1[t3]
  traps_free_ltX20xray1t3 =traps_free_ltX20xray1[t3]
  traps_free_ltX20xray1t3_tot = total(traps_free_ltX20xray1t3)
  traps_filled_ltX20xray1t3 =traps_filled_ltX20xray1[t3]
  losses20xray1_t3 = (losses20xray1[t3])    
  losses20xray1_t3_tot = total(losses20xray1[t3])
  print, 'Trap3'
  print, 'Total traps, free', total(traps_ltX20xray1t3), total(traps_free_ltX20xray1t3)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray1t3/traps_ltX20xray1t3), max(traps_filled_ltX20xray1t3/traps_ltX20xray1t3), mean(traps_filled_ltX20xray1t3/traps_ltX20xray1t3)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray1t3), traps_free_ltX20xray1t3_tot, traps_free_ltX20xray1t3_tot*mean(geo_frac20xray1t3)
  print, 'Total losses, mean Pc, expected losses', losses20xray1_t3_tot, mean(capture_p20xray1_t3), total(capture_p20xray1_t3*traps_free_ltX20xray1t3*geo_frac20xray1t3)



 
  stop
  openw, lu, 'cti_table.txt', /get_lun, width=350
  printf, lu, 'FluxOrig FluxSerialIn FluxNodeOut Ybinned X CTIp CTIs'
  for i = 0, n_elements(binnedy_all)-1 do begin
     for xray_id = 0, 2 do begin

        if xray_id eq 0 then detx = 600
        if xray_id eq 1 then detx = 1200
        if xray_id eq 2 then detx = 1800

        binnedy = binnedy_all[i]
        xray1 = where(storeTrN eq binnedy and xrayx eq xray_id, nx1)

        if nx1 gt 0 then begin
           if binnedy eq 16 then en_in = 150.0 + bg_binned_flux
           if binnedy eq 33 then en_in = 500.0 + bg_binned_flux
           if binnedy eq 66 then en_in = 1600.0 + bg_binned_flux
           if binnedy eq 333 then en_in = 500.0 + bg_binned_flux
           if binnedy eq 366 then en_in = 1600.0 + bg_binned_flux
           if binnedy eq 416 then en_in = 150.0 + bg_binned_flux
           if binnedy eq 550 then en_in = 1600.0 + bg_binned_flux
           if binnedy eq 583 then en_in = 150.0 + bg_binned_flux
           if binnedy eq 616 then en_in = 500.0 + bg_binned_flux
        
     
           SerialTrN20xray1 = SerialTrN[xray1]
           X20xray1 = X[xray1]
           Flux_iter20xray1 =Flux_iter[xray1]
           geo_frac20xray1 = geo_frac[xray1]
           trap_index20xray1 =trap_index[xray1]
           capture_p20xray1 =capture_p[xray1]
           rv20xray1 =rv[xray1]
           traps_ltX20xray1 =traps_ltX[xray1]
           traps_free_ltX20xray1 =traps_free_ltX[xray1]
           traps_filled_ltX20xray1 =traps_filled_ltX[xray1]
           losses20xray1 = losses[xray1]

           print, 'Start and End flux', Flux_iter20xray1[0], Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]
           cti_p = 1.0 - (Flux_iter20xray1[0]/en_in)^(1./(binnedy*6+719))
           cti_s = 1.0 - (Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]/Flux_iter20xray1[0])^(1./detx)
           print, 'Ybinned, X, en_in, CTIp, CTIs = ', binnedy, detx, en_in, cti_p, cti_s, format='(a, i, i, d6.1, e12.5, e12.5)'
           printf, lu, en_in, Flux_iter20xray1[0], Flux_iter20xray1[n_elements(Flux_iter20xray1)-1], binnedy, detx, cti_p, cti_s, format='(i, d8.1, d8.1, i, i, e14.5, e14.5)'
           plot, SerialTrN20xray1, Flux_iter20xray1, psym=1, yr = [Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]-10, Flux_iter20xray1[0]+10]
        endif
     endfor
  endfor

  free_lun, lu
      
  
  
end

