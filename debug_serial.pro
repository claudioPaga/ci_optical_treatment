pro debug_serial, input_serial_stats_file, binnedy

;;; CP, 3 Feb 2022
;;;
;;; Summary - Inspect the output statistics from the processing of a
;;;           diffuse optical bg + CI lines for pixels with X-ray
;;;           events.
;;;           X-ray events were positioned at specific locations,
;;;           at binned line 33 (towards bottom of
;;;           image) and at binned line 616 (towards top of image)
;;;           Select either with the binnedy input parameter
;;;           
;;; Example - debug_serial, 'xrays_serial_evo_bg01.txt', 33
;;;           debug_serial, 'xrays_serial_evo_bg01.txt', 616
  


  

    if n_params() lt 2 then begin
       print,'Please input the serial stats filename and the Y-binned location of the X-rays (either 33 or 616):'
       print, "'debug_serial, 'xrays_serial_evo_bg01.txt', 33"
       return
    endif
  
  readcol, input_serial_stats_file, storeTrN,  SerialTrN, X, Flux_iter, geo_frac, trap_index, capture_p, rv, traps_ltX, traps_free_ltX, traps_filled_ltX, losses

  ;;; X-rays are introduced at binned line 33 (towards bottom of
  ;;; image) and at binned line 616 (towards top of image)
  ;;; Manually select one of the two
  ; Select Xrays in line 33 or 616
  
  y_binned_index = where(storeTrN eq binnedy)

  
  SerialTrN20 = SerialTrN[y_binned_index]
  X20 = X[y_binned_index]
  Flux_iter20 =Flux_iter[y_binned_index]
  geo_frac20 = geo_frac[y_binned_index]
  trap_index20 =trap_index[y_binned_index]
  capture_p20 =capture_p[y_binned_index]
  rv20 =rv[y_binned_index]
  traps_ltX20 =traps_ltX[y_binned_index]
  traps_free_ltX20 =traps_free_ltX[y_binned_index]
  traps_filled_ltX20 =traps_filled_ltX[y_binned_index]
  losses20 = losses[y_binned_index]

  ; Select the X-ray closest to the node.
  xray1 = where(x20 le 600 and SerialTrN20 le 600)

  SerialTrN20xray1 = SerialTrN20[xray1]
  X20xray1 = X20[xray1]
  Flux_iter20xray1 =Flux_iter20[xray1]
  geo_frac20xray1 = geo_frac20[xray1]
  trap_index20xray1 =trap_index20[xray1]
  capture_p20xray1 =capture_p20[xray1]
  rv20xray1 =rv20[xray1]
  traps_ltX20xray1 =traps_ltX20[xray1]
  traps_free_ltX20xray1 =traps_free_ltX20[xray1]
  traps_filled_ltX20xray1 =traps_filled_ltX20[xray1]
  losses20xray1 = losses20[xray1]

  print, 'Start and End flux', Flux_iter20xray1[0], Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]
  cti_p = 1.0 - (Flux_iter20xray1[0]/500.)^(1./binnedy)
  cti_s = 1.0 - (Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]/Flux_iter20xray1[0])^(1./600.)
  print, 'CTIp, CTIs = ', cti_p, cti_s
  stop
  plot, SerialTrN20xray1, Flux_iter20xray1, psym=1, yr = [Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]-10, Flux_iter20xray1[0]+10]
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



                                ; Select the X-ray close to the centre

  ;;; Find the indices
  ;;; It's complicated because after 600 transfers the 1st
  ;;; X-ray has completed its readout and it's not recorded
  ;;; anymore in the input file

  xray2 = [4, 5, 6, 7]  
  for c = 1, 600 do begin
     next = [c*12+4: c*12+7]
     xray2 = [xray2, next]
  endfor

  for c = 601, 1200 do begin
     next = [600*12 + (c-600)*8+4: 600*12 + (c-600)*8+7]
     xray2 = [xray2, next]
  endfor

  print, ''
  print, 'Central X-ray now'
  print, 'Central X-ray now'
  SerialTrN20xray2 = SerialTrN20[xray2]
  X20xray2 = X20[xray2]
  Flux_iter20xray2 =Flux_iter20[xray2]
  geo_frac20xray2 = geo_frac20[xray2]
  trap_index20xray2 =trap_index20[xray2]
  capture_p20xray2 =capture_p20[xray2]
  rv20xray2 =rv20[xray2]
  traps_ltX20xray2 =traps_ltX20[xray2]
  traps_free_ltX20xray2 =traps_free_ltX20[xray2]
  traps_filled_ltX20xray2 =traps_filled_ltX20[xray2]
  losses20xray2 = losses20[xray2]

  print, 'Central X-ray Start and End flux', Flux_iter20xray2[0], Flux_iter20xray2[n_elements(Flux_iter20xray2)-1]
  plot, SerialTrN20xray2, Flux_iter20xray2, psym=1, yr = [Flux_iter20xray2[n_elements(Flux_iter20xray2)-1]-10, Flux_iter20xray2[0]+10]

  cti_p = 1.0 - (Flux_iter20xray1[0]/500.)^(1./binnedy)
  cti_s = 1.0 - (Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]/Flux_iter20xray1[0])^(1./1200.)
  print, 'CTIp, CTIs = ', cti_p, cti_s

  t0 = where(trap_index20xray2 eq 0)
  SerialTrN20xray2t0 = SerialTrN20xray2[t0]
  geo_frac20xray2t0 = geo_frac20xray2[t0]
  capture_p20xray2_t0 = capture_p20xray2[t0]
  rv20xray2_t0 =  rv20xray2[t0]
  traps_ltX20xray2t0 =traps_ltX20xray2[t0]
  traps_free_ltX20xray2t0 =traps_free_ltX20xray2[t0]
  traps_filled_ltX20xray2t0 =traps_filled_ltX20xray2[t0]
  losses20xray2_t0 = (losses20xray2[t0])  
  losses20xray2_t0_tot = total(losses20xray2[t0])
  print, 'Trap0'
  print, 'Total traps, free', total(traps_ltX20xray2t0), total(traps_free_ltX20xray2t0)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray2t0/traps_ltX20xray2t0), max(traps_filled_ltX20xray2t0/traps_ltX20xray2t0), mean(traps_filled_ltX20xray2t0/traps_ltX20xray2t0)
  print, 'Total losses, mean Pc, expected losses', losses20xray2_t0_tot, mean(capture_p20xray2_t0), total(capture_p20xray2_t0*traps_free_ltX20xray2t0*geo_frac20xray2t0)

 
  t1 = where(trap_index20xray2 eq 1)
  SerialTrN20xray2t1 = SerialTrN20xray2[t1]
  geo_frac20xray2t1 = geo_frac20xray2[t1]
  capture_p20xray2_t1 = capture_p20xray2[t1]
  rv20xray2_t1 =  rv20xray2[t1]
  traps_ltX20xray2t1 =traps_ltX20xray2[t1]
  traps_free_ltX20xray2t1 =traps_free_ltX20xray2[t1]
  traps_free_ltX20xray2t1_tot = total(traps_free_ltX20xray2t1)
  traps_filled_ltX20xray2t1 =traps_filled_ltX20xray2[t1]
  losses20xray2_t1 = (losses20xray2[t1])    
  losses20xray2_t1_tot = total(losses20xray2[t1])
  print, 'Trap1'
  print, 'Total traps, free', total(traps_ltX20xray2t1), total(traps_free_ltX20xray2t1)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray2t1/traps_ltX20xray2t1), max(traps_filled_ltX20xray2t1/traps_ltX20xray2t1), mean(traps_filled_ltX20xray2t1/traps_ltX20xray2t1)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray2t1), traps_free_ltX20xray2t1_tot, traps_free_ltX20xray2t1_tot*mean(geo_frac20xray2t1)
  print, 'Total losses, mean Pc, expected losses', losses20xray2_t1_tot, mean(capture_p20xray2_t1), total(capture_p20xray2_t1*traps_free_ltX20xray2t1*geo_frac20xray2t1)

  t2 = where(trap_index20xray2 eq 2)
  SerialTrN20xray2t2 = SerialTrN20xray2[t2]
  geo_frac20xray2t2 = geo_frac20xray2[t2]
  capture_p20xray2_t2 = capture_p20xray2[t2]
  rv20xray2_t2 =  rv20xray2[t2]
  traps_ltX20xray2t2 =traps_ltX20xray2[t2]
  traps_free_ltX20xray2t2 =traps_free_ltX20xray2[t2]
  traps_free_ltX20xray2t2_tot = total(traps_free_ltX20xray2t2)
  traps_filled_ltX20xray2t2 =traps_filled_ltX20xray2[t2]
  losses20xray2_t2 = (losses20xray2[t2])    
  losses20xray2_t2_tot = total(losses20xray2[t2])
  print, 'Trap2'
  print, 'Total traps, free', total(traps_ltX20xray2t2), total(traps_free_ltX20xray2t2)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray2t2/traps_ltX20xray2t2), max(traps_filled_ltX20xray2t2/traps_ltX20xray2t2), mean(traps_filled_ltX20xray2t2/traps_ltX20xray2t2)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray2t2), traps_free_ltX20xray2t2_tot, traps_free_ltX20xray2t2_tot*mean(geo_frac20xray2t2)
  print, 'Total losses, mean Pc, expected losses', losses20xray2_t2_tot, mean(capture_p20xray2_t2), total(capture_p20xray2_t2*traps_free_ltX20xray2t2*geo_frac20xray2t2)


  t3 = where(trap_index20xray2 eq 3)
  SerialTrN20xray2t3 = SerialTrN20xray2[t3]
  geo_frac20xray2t3 = geo_frac20xray2[t3]
  capture_p20xray2_t3 = capture_p20xray2[t3]
  rv20xray2_t3 =  rv20xray2[t3]
  traps_ltX20xray2t3 =traps_ltX20xray2[t3]
  traps_free_ltX20xray2t3 =traps_free_ltX20xray2[t3]
  traps_free_ltX20xray2t3_tot = total(traps_free_ltX20xray2t3)
  traps_filled_ltX20xray2t3 =traps_filled_ltX20xray2[t3]
  losses20xray2_t3 = (losses20xray2[t3])    
  losses20xray2_t3_tot = total(losses20xray2[t3])
  print, 'Trap3'
  print, 'Total traps, free', total(traps_ltX20xray2t3), total(traps_free_ltX20xray2t3)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray2t3/traps_ltX20xray2t3), max(traps_filled_ltX20xray2t3/traps_ltX20xray2t3), mean(traps_filled_ltX20xray2t3/traps_ltX20xray2t3)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray2t3), traps_free_ltX20xray2t3_tot, traps_free_ltX20xray2t3_tot*mean(geo_frac20xray2t3)
  print, 'Total losses, mean Pc, expected losses', losses20xray2_t3_tot, mean(capture_p20xray2_t3), total(capture_p20xray2_t3*traps_free_ltX20xray2t3*geo_frac20xray2t3)

   ; Select the X-ray farthest from node

  ;;; Find the indices
  ;;; It's complicated because after 600 transfers the 1st
  ;;; X-ray has completed its readout and it's not recorded
  ;;; anymore in the input file

  xray3 = [8, 9, 10, 11]  
  for c = 1, 600 do begin
     next = [c*12+8: c*12+11]
     xray3 = [xray3, next]
  endfor

  for c = 601, 1200 do begin
     next = [600*12 + (c-600)*8+8: 600*12 + (c-600)*8+11]
     xray3 = [xray3, next]
  endfor

  ;The final ones are all the fartherst X-ray
  next = [600*12 + (600)*8+11 +1 : n_elements(X20)-1]
  xray3 = [xray3, next]

  print, ''
  print, 'Far X-ray now'
  print, 'Far X-ray now'
  SerialTrN20xray3 = SerialTrN20[xray3]
  X20xray3 = X20[xray3]
  Flux_iter20xray3 =Flux_iter20[xray3]
  geo_frac20xray3 = geo_frac20[xray3]
  trap_index20xray3 =trap_index20[xray3]
  capture_p20xray3 =capture_p20[xray3]
  rv20xray3 =rv20[xray3]
  traps_ltX20xray3 =traps_ltX20[xray3]
  traps_free_ltX20xray3 =traps_free_ltX20[xray3]
  traps_filled_ltX20xray3 =traps_filled_ltX20[xray3]
  losses20xray3 = losses20[xray3]

  print, 'Far X-ray Start and End flux', Flux_iter20xray3[0], Flux_iter20xray3[n_elements(Flux_iter20xray3)-1]
  plot, SerialTrN20xray3, Flux_iter20xray3, psym=1, yr = [Flux_iter20xray3[n_elements(Flux_iter20xray3)-1]-10, Flux_iter20xray3[0]+10]

  cti_p = 1.0 - (Flux_iter20xray1[0]/500.)^(1./binnedy)
  cti_s = 1.0 - (Flux_iter20xray1[n_elements(Flux_iter20xray1)-1]/Flux_iter20xray1[0])^(1./1800.)
  print, 'CTIp, CTIs = ', cti_p, cti_s
  
  t0 = where(trap_index20xray3 eq 0)
  SerialTrN20xray3t0 = SerialTrN20xray3[t0]
  geo_frac20xray3t0 = geo_frac20xray3[t0]
  capture_p20xray3_t0 = capture_p20xray3[t0]
  rv20xray3_t0 =  rv20xray3[t0]
  traps_ltX20xray3t0 =traps_ltX20xray3[t0]
  traps_free_ltX20xray3t0 =traps_free_ltX20xray3[t0]
  traps_filled_ltX20xray3t0 =traps_filled_ltX20xray3[t0]
  losses20xray3_t0 = (losses20xray3[t0])  
  losses20xray3_t0_tot = total(losses20xray3[t0])
  print, 'Trap0'
  print, 'Total traps, free', total(traps_ltX20xray3t0), total(traps_free_ltX20xray3t0)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray3t0/traps_ltX20xray3t0), max(traps_filled_ltX20xray3t0/traps_ltX20xray3t0), mean(traps_filled_ltX20xray3t0/traps_ltX20xray3t0)
  print, 'Total losses, mean Pc, expected losses', losses20xray3_t0_tot, mean(capture_p20xray3_t0), total(capture_p20xray3_t0*traps_free_ltX20xray3t0*geo_frac20xray3t0)

 
  t1 = where(trap_index20xray3 eq 1)
  SerialTrN20xray3t1 = SerialTrN20xray3[t1]
  geo_frac20xray3t1 = geo_frac20xray3[t1]
  capture_p20xray3_t1 = capture_p20xray3[t1]
  rv20xray3_t1 =  rv20xray3[t1]
  traps_ltX20xray3t1 =traps_ltX20xray3[t1]
  traps_free_ltX20xray3t1 =traps_free_ltX20xray3[t1]
  traps_free_ltX20xray3t1_tot = total(traps_free_ltX20xray3t1)
  traps_filled_ltX20xray3t1 =traps_filled_ltX20xray3[t1]
  losses20xray3_t1 = (losses20xray3[t1])    
  losses20xray3_t1_tot = total(losses20xray3[t1])
  print, 'Trap1'
  print, 'Total traps, free', total(traps_ltX20xray3t1), total(traps_free_ltX20xray3t1)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray3t1/traps_ltX20xray3t1), max(traps_filled_ltX20xray3t1/traps_ltX20xray3t1), mean(traps_filled_ltX20xray3t1/traps_ltX20xray3t1)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray3t1), traps_free_ltX20xray3t1_tot, traps_free_ltX20xray3t1_tot*mean(geo_frac20xray3t1)
  print, 'Total losses, mean Pc, expected losses', losses20xray3_t1_tot, mean(capture_p20xray3_t1), total(capture_p20xray3_t1*traps_free_ltX20xray3t1*geo_frac20xray3t1)

  t2 = where(trap_index20xray3 eq 2)
  SerialTrN20xray3t2 = SerialTrN20xray3[t2]
  geo_frac20xray3t2 = geo_frac20xray3[t2]
  capture_p20xray3_t2 = capture_p20xray3[t2]
  rv20xray3_t2 =  rv20xray3[t2]
  traps_ltX20xray3t2 =traps_ltX20xray3[t2]
  traps_free_ltX20xray3t2 =traps_free_ltX20xray3[t2]
  traps_free_ltX20xray3t2_tot = total(traps_free_ltX20xray3t2)
  traps_filled_ltX20xray3t2 =traps_filled_ltX20xray3[t2]
  losses20xray3_t2 = (losses20xray3[t2])    
  losses20xray3_t2_tot = total(losses20xray3[t2])
  print, 'Trap2'
  print, 'Total traps, free', total(traps_ltX20xray3t2), total(traps_free_ltX20xray3t2)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray3t2/traps_ltX20xray3t2), max(traps_filled_ltX20xray3t2/traps_ltX20xray3t2), mean(traps_filled_ltX20xray3t2/traps_ltX20xray3t2)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray3t2), traps_free_ltX20xray3t2_tot, traps_free_ltX20xray3t2_tot*mean(geo_frac20xray3t2)
  print, 'Total losses, mean Pc, expected losses', losses20xray3_t2_tot, mean(capture_p20xray3_t2), total(capture_p20xray3_t2*traps_free_ltX20xray3t2*geo_frac20xray3t2)


  t3 = where(trap_index20xray3 eq 3)
  SerialTrN20xray3t3 = SerialTrN20xray3[t3]
  geo_frac20xray3t3 = geo_frac20xray3[t3]
  capture_p20xray3_t3 = capture_p20xray3[t3]
  rv20xray3_t3 =  rv20xray3[t3]
  traps_ltX20xray3t3 =traps_ltX20xray3[t3]
  traps_free_ltX20xray3t3 =traps_free_ltX20xray3[t3]
  traps_free_ltX20xray3t3_tot = total(traps_free_ltX20xray3t3)
  traps_filled_ltX20xray3t3 =traps_filled_ltX20xray3[t3]
  losses20xray3_t3 = (losses20xray3[t3])    
  losses20xray3_t3_tot = total(losses20xray3[t3])
  print, 'Trap3'
  print, 'Total traps, free', total(traps_ltX20xray3t3), total(traps_free_ltX20xray3t3)
  print, 'Fraction of filled traps, min, max, mean', min(traps_filled_ltX20xray3t3/traps_ltX20xray3t3), max(traps_filled_ltX20xray3t3/traps_ltX20xray3t3), mean(traps_filled_ltX20xray3t3/traps_ltX20xray3t3)
  print, 'Total traps, total free traps, fraction free visible', total(traps_ltX20xray3t3), traps_free_ltX20xray3t3_tot, traps_free_ltX20xray3t3_tot*mean(geo_frac20xray3t3)
  print, 'Total losses, mean Pc, expected losses', losses20xray3_t3_tot, mean(capture_p20xray3_t3), total(capture_p20xray3_t3*traps_free_ltX20xray3t3*geo_frac20xray3t3)



  
  
  stop
end

