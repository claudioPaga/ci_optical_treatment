pro test_optical_pixel_proc, diffuse_flux, n_frames, flag_xrays = flag_xrays

  ;
  ;+
  ; NAME: test_optical_pixel_proc.pro
  ;
  ; PURPOSE: Simulates the effects of optical loading and CI lines in SXI images
  ;
  ; CATEGORY: CDM
  ;
  ; INPUTS:
  ; 
  ; diffuse_flux = Single flux value corresponding to the optical loading in the image area per pixel
  ;
  ; n_frames = number of consecutive
  ; frames with diffuse optical signal
  ; to model
  ;
  ; HISTORY - CP, 10 Dec 2021
  ;           Modified to allow CI lines
  ;           injection to fill lines in
  ;           following image frame
  ;
  ;           CP, 16 Dec 2021
  ;           Added flag_xrays flag to
  ;           include pixels with energy
  ;           in the X-ray energy band
  ;           
  tic
  restore, 'distort_cdm_properties.sav'
  print, 'trap density image = ', trap_species_image_density
  print, 'charge_injection_flag = ', charge_injection_flag
  print, 'capture_cross_section_image = ', capture_cross_section_image
  print, 'flag_unbinned = ', flag_unbinned
  print, 'Diffuse bg flux = ', diffuse_flux

  ;;; Record the evolution of filled traps for each species in columns and serial register
  ;;;openw, luimat1, 'ol_image_trap1_stats.txt', /get_lun, width=500
  ;;;openw, luimat2, 'ol_image_trap2_stats.txt', /get_lun, width=500
  ;;;openw, luimat3, 'ol_image_trap3_stats.txt', /get_lun, width=500
  ;;;openw, luimat4, 'ol_image_trap4_stats.txt', /get_lun, width=500
  
  ;;;openw, lustoret1, 'ol_store_trap1_stats.txt', /get_lun, width=500
  ;;;openw, lustoret2, 'ol_store_trap2_stats.txt', /get_lun, width=500
  ;;;openw, lustoret3, 'ol_store_trap3_stats.txt', /get_lun, width=500
  ;;;openw, lustoret4, 'ol_store_trap4_stats.txt', /get_lun, width=500

  ;;;openw, luserialt1, 'ol_serial_trap1_stats.txt', /get_lun, width=500
  ;;;openw, luserialt2, 'ol_serial_trap2_stats.txt', /get_lun, width=500
  ;;;openw, luserialt3, 'ol_serial_trap3_stats.txt', /get_lun, width=500
  ;;;openw, luserialt4, 'ol_serial_trap4_stats.txt', /get_lun, width=500
  
  t1_col_filling_evolution = dblarr(image_section_lines + store_section_lines, n_frames*2)
  t2_col_filling_evolution = dblarr(image_section_lines + store_section_lines, n_frames*2)
  t3_col_filling_evolution = dblarr(image_section_lines + store_section_lines, n_frames*2)
  t4_col_filling_evolution = dblarr(image_section_lines + store_section_lines, n_frames*2)
  t1_serial_filling_evolution = dblarr(serial_columns/readout_nodes, n_frames*2)
  t2_serial_filling_evolution = dblarr(serial_columns/readout_nodes, n_frames*2)
  t3_serial_filling_evolution = dblarr(serial_columns/readout_nodes, n_frames*2)
  t4_serial_filling_evolution = dblarr(serial_columns/readout_nodes, n_frames*2)


  ;;;Setup the input flux consisting of diffuse bg in the image (0-3718) and a
  ;;;few lines of CI every charge_injection_period_frame lines
  ;;;flux_line includes all the lines of both store + image at the end
  ;;;of the image readout (Phase A), so it has length of 719*6 + 3791
  
  flux_line = dblarr(store_section_lines*ccd_mode_binning + image_section_lines)
  flux_line[0:image_section_lines-1] = diffuse_flux
  if charge_injection_flag eq 1 then begin
    for j = image_section_lines, store_section_lines*ccd_mode_binning + image_section_lines, charge_injection_period_frame do begin
      flux_line[j] = charge_injection_electrons
    endfor
  endif 


  ;;; Add to the line pixels with Energies around 550, 1.8kev and
  ;;; 5.9KeV at Y = 100, 200, 400,
  ;;;           Y = 2000, 2200, 2500
  ;;;           Y = 3300, 3500, 3700
  if keyword_set(flag_xrays) then begin
     x = [100, 200, 400]
     flux_line[x] = [150.0, 500.0, 1600.0]
     x = [2000, 2200, 2500]
     flux_line[x] = [500.0, 1600.0, 150.0]
     x = [3300, 3500, 3700]
     flux_line[x] = [1600.0, 150.0, 500.0]
     xrays_dety_binned = [100, 200, 400, 2000, 2200, 2500, 3300, 3500, 3700]/6
  endif else xrays_dety_binned = [0]
  
     
  trap_species = trap_species_parallel
;;; Setup densities
  trap_density_column = dblarr(image_section_lines + store_section_lines, trap_species)
  trap_density_row = dblarr(serial_columns/readout_nodes, trap_species)
  trap_density_filled_column = dblarr(image_section_lines + store_section_lines, trap_species)
  trap_density_filled_row = dblarr(serial_columns/readout_nodes, trap_species)
  trap_density_free_column = dblarr(image_section_lines + store_section_lines, trap_species)
  trap_density_free_row = dblarr(serial_columns/readout_nodes, trap_species)

  for trap_index = 0, trap_species-1 do begin
     trap_density_column[*,trap_index] = trap_species_image_density[trap_index]
     trap_density_row[*,trap_index] = trap_species_image_density[trap_index]
     trap_density_filled_column[*,trap_index] = 0.0
     trap_density_filled_row[*,trap_index] = 0.0
  endfor
  trap_density_free_column  = trap_density_column - trap_density_filled_column
  trap_density_free_row  = trap_density_row - trap_density_filled_row
  
  frames_counter = 0
  plot, flux_line, psym=1
  print, flux_line[0:200]
  wait, 10
  trap_occupancy_ol = optical_loading_ci_pixel_process(flux_line, image_section_lines, store_section_lines, serial_columns, readout_nodes, readout_image_time, readout_serial_time, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section_image, charge_volume_coeff_image, fwc, overscan_cols, ima_expo_time, frames_counter, xrays_dety_binned, flag_retrapping = flag_retrapping, flag_same_pixel_release = flag_same_pixel_release)
 
  ;;; Sub very low values with zeros.
  low_occupancy_reset_index = where(trap_occupancy_ol lt 1.E-6, nlow)
  if nlow gt 0 then trap_occupancy_ol[low_occupancy_reset_index] = 0.0

  ;;; 3 - Repeat the call for a number of frames, ideally until an equilibrium is reached
  for frames_counter = 1, n_frames do begin
    ;;; Get trap occupancy stats after frame readout
    trap_density_filled_column = trap_occupancy_ol[0:image_section_lines + store_section_lines -1, *]
    trap_density_filled_row = trap_occupancy_ol[image_section_lines + store_section_lines: image_section_lines + store_section_lines + serial_columns/readout_nodes -1, *]
    trap_density_free_column = trap_density_column - trap_density_filled_column
    trap_density_free_row = trap_density_row - trap_density_filled_row

    print, 'Readout frame n. ', frames_counter
    print, 'Total filled traps in image+store column', total(trap_density_filled_column)
    print, 'Total filled traps in serial register', total(trap_density_filled_row)
    print, 'Total filled specie1 in image+store column & serial register', total(trap_density_filled_column[*,0]), total(trap_density_filled_row[*,0])
    print, 'Total filled specie2 in image+store column & serial register', total(trap_density_filled_column[*,1]), total(trap_density_filled_row[*,1])
    print, 'Total filled specie3 in image+store column & serial register', total(trap_density_filled_column[*,2]), total(trap_density_filled_row[*,2])
    print, 'Total filled specie4 in image+store column & serial register', total(trap_density_filled_column[*,3]), total(trap_density_filled_row[*,3])

    print, 'prepare for next frame...'''


    ;;; Setup updated flux, including contribution from CI lines from
    ;;; previous frame

    flux_line = dblarr(store_section_lines*ccd_mode_binning + image_section_lines)
    flux_previous_frame = trap_occupancy_ol[image_section_lines + store_section_lines + serial_columns/readout_nodes : image_section_lines + store_section_lines + serial_columns/readout_nodes + image_section_lines -1 , 0]
    flux_line[0:image_section_lines-1] = diffuse_flux + flux_previous_frame 
    ;;; CI lines (if CI on)
    if charge_injection_flag eq 1 then begin
      for j = image_section_lines, store_section_lines*ccd_mode_binning + image_section_lines, charge_injection_period_frame do begin
        flux_line[j] = charge_injection_electrons
      endfor
    endif
    ;;; Add Xrays if flag is ON
    if keyword_set(flag_xrays) then begin
      x = [100, 200, 400]
      flux_line[x] = [150.0, 500.0, 1600.0]
      x = [2000, 2200, 2500]
      flux_line[x] = [500.0, 1600.0, 150.0]
      x = [3300, 3500, 3700]
      flux_line[x] = [1600.0, 150.0, 500.0]
    endif
    
    ;;; Record trap stats
    t1_col_filling_evolution[*,frames_counter*2-1] = trap_density_filled_column[*,0]
    t2_col_filling_evolution[*,frames_counter*2-1] = trap_density_filled_column[*,1]
    t3_col_filling_evolution[*,frames_counter*2-1] = trap_density_filled_column[*,2]
    t4_col_filling_evolution[*,frames_counter*2-1] = trap_density_filled_column[*,3]

    t1_serial_filling_evolution[*,frames_counter*2-1] = trap_density_filled_row[*,0]
    t2_serial_filling_evolution[*,frames_counter*2-1] = trap_density_filled_row[*,1]
    t3_serial_filling_evolution[*,frames_counter*2-1] = trap_density_filled_row[*,2]
    t4_serial_filling_evolution[*,frames_counter*2-1] = trap_density_filled_row[*,3]

    ;;;Once reached the number of frames to process break out of the loop.
    ;;;Otherwise calculate the charge released befor the following frame, update occupancy and call the frame transfer proc.
    if frames_counter eq n_frames then break

    plot, flux_line, psym=1
    wait, 10
    ; Process readout of following frame
    trap_occupancy_ol = optical_loading_ci_pixel_process(flux_line, image_section_lines, store_section_lines, serial_columns, readout_nodes, readout_image_time, readout_serial_time, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section_image, charge_volume_coeff_image, fwc, overscan_cols, ima_expo_time, frames_counter, xrays_dety_binned, flag_retrapping = flag_retrapping, flag_same_pixel_release = flag_same_pixel_release)
    low_occupancy_reset_index = where(trap_occupancy_ol lt 1.E-6, nlow)
    if nlow gt 0 then trap_occupancy_ol[low_occupancy_reset_index] = 0.0
  endfor

  ;;; Write trap filling stats to file
  stringfmt = '('+strtrim(string(n_frames*2), 2)+'d8.5)'
 ; printf, luimat1, transpose(t1_col_filling_evolution[0:image_section_lines-1, *]), format=stringfmt
 ; printf, luimat2, transpose(t2_col_filling_evolution[0:image_section_lines-1, *]), format=stringfmt
 ; printf, luimat3, transpose(t3_col_filling_evolution[0:image_section_lines-1, *]), format=stringfmt
 ; printf, luimat4, transpose(t4_col_filling_evolution[0:image_section_lines-1, *]), format=stringfmt
 ; printf, lustoret1, transpose(t1_col_filling_evolution[image_section_lines:image_section_lines+store_section_lines-1, *]), format=stringfmt
 ; printf, lustoret2, transpose(t2_col_filling_evolution[image_section_lines:image_section_lines+store_section_lines-1, *]), format=stringfmt
 ; printf, lustoret3, transpose(t3_col_filling_evolution[image_section_lines:image_section_lines+store_section_lines-1, *]), format=stringfmt
 ; printf, lustoret4, transpose(t4_col_filling_evolution[image_section_lines:image_section_lines+store_section_lines-1, *]), format=stringfmt
 ; printf, luserialt1, transpose(t1_serial_filling_evolution), format=stringfmt
 ; printf, luserialt2, transpose(t2_serial_filling_evolution), format=stringfmt
 ; printf, luserialt3, transpose(t3_serial_filling_evolution), format=stringfmt
 ; printf, luserialt4, transpose(t4_serial_filling_evolution), format=stringfmt

 ; free_lun, luimat1
 ; free_lun, luimat2
 ; free_lun, luimat3
 ; free_lun, luimat4
 ; free_lun, lustoret1
 ; free_lun, lustoret2
 ; free_lun, lustoret3
 ; free_lun, lustoret4
 ; free_lun, luserialt1
 ; free_lun, luserialt2
 ; free_lun, luserialt3
 ; free_lun, luserialt4

  print, 'End of optical loading'

  toc
  stop
end


  
