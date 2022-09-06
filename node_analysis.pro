pro node_analysis, nodefile, storefile
  ;
  ;+
  ; NAME: node_analysis.pro
  ;
  ; PURPOSE: Debug node flux values
  ;
  ; CATEGORY: CDM
  ;
  ; INPUTS:
  ; 
  ; nodefile = file with X, Y, Flux at output node
  ; storefile = file with Y, Flux at store/serial boundary.
  ;
  ; HISTORY - CP, 6 Jan 2022
  ;
  ; EXAMPLE
  ; node_analysis, 'node_flux_fr1.txt', ;store_flux_frn1.txt'

  readcol, nodefile, x, y, flux, format= '(i,i, d)'

  readcol, storefile, ystore, yflux, format='(i,d)'
  
  for i = 1, max(y) do begin
     print, 'Flux at store at coordinate ', i, yflux[i]
     node_index_i = where(y eq i)
     print, 'Flux at node at X=1 and mean', flux[node_index_i[0]], mean(flux[node_index_i[0]]
     plot, x[where(y eq i)], flux[where(y eq i)], psym=1
     wait,1
  endfor   

end
  
  
