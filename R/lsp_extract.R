ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))

lsp_extract = function(x, window, id){
  windows_sf = lsp_add_sf(x = x, window = window)
  windows_sf = windows_sf[windows_sf$id == id, ]
  x[windows_sf]
}

plot(lsp_extract(x = landcover, window = 100, id = 1895))
plot(lsp_extract(x = landcover, window = ecoregions["id"], id = 10))
