

st_collection_extract2 = function(x, type) {
  UseMethod("st_collection_extract2")
}

st_collection_extract2.sfc = function(x, type) {
  type = switch_type(type)
  st_sfc(CPL_collection_extract(x, type), crs = st_crs(x))
}

st_collection_extract2.sfg = function(x, type) {
  st_collection_extract2(st_geometry(x), type = type)[[1]]
}

st_collection_extract2.sf = function(x, type) {
  st_set_geometry(x, st_collection_extract2(st_geometry(x), type = type))
}

switch_type = function(type = c("POINT", "LINESTRING", "POLYGON")) {
  type = match.arg(type)
  switch(type, 
         "POINT" = 1, 
         "LINESTRING" = 2, 
         "POLYGON" = 3)
}
