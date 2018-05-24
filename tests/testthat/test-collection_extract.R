context("test-collection_extract.R")
library(sf)

pt <- st_point(c(1, 0))
ls <- st_linestring(matrix(c(4, 3, 0, 0), ncol = 2))
poly1 <- st_polygon(list(matrix(c(5.5, 7, 7, 6, 5.5, 0, 0, -0.5, -0.5, 0), ncol = 2)))
poly2 <- st_polygon(list(matrix(c(6.6, 8, 8, 7, 6.6, 1, 1, 1.5, 1.5, 1), ncol = 2)))
multipoly <- st_multipolygon(list(poly1, poly2))

i <- st_geometrycollection(list(pt, ls, poly1, poly2))

j <- st_geometrycollection(list(pt, ls, poly1, poly2, multipoly))

## A GEOMETRYCOLLECTION
aa <- rbind(st_sf(a=1, geom = st_sfc(i)),
            st_sf(a=2, geom = st_sfc(j)))

##
## A GEOMETRY of single types
bb <- rbind(
  st_sf(a = 1, geom = st_sfc(pt)),
  st_sf(a = 2, geom = st_sfc(ls)),
  st_sf(a = 3, geom = st_sfc(poly1)),
  st_sf(a = 4, geom = st_sfc(multipoly))
)

## A GEOMETRY of mixed single types and GEOMETRYCOLLECTIONS
cc <- rbind(aa, bb)

test_that("st_collection_extract2 works with sfg objects", {
  expect_is(st_collection_extract2(i, "POLYGON"), "MULTIPOLYGON")
  expect_is(st_collection_extract2(j, "POLYGON"), "MULTIPOLYGON")
  expect_is(st_collection_extract2(i, "POINT"), "MULTIPOINT")
  expect_is(st_collection_extract2(i, "LINESTRING"), "MULTILINESTRING")
})

test_that("st_collection_extract2 works with sfc objects", {
  expect_is(st_collection_extract2(st_geometry(aa), "POLYGON"), "sfc_MULTIPOLYGON")
  expect_is(st_collection_extract2(st_geometry(aa), "LINESTRING"), "sfc_MULTILINESTRING")
  expect_is(st_collection_extract2(st_geometry(aa), "POINT"), "sfc_MULTIPOINT")
  expect_is(st_collection_extract2(st_geometry(bb), "POINT"), "sfc_GEOMETRY")
  expect_is(st_collection_extract2(st_geometry(cc), "POLYGON"), "sfc_GEOMETRY")
})


test_that("st_collection_extract2 works with sf objects", {
  expect_is(st_geometry(st_collection_extract2(aa, "POLYGON")), "sfc_MULTIPOLYGON")
  expect_is(st_geometry(st_collection_extract2(aa, "LINESTRING")), "sfc_MULTILINESTRING")
  expect_is(st_geometry(st_collection_extract2(aa, "POINT")), "sfc_MULTIPOINT")
  expect_is(st_geometry(st_collection_extract2(bb, "POLYGON")), "sfc_GEOMETRY")
  expect_is(st_geometry(st_collection_extract2(cc, "POLYGON")), "sfc_GEOMETRY")
})

test_that("st_collection_extract2 behaves with unexpected inputs", {
  expect_is(st_collection_extract2(poly1, "POLYGON"), "POLYGON")
  expect_true(st_is_empty(st_collection_extract2(st_sfc(pt), "POLYGON")))
  expect_true(st_is_empty(
    st_collection_extract2(st_sf(a = "a", geom = st_sfc(pt)), "POLYGON")
  ))
  ## Returns empty geometry
  expect_true(all(st_is_empty(st_collection_extract2(st_sfc(pt, ls), "POLYGON"))))
  expect_true(all(st_is_empty(
    st_collection_extract2(st_sf(a = c("a", "b"), geom = st_sfc(ls, pt)),
                                       "POLYGON")
  )))
  expect_true(all(st_is_empty(
    empty <- st_collection_extract2(st_geometrycollection(list(pt, ls)), "POLYGON")
  )))
  expect_length(empty, 0L)
  expect_is(empty, "sfg")
  expect_true(all(st_is(empty, "MULTIPOLYGON")))
})