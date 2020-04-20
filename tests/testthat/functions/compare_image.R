# from tools package.. to extract file exntesion
file.ext = function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

# function to generate test paths
make_test_path <- function(func, id, type = 'png', create.dir = T){
  fname = paste(func,id,type, sep = '.')
  dirs = system.file('tests','testthat',c('reference','result'), package = "genoppi")
  dirs = file.path(dirs, func)
  if (create.dir){
    if (!dir.exists(dirs[[1]])) dir.create(dirs[[1]])
    if (!dir.exists(dirs[[2]])) dir.create(dirs[[2]])
  }
  paths = file.path(dirs, fname)
  return(list(ref=paths[1],res=paths[2]))
}

# save a reference
save_gg_reference <- function(plt, func, id, type = 'png', width = 3, height = 3, seed = 1){
  paths = make_test_path(func, id, type)
  catf(paste('[SAVE]',paths$ref))
  set.seed(seed)
  ggsave(filename = paths$ref, plt, width = width, height = height)
}


#
compare_with_reference <- function(plt, func, id, type = 'png', width = 3, height = 3, seed = 1){
  require(png)
  paths = make_test_path(func, id, type)
  if (file.exists(paths$res)) unlink(paths$res)
  set.seed(seed)
  ggsave(filename = paths$res, plt, width = width, height = height)
  return(test_png_identity(paths))
}

test_png_identity <- function(paths){
  require(png)
  if (!all(unlist(lapply(paths, file.exists)))) stop('either ref or res does not exist!')
  images = lapply(paths, function(x) as.matrix(readPNG(x)[,,1]))
  if (all(dim(images$res) != dim(images$ref))) stop('image dimensions are not the same!')
  return(all(images$ref == images$res))
}


compare_both <- function(func, id){
  require(png)
  paths = make_test_path(func, id, type)
  if (!all(unlist(lapply(paths, file.exists)))) stop('either ref or res does not exist!')
  images = lapply(paths, function(x) as.matrix(readPNG(x)[,,1]))
  if (all(dim(images$res) != dim(images$ref))) stop('image dimensions are not the same!')
  img.comp = abs(images$ref-images$res)
  img.comp = images$ref+8*(img.comp)
  img.comp = img.comp/max(img.comp)
  grid.raster(img.comp)
}



